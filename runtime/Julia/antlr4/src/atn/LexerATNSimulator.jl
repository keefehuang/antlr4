#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/

# When we hit an accept state in either the DFA || the ATN, we
#  have to notify the character stream to start buffering characters
#  via {@link IntStream#mark} && record the current state. The current sim state
#  includes the current index into the input, the current line,
#  && current character position in that line. Note that the Lexer is
#  tracking the starting line && characterization of the token. These
#  variables track the "state" of the simulator when it hits an accept state.
#
#  <p>We track these variables separately for the DFA && ATN simulation
#  because the DFA simulation often has to fail over to the ATN
#  simulation. If the ATN simulation fails, we need the DFA to fall
#  back to its previously accepted state, if any. If the ATN succeeds,
#  then the ATN does the accept && the DFA simulator that invoked it
#  can simply return the predicted token type.</p>
#/

# from antlr4.PredictionContext import PredictionContextCache, SingletonPredictionContext, PredictionContext
# from antlr4.InputStream import InputStream
# from antlr4.Token import Token
# from antlr4.atn.ATN import ATN
# from antlr4.atn.ATNConfig import LexerATNConfig
# from antlr4.atn.ATNSimulator import ATNSimulator
# from antlr4.atn.ATNConfigSet import ATNConfigSet, OrderedATNConfigSet
# from antlr4.atn.ATNState import RuleStopState, ATNState
# from antlr4.atn.LexerActionExecutor import LexerActionExecutor
# from antlr4.atn.Transition import Transition
# from antlr4.dfa.DFAState import DFAState
# from antlr4.error.Errors import LexerNoViableAltException, UnsupportedOperationException
using Logging

mutable struct SimState
    index::Int
    line::Int
    column::Int
    dfaState::nDFAState

    function SimState()
        new(-1, 0, -1, nothing)
    end
end

function reset!(s::SimState)
    s.index = -1
    s.line = 0
    s.column = -1
    s.dfaState = nothing
end


MIN_DFA_EDGE = 0
MAX_DFA_EDGE = 127 # forces unicode to stay in ATN


global debug = false
global dfa_debug = false
global debug_list_atn_decisions = false
global retry_debug = false
global ERROR = DFAState(stateNumber=0x7FFFFFFF, configs=ATNConfigSet())


mutable struct LexerATNSimulator <: ATNSimulator
    atn::ATN
    sharedContextCache::PredictionContextCache
    decisionToDFA::Vector{DFA}
    recog::AbstractLexer
    # The current token's starting index into the character stream.
    #  Shared across DFA to ATN simulation in case the ATN fails && the
    #  DFA did not have a previous accept state. In this case, we use the
    #  ATN-generated exception object.
    startIndex::Int
    # line number 1..n within the input#/
    line::Int
    column::Int
    mode::Int
    # Cache Lexer properties to avoid further imports
    DEFAULT_MODE::Int
    MAX_CHAR_VALUE::Int
    # Used during DFA/ATN exec to record the most recent accept configuration info
    prevAccept::SimState
    ERROR::Union{Exception,Nothing}

    function LexerATNSimulator(recog::AbstractRecognizer, atn::ATN, decisionToDFA::Vector, sharedContextCache::PredictionContextCache)
        new(atn, sharedContextCache, decisionToDFA, recog, -1, 1, 1, LEXER_DEFAULT_MODE, LEXER_DEFAULT_MODE, MAX_CHAR_VALUE, SimState(), nothing)
    end
end

function copyState(lexerATNSimulator::LexerATNSimulator, simulator::LexerATNSimulator)
    lexerATNSimulator.column = simulator.column
    lexerATNSimulator.line = simulator.line
    lexerATNSimulator.mode = simulator.mode
    lexerATNSimulator.startIndex = simulator.startIndex
end

function match(lexerATNSimulator::LexerATNSimulator, input::InputStream , mode::Int)
    lexerATNSimulator.mode = mode
    marker = mark(input)
    try
        lexerATNSimulator.startIndex = input._index
        reset!(lexerATNSimulator.prevAccept)
        dfa = lexerATNSimulator.decisionToDFA[mode]
        if isnothing(dfa.s0)
            return matchATN(lexerATNSimulator, input)
        else
            return execATN(lexerATNSimulator, input, dfa.s0)
        end
    finally
        release!(input, marker)
    end
end

function reset!(lexerATNSimulator::LexerATNSimulator)
    reset!(lexerATNSimulator.prevAccept)
    lexerATNSimulator.startIndex = -1
    lexerATNSimulator.line = 1
    lexerATNSimulator.column = 0
    lexerATNSimulator.mode = lexerATNSimulator.DEFAULT_MODE
end

function matchATN(lexerATNSimulator::LexerATNSimulator, input::InputStream)
    startState = lexerATNSimulator.atn.modeToStartState[lexerATNSimulator.mode]

    if debug
        @info "matchATN mode " * string(lexerATNSimulator.mode) * " start: " * string(startState)
    end

    old_mode = lexerATNSimulator.mode
    s0_closure = computeStartState(lexerATNSimulator, input, startState)
    suppressEdge = s0_closure.hasSemanticContext
    s0_closure.hasSemanticContext = false
    
    next = addDFAState(lexerATNSimulator, s0_closure)
    if !suppressEdge
        lexerATNSimulator.decisionToDFA[lexerATNSimulator.mode].s0 = next
    end
    predict = execATN(lexerATNSimulator, input, next)

    if debug
        @info "DFA after matchATN: " * string(toLexerString(lexerATNSimulator.decisionToDFA[old_mode]))
    end

    return predict
end

function execATN(lexerATNSimulator::LexerATNSimulator, input::InputStream, ds0::DFAState)
    if debug
        println("start state closure=" * string(ds0.configs))
    end
    if ds0.isAcceptState
        # allow zero-length tokens
        captureSimState(lexerATNSimulator, input, ds0)
    end
    t = LA(input, 1)
    
    s = ds0 # s is current/from DFA state
    while true # while more work
        if debug
            println("execATN loop starting closure:" * string(s.configs))
        end
        # As we move src->trg, src->trg, we keep track of the previous trg to
        # avoid looking up the DFA state again, which is expensive.
        # If the previous target was already part of the DFA, we might
        # be able to avoid doing a reach operation upon t. If s!=null,
        # it means that semantic predicates didn't prevent us from
        # creating a DFA state. Once we know s!=null, we check to see if
        # the DFA state has an edge already for t. If so, we can just reuse
        # it's configuration set; there's no point in re-computing it.
        # This is kind of like doing DFA simulation within the ATN
        # simulation because DFA simulation is really just a way to avoid
        # computing reach/closure sets. Technically, once we know that
        # we have a previously added DFA state, we could jump over to
        # the DFA simulator. But, that would mean popping back && forth
        # a lot && making things more complicated algorithmically.
        # This optimization makes a lot of sense for loops within DFA.
        # A character will take us back to an existing DFA state
        # that already has lots of edges out of it. e.g., .* in comments.
        # println("Target for:" * string(s) + " and:" * string(t))
        target = getExistingTargetState(lexerATNSimulator, s, t)

        if isnothing(target)
            target = computeTargetState(lexerATNSimulator, input, s, t)
        end

        if target == ERROR
            break
        end

        # If this is a consumable input element, make sure to consume before
        # capturing the accept state so the input index, line, && char
        # position accurately reflect the state of the interpreter at the
        # end of the token.
        if t != TOKEN_EOF
            consume(lexerATNSimulator, input)
        end

        if target.isAcceptState
            captureSimState(lexerATNSimulator, input, target)
            if t == TOKEN_EOF
                break
            end
        end
        t = LA(input, 1)

        s = target # flip; current DFA target becomes new src/from state
    end
    return failOrAccept(lexerATNSimulator, lexerATNSimulator.prevAccept, input, s.configs, t)
end

# Get an existing target state for an edge in the DFA. If the target state
# for the edge has not yet been computed || is otherwise not available,
# this method returns {@code null}.
#
# @param s The current DFA state
# @param t The next input symbol
# @return The existing target DFA state for the given input symbol
# {@code t}, || {@code null} if the target state for this edge is not
# already cached
function getExistingTargetState(lexerATNSimulator::LexerATNSimulator, s::DFAState, t::Int)
    if isempty(s.edges) || t < MIN_DFA_EDGE || t > MAX_DFA_EDGE
        return nothing
    end
    target = s.edges[t-MIN_DFA_EDGE]
    if debug && !isnothing(target)
        println("reuse state " * string(s.stateNumber) * " edge to " * string(target.stateNumber))
    end

    return target
end

# Compute a target state for an edge in the DFA, && attempt to add the
# computed state && corresponding edge to the DFA.
#
# @param input The input stream
# @param s The current DFA state
# @param t The next input symbol
#
# @return The computed target DFA state for the given input symbol
# {@code t}. If {@code t} does not lead to a valid DFA state, this method
# returns {@link #ERROR}.
function computeTargetState(lexerATNSimulator::LexerATNSimulator, input::InputStream, s::DFAState, t::Int)
    reach = ATNConfigSet()

    # if we don't find an existing DFA state
    # Fill reach starting from closure, following t transitions
    getReachableConfigSet(lexerATNSimulator, input, s.configs, reach, t)
    if length(reach)==0 # we got nowhere on t from s
        if !reach.hasSemanticContext
            # we got nowhere on t, don't throw out this knowledge; it'd
            # cause a failover from DFA later.
            addDFAEdge(lexerATNSimulator, s, t; to=ERROR)
        end

        # stop when we can't match any more char
        return ERROR
    end
    # Add an edge from s to target DFA found/created for reach
    return addDFAEdge(lexerATNSimulator, s, t; cfgs=reach)
end

function failOrAccept(lexerATNSimulator::LexerATNSimulator, prevAccept::SimState , input::InputStream, reach::ATNConfigSet, t::Int)
    if !isnothing(lexerATNSimulator.prevAccept.dfaState)
        lexerActionExecutor = prevAccept.dfaState.lexerActionExecutor
        accept(lexerATNSimulator, input, lexerActionExecutor, lexerATNSimulator.startIndex, prevAccept.index, prevAccept.line, prevAccept.column)
        return prevAccept.dfaState.prediction
    else
        # if no accept && EOF is first char, return EOF
        if t == TOKEN_EOF && input._index == lexerATNSimulator.startIndex
            return TOKEN_EOF
        end
        throw(LexerNoViableAltException(lexerATNSimulator.recog, input, lexerATNSimulator.startIndex, reach))
    end
end

# Given a starting configuration set, figure out all ATN configurations
#  we can reach upon input {@code t}. Parameter {@code reach} is a return
#  parameter.
function getReachableConfigSet(lexerATNSimulator::LexerATNSimulator, input::InputStream, closures::ATNConfigSet, reach::ATNConfigSet, t::Int)
    # this is used to skip processing for configs which have a lower priority
    # than a config that already reached an accept state for the same rule
    skipAlt = ATNENUM.INVALID_ALT_NUMBER
    for cfg in closures
        currentAltReachedAcceptState = ( cfg.alt == skipAlt )
        if currentAltReachedAcceptState && cfg.passedThroughNonGreedyDecision
            continue
        end

        if debug
            @info "testing" * getTokenName(lexerATNSimulator, t) * "at" * string(cfg)
        end

        for trans in cfg.state.transitions          # for each transition
            target = getReachableTarget(lexerATNSimulator, trans, t)
            if !isnothing(target)
                lexerActionExecutor = cfg.lexerActionExecutor
                if !isnothing(lexerActionExecutor)
                    lexerActionExecutor = fixOffsetBeforeMatch!(lexerActionExecutor, input._index - lexerATNSimulator.startIndex)
                end
                treatEofAsEpsilon = (t == TOKEN_EOF)
                config = LexerATNConfig(target; lexerActionExecutor=lexerActionExecutor, config=cfg)
                if closure(lexerATNSimulator, input, config, reach, currentAltReachedAcceptState, true, treatEofAsEpsilon)
                    # any remaining configs for this alt have a lower priority than
                    # the one that just reached an accept state.
                    skipAlt = cfg.alt
                end
            end
        end
    end
end

function accept(lexerATNSimulator::LexerATNSimulator, input::InputStream, lexerActionExecutor::nLexerActionExecutor, startIndex::Int, index::Int, line::Int, charPos::Int)
    if debug
        println("ACTION" * string(lexerActionExecutor))
    end

    # seek to after last char in token
    seek!(input, index)
    lexerATNSimulator.line = line
    lexerATNSimulator.column = charPos

    if !isnothing(lexerActionExecutor) && !isnothing(lexerATNSimulator)
        execute(lexerActionExecutor, lexerATNSimulator.recog, input, startIndex)
    end
end

function getReachableTarget(lexerATNSimulator::LexerATNSimulator, trans::Transition, t::Int)
    if matches(trans, t, 0, lexerATNSimulator.MAX_CHAR_VALUE)
        return trans.target
    else
        return nothing
    end
end

function computeStartState(lexerATNSimulator::LexerATNSimulator, input::InputStream, p::ATNState)
    initialContext = PREDICTION_CONTEXT_EMPTY
    configs = ATNConfigSet()
    for i in 1:length(p.transitions)
        target = p.transitions[i].target
        c = LexerATNConfig(target; alt=i, context=initialContext)
        closure(lexerATNSimulator, input, c, configs, false, false, false)
    end
    return configs
end

# Since the alternatives within any lexer decision are ordered by
# preference, this method stops pursuing the closure as soon as an accept
# state is reached. After the first accept state is reached by depth-first
# search from {@code config}, all other (potentially reachable) states for
# this rule would have a lower priority.
#
# @return {@code true} if an accept state is reached, otherwise
# {@code false}.
function closure(lexerATNSimulator::LexerATNSimulator, input::InputStream, config::LexerATNConfig, configs::ATNConfigSet, currentAltReachedAcceptState::Bool,
            speculative::Bool, treatEofAsEpsilon::Bool)
    if debug
        println("closure(" * string(config) * ")")
    end

    if config.state isa RuleStopState
        if debug
            if !isnothing(lexerATNSimulator.recog)
                println("closure at" * string(lexerATNSimulator.recog.symbolicNames[config.state.ruleIndex]) * "rule stop" * string(config))
            else
                println("closure at rule stop", string(config))
            end
        end

        if isnothing(config.context) || hasEmptyPath(config.context)
            if isnothing(config.context) || isempty(config.context)
                add!(configs, config)
                return true
            else
                add!(configs, LexerATNConfig(config.state; config=config, context=PredictionContext.EMPTY))
                currentAltReachedAcceptState = true
            end
        end

        if !isnothing(config.context) && !isempty(config.context)
            for i in 1:length(config.context)
                if getReturnState(config.context, i) != EMPTY_RETURN_STATE
                    newContext = getParent(config.context, i) # "pop" return state
                    returnState = lexerATNSimulator.atn.states[getReturnState(config.context, i)+1]
                    c = LexerATNConfig(returnState; config=config, context=newContext)
                    currentAltReachedAcceptState = closure(lexerATNSimulator, input, c, configs,
                                currentAltReachedAcceptState, speculative, treatEofAsEpsilon)
                end
            end
        end
        return currentAltReachedAcceptState
    end
    # optimization
    if !config.state.epsilonOnlyTransitions
        if !currentAltReachedAcceptState || !config.passedThroughNonGreedyDecision
            add!(configs, config)
        end
    end
    for t in config.state.transitions
        c = getEpsilonTarget(lexerATNSimulator, input, config, t, configs, speculative, treatEofAsEpsilon)
        if !isnothing(c)
            currentAltReachedAcceptState = closure(lexerATNSimulator, input, c, configs, currentAltReachedAcceptState, speculative, treatEofAsEpsilon)
        end
    end

    return currentAltReachedAcceptState
end

# side-effect: can alter configs.hasSemanticContext
function getEpsilonTarget(lexerATNSimulator::LexerATNSimulator, input::InputStream, config::LexerATNConfig, t::Transition, configs::ATNConfigSet,
                                        speculative::Bool, treatEofAsEpsilon::Bool)
    c = nothing
    if t.serializationType==TransitionConstant.RULE
        newContext =    create(config.context, t.followState.stateNumber)
        c = LexerATNConfig(t.target; config=config, context=newContext)
    elseif t.serializationType==TransitionConstant.PRECEDENCE
        throw(UnsupportedOperationException("Precedence predicates are not supported in lexers."))

    elseif t.serializationType==TransitionConstant.PREDICATE
        #  Track traversing semantic predicates. If we traverse,
        # we cannot add a DFA state for this "reach" computation
        # because the DFA would not test the predicate again in the
        # future. Rather than creating collections of semantic predicates
        # like v3 && testing them on prediction, v4 will test them on the
        # fly all the time using the ATN not the DFA. This is slower but
        # semantically it's not used that often. One of the key elements to
        # this predicate mechanism is not adding DFA states that see
        # predicates immediately afterwards in the ATN. For example,addTransition

        # a : ID {p1}? | ID {p2}? ;

        # should create the start state for rule 'a' (to save start state
        # competition), but should not create target of ID state. The
        # collection of ATN states the following ID references includes
        # states reached by traversing predicates. Since this is when we
        # test them, we cannot cash the DFA state target of ID.
        
        if debug
            println("EVAL rule "* string(t.ruleIndex) * ":" * string(t.predIndex))
        end
        configs.hasSemanticContext = true
        if lexerATNSimulator.evaluatePredicate(input, t.ruleIndex, t.predIndex, speculative)
            c = LexerATNConfig(t.target; config=config)
        end

    elseif t.serializationType==TransitionConstant.ACTION
        if isnothing(config.context) || hasEmptyPath(config.context)
            # execute actions anywhere in the start rule for a token.
            #
            # TODO: if the entry rule is invoked recursively, some
            # actions may be executed during the recursive call. The
            # problem can appear when hasEmptyPath() is true but
            # isEmpty() is false. In this case, the config needs to be
            # split into two contexts - one with just the empty path
            # && another with everything but the empty path.
            # Unfortunately, the current algorithm does not allow
            # getEpsilonTarget to return two configurations, so
            # additional modifications are needed before we can support
            # the split operation.
            if isnothing(config.lexerActionExecutor)
                config.lexerActionExecutor = LexerActionExecutor(LexerAction[lexerATNSimulator.atn.lexerActions[t.actionIndex+1]])
            else
                push!(config.lexerActionExecutor, lexerATNSimulator.atn.lexerActions[t.actionIndex+1])
            end
            c = LexerATNConfig(t.target; config=config, lexerActionExecutor=config.lexerActionExecutor)

        else
            # ignore actions in referenced rules
            c = LexerATNConfig(t.target; config=config)
        end

    elseif t.serializationType==TransitionConstant.EPSILON
        c = LexerATNConfig(t.target; config=config)

    elseif t.serializationType in [ TransitionConstant.ATOM, TransitionConstant.RANGE, TransitionConstant.SET ]
        if treatEofAsEpsilon
            if matches(t, TOKEN_EOF, 0, MAX_CHAR_VALUE)
                c = LexerATNConfig(t.target; config=config)
            end
        end
    end

    return c
end

# Evaluate a predicate specified in the lexer.
#
# <p>If {@code speculative} is {@code true}, this method was called before
# {@link #consume} for the matched character. This method should call
# {@link #consume} before evaluating the predicate to ensure position
# sensitive values, including {@link Lexer#getText}, {@link Lexer#getLine},
# && {@link Lexer#getcolumn}, properly reflect the current
# lexer state. This method should restore {@code input} && the simulator
# to the original state before returning (i.e. undo the actions made by the
# call to {@link #consume}.</p>
#
# @param input The input stream.
# @param ruleIndex The rule containing the predicate.
# @param predIndex The index of the predicate within the rule.
# @param speculative {@code true} if the current index in {@code input} is
# one character before the predicate's location.
#
# @return {@code true} if the specified predicate evaluates to
# {@code true}.
#/
function evaluatePredicate(lexerATNSimulator::LexerATNSimulator, input::InputStream, ruleIndex::Int, predIndex::Int, speculative::Bool)
    # assume true if no recognizer was provided
    if isnothing(lexerATNSimulator.recog)
        return true
    end

    if not speculative
        return sempred(lexerATNSimulator.recog, nothing, ruleIndex, predIndex)
    end

    savedcolumn = lexerATNSimulator.column
    savedLine = lexerATNSimulator.line
    index = input._index
    marker = mark(input)
    try
        consume(lexerATNSimulator, input)
        return sempred(lexerATNSimulator.recog, nothing, ruleIndex, predIndex)
    finally
        lexerATNSimulator.column = savedcolumn
        lexerATNSimulator.line = savedLine
        input.seek(index)
        input.release(marker)
    end
end

function captureSimState(lexerATNSimulator::LexerATNSimulator, input::InputStream, dfaState::DFAState)
    lexerATNSimulator.prevAccept.index = input._index
    lexerATNSimulator.prevAccept.line = lexerATNSimulator.line
    lexerATNSimulator.prevAccept.column = lexerATNSimulator.column
    lexerATNSimulator.prevAccept.dfaState = dfaState
end

function addDFAEdge(lexerATNSimulator::LexerATNSimulator, from_::DFAState, tk::Int; to::nDFAState=nothing, cfgs::nATNConfigSet=nothing)
    
    if isnothing(to) && !isnothing(cfgs)
        # leading to this call, ATNConfigSet.hasSemanticContext is used as a
        # marker indicating dynamic predicate evaluation makes this edge
        # dependent on the specific input sequence, so the static edge in the
        # DFA should be omitted. The target DFAState is still created since
        # execATN has the ability to resynchronize with the DFA state cache
        # following the predicate evaluation step.
        #
        # TJP notes: next time through the DFA, we see a pred again && eval.
        # If that gets us to a previously created (but dangling) DFA
        # state, we can continue in pure DFA mode from there.
        #/
        suppressEdge = cfgs.hasSemanticContext
        cfgs.hasSemanticContext = false   


        to = addDFAState(lexerATNSimulator, cfgs)

        if suppressEdge
            return to
        end
    end

    # add the edge
    if tk < MIN_DFA_EDGE || tk > MAX_DFA_EDGE
        # Only track edges within the DFA bounds
        return to
    end
    if debug
        println("EDGE " * string(from_) * " -> " * string(to) * " upon " * Char(tk))
    end

    if isempty(from_.edges)
        #  make room for tokens 1..n && -1 masquerading as index 0
        from_.edges = Vector{nDFAState}(nothing, MAX_DFA_EDGE - MIN_DFA_EDGE * 1)
    end
    from_.edges[tk - MIN_DFA_EDGE] = to # connect

    return to
end

# Add a new DFA state if there isn't one with this set of
# configurations already. This method also detects the first
# configuration containing an ATN rule stop state. Later, when
# traversing the DFA, we will know which rule to accept.
function addDFAState(lexerATNSimulator::LexerATNSimulator, configs::ATNConfigSet)
    proposed = DFAState(configs=configs)
    index = findfirst(x->(x.state isa RuleStopState), configs.configs)

    if !isnothing(index)
        firstConfigWithRuleStopState = get(configs.configs, index, nothing)
    else
        firstConfigWithRuleStopState = nothing
    end


    if !isnothing(firstConfigWithRuleStopState)
        stopstate = firstConfigWithRuleStopState.state
        proposed.isAcceptState = true
        proposed.lexerActionExecutor = firstConfigWithRuleStopState.lexerActionExecutor
        proposed.prediction = lexerATNSimulator.atn.ruleToTokenType[firstConfigWithRuleStopState.state.ruleIndex+1]
    end

    dfa = lexerATNSimulator.decisionToDFA[lexerATNSimulator.mode]
    existing = get(dfa._states, proposed, nothing)
    if !isnothing(existing)
        return existing
    end

    newState = proposed

    newState.stateNumber = length(dfa._states)
    setReadonly!(configs, true)
    newState.configs = configs
    dfa._states[newState] = newState
    return newState
end

function getDFA(lexerATNSimulator::LexerATNSimulator, mode::Int)
    return lexerATNSimulator.decisionToDFA[mode]
end

# Get the text matched so far for the current token.
function getText(lexerATNSimulator::LexerATNSimulator, input::InputStream)
    # index is first lookahead char, don't include.
    return getText(input, lexerATNSimulator.startIndex, input._index)
end

function consume(lexerATNSimulator::LexerATNSimulator, input::InputStream)
    curChar = LA(input, 1)
    if curChar==ord('\n')
        lexerATNSimulator.line += 1
        lexerATNSimulator.column = 0
    else
        lexerATNSimulator.column += 1
    end
    consume(input)
end

function getTokenName(lexerATNSimulator::LexerATNSimulator, t::Int)
    if t==-1
        return "EOF"
    else
        return "'" * Char(t) * "'"
    end
end



