# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/
# from uuid import UUID
# from io import StringIO
# from typing import Callable
# from antlr4.Token import Token
# from antlr4.atn.ATN import ATN
# from antlr4.atn.ATNType import ATNType
# from antlr4.atn.ATNState import *
# from antlr4.atn.Transition import *
# from antlr4.atn.LexerAction import *
# from antlr4.atn.ATNDeserializationOptions import ATNDeserializationOptions

using UUIDs

# This is the earliest supported serialized UUID.
BASE_SERIALIZED_UUID = UUID("AADB8D7E-AEEF-4415-AD2B-8204D6CF042E")

# This UUID indicates the serialized ATN contains two sets of
# IntervalSets, where the second set's values are encoded as
# 32-bit integers to support the full Unicode SMP range up to U+10FFFF.
ADDED_UNICODE_SMP = UUID("59627784-3BE5-417A-B9EB-8131A7286089")

# This list contains all of the currently supported UUIDs, ordered by when
# the feature first appeared in this branch.
SUPPORTED_UUIDS = [ BASE_SERIALIZED_UUID, ADDED_UNICODE_SMP ]

SERIALIZED_VERSION = 3

# This is the current serialized UUID.
SERIALIZED_UUID = ADDED_UNICODE_SMP

mutable struct ATNDeserializer
    deserializationOptions::ATNDeserializationOptions
    data::Vector{Int}
    pos::Int
    uuid::UUID
    function ATNDeserializer(options::ATNDeserializationOptions=ATNDeserializationOptions())
        new(options)
    end
end

function readInt!(atnd::ATNDeserializer)
    i = atnd.data[atnd.pos]
    atnd.pos += 1
    return i
end

function readInt32!(atnd::ATNDeserializer)
    low = readInt!(atnd)
    high = readInt!(atnd)
    return low | (high << 16)
end

function readLong!(atnd::ATNDeserializer)
    low = readInt32!(atnd)
    high = readInt32!(atnd)
    return (low & 0x00000000FFFFFFFF) | (high << 32)
end

function readUUID!(atnd::ATNDeserializer)
    low = UInt128(readLong!(atnd))
    high = UInt128(readLong!(atnd))
    allBits = (low & 0xFFFFFFFFFFFFFFFF) | (high << 64)
    return UUID(allBits)
end


# Determines if a particular serialized representation of an ATN supports
# a particular feature, identified by the {@link UUID} used for serializing
# the ATN at the time the feature was first introduced.
#
# @param feature The {@link UUID} marking the first time the feature was
# supported in the serialized ATN.
# @param actualUuid The {@link UUID} of the actual serialized ATN which is
# currently being deserialized.
# @return {@code true} if the {@code actualUuid} value represents a
# serialized ATN at || after the feature identified by {@code feature} was
# introduced; otherwise, {@code false}.

function isFeatureSupported(::ATNDeserializer, feature::UUID , actualUuid::UUID )
    idx1 = findfirst(x->x==feature, SUPPORTED_UUIDS)
    if isnothing(idx1)
        return false
    end
    idx2 = findfirst(x->x==actualUuid, SUPPORTED_UUIDS)
    if isnothing(idx2)
        return false
    end
    return idx2 >= idx1
end

function deserialize(atnd::ATNDeserializer, data::String)
    reset!(atnd::ATNDeserializer, data)
    checkVersion(atnd)
    checkUUID!(atnd)
    atn = readATN(atnd)
    readStates!(atnd, atn)
    readRules!(atnd, atn)
    readModes!(atnd, atn)
    sets = IntervalSet[]
    # First, read all sets with 16-bit Unicode code points <= U+FFFF.
    readSets!(atnd, atn, sets, readInt!)
    # Next, if the ATN was serialized with the Unicode SMP feature,
    # deserialize sets with 32-bit arguments <= U+10FFFF.
    if isFeatureSupported(atnd::ATNDeserializer, ADDED_UNICODE_SMP, atnd.uuid)
        readSets!(atnd, atn, sets, readInt32!)
    end
    readEdges!(atnd::ATNDeserializer, atn, sets)
    readDecisions!(atnd::ATNDeserializer, atn)
    readLexerActions!(atnd::ATNDeserializer, atn)
    markPrecedenceDecisions(atnd::ATNDeserializer, atn)
    verifyATN(atnd::ATNDeserializer, atn)

    if atnd.deserializationOptions.generateRuleBypassTransitions &&
            atn.grammarType == ATNType.PARSER
        generateRuleBypassTransitions(atnd::ATNDeserializer, atn)
        # re-verify after modification
        verifyATN(atnd::ATNDeserializer,atn)
    end
    return atn
end

function reset!(atnd::ATNDeserializer, data::String)
    function adjust(c::Char)
        v = ord(c)
        return v>1 ? v-2 : v+65533
    end
    temp = [ adjust(c) for c in data ]
    # don't adjust the first value since that's the version number
    temp[1] = ord(data[1])

    # don't adjust the first value since that's the version number
    atnd.data = temp
    atnd.pos = 1
end


function checkVersion(atnd::ATNDeserializer)
    version = readInt!(atnd)
    if version != SERIALIZED_VERSION
        error("Could not deserialize ATN with version " * string(version) * " (expected " * string(SERIALIZED_VERSION) * ").")
    end
end

function checkUUID!(atnd::ATNDeserializer)
    uuid = readUUID!(atnd)
    if !(uuid in SUPPORTED_UUIDS)
        error("Could not deserialize ATN with UUID: " * string(uuid) *
                        " (expected " * string(SERIALIZED_UUID) * " || a legacy UUID).")
    end
    atnd.uuid = uuid
end

function readStates!(atnd::ATNDeserializer, atn::AbstractATN)
    loopBackStateNumbers = []
    endStateNumbers = []
    nstates = readInt!(atnd)
    a = []
    b = []
    for i in 1:nstates
        stype = readInt!(atnd)
        # ignore bad type of states
        if stype==Int(ATNStateType.INVALID_TYPE)
            addState!(atn, nothing)
            continue
        end
        ruleIndex = readInt!(atnd)

        push!(a, stype)
        push!(b, ruleIndex)

        if ruleIndex == 0xFFFF
            ruleIndex = length(stateFactories)
        end
        
        s = stateFactory(atnd, stype, ruleIndex)
        if stype == Int(ATNStateType.LOOP_END) # special case
            loopBackStateNumber = readInt!(atnd)
            push!(loopBackStateNumbers, (s, loopBackStateNumber))
        elseif s isa BlockStartState
            endStateNumber = readInt!(atnd)
            push!(endStateNumbers, (s, endStateNumber))
        end
        addState!(atn, s)
    end
    # delay the assignment of loop back and end states until we know all the state instances have been initialized
    for pair in loopBackStateNumbers
        pair[1].loopBackState = atn.states[pair[2]+1]
    end

    for pair in endStateNumbers
        pair[1].endState = atn.states[pair[2]+1]
    end

    numNonGreedyStates = readInt!(atnd)
    for i in 1:numNonGreedyStates
        stateNumber = readInt!(atnd)
        atn.states[stateNumber+1].nonGreedy = true
    end

    numPrecedenceStates = readInt!(atnd)
    for i in 1:numPrecedenceStates
        stateNumber = readInt!(atnd)
        atn.states[stateNumber+1].isPrecedenceRule = true
    end

end

function readRules!(atnd::ATNDeserializer, atn::AbstractATN)
    nrules = readInt!(atnd)
    if atn.grammarType == ATNType.LEXER
        atn.ruleToTokenType = zeros(Int, nrules)
    end

    atn.ruleToStartState = Vector{RuleStartState}(undef, nrules)
    for i in 1:nrules
        s = readInt!(atnd)
        startState = atn.states[s+1]
        atn.ruleToStartState[i] = startState
        if atn.grammarType == ATNType.LEXER
            tokenType = readInt!(atnd)
            if tokenType == 0xFFFF
                tokenType = TOKEN_EOF
            end
            atn.ruleToTokenType[i] = tokenType
        end
    end
    atn.ruleToStopState = Vector{RuleStopState}(undef, nrules)
    for state in atn.states
        if !(state isa RuleStopState)
            continue
        end
        atn.ruleToStopState[state.ruleIndex+1] = state
        atn.ruleToStartState[state.ruleIndex+1].stopState = state
    end
end

function readModes!(atnd::ATNDeserializer, atn::AbstractATN)
    nmodes = readInt!(atnd)
    for i in 1:nmodes
        s = readInt!(atnd)
        push!(atn.modeToStartState, atn.states[s+1])
    end
end

function readSets!(atnd::ATNDeserializer, atn::AbstractATN, sets::Vector{IntervalSet}, readUnicode::Function)
    m = readInt!(atnd)
    for i in 1:m
        iset = IntervalSet()
        push!(sets, iset)
        n = readInt!(atnd)
        containsEof = readInt!(atnd)
        if containsEof != 0
            addOne(iset, -1)
        end
        for j in 1:n
            i1 = readUnicode(atnd)
            i2 = readUnicode(atnd)
            addRange!(iset, i1:i2) # range upper limit is exclusive
        end
    end
end

function readEdges!(atnd::ATNDeserializer, atn::AbstractATN, sets::Vector{IntervalSet})
    nedges = readInt!(atnd)
    for i in 1:nedges
        src = readInt!(atnd)
        trg = readInt!(atnd)
        ttype = readInt!(atnd)
        arg1 = readInt!(atnd)
        arg2 = readInt!(atnd)
        arg3 = readInt!(atnd)
        trans = edgeFactory(atnd, atn, ttype, src, trg, arg1, arg2, arg3, sets)
        addTransition!(atn.states[src+1], trans)
    end
    
    # edges for rule stop states can be derived, so they aren't serialized
    for state in atn.states
        for i in 1:length(state.transitions)
            t = state.transitions[i]
            if !(t isa RuleTransition)
                continue
            end
            outermostPrecedenceReturn = -1
            if atn.ruleToStartState[t.target.ruleIndex+1].isPrecedenceRule
                if t.precedence == 0
                    outermostPrecedenceReturn = t.target.ruleIndex
                end
            end
            trans = EpsilonTransition(t.followState, outermostPrecedenceReturn)
            addTransition!(atn.ruleToStopState[t.target.ruleIndex+1], trans)
        end
    end

    for state in atn.states
        if state isa BlockStartState
            # we need to know the end state to set its start state
            if isnothing(state.endState)
                error("IllegalState")
            end
            # block end states can only be associated to a single block start state
            if !isnothing(state.endState.startState)
                error("IllegalState")
            end
            state.endState.startState = state
        end

        if state isa PlusLoopbackState
            for i in 1:length(state.transitions)
                target = state.transitions[i].target
                if target isa PlusBlockStartState
                    target.loopBackState = state
                end
            end
        elseif state isa StarLoopbackState
            for i in 1:length(state.transitions)
                target = state.transitions[i].target
                if target isa StarLoopEntryState
                    target.loopBackState = state
                end
            end
        end
    end
end

function readDecisions!(atnd::ATNDeserializer, atn::AbstractATN)
    ndecisions = readInt!(atnd)
    for i in 1:ndecisions
        s = readInt!(atnd)
        decState = atn.states[s+1]
        push!(atn.decisionToState, decState)
        decState.decision = i-1
    end
end

function readLexerActions!(atnd::ATNDeserializer, atn::AbstractATN)
    if atn.grammarType == ATNType.LEXER
        count = readInt!(atnd)
        atn.lexerActions = Vector{LexerAction}(undef, count)
        for i in 1:count
            actionType = readInt!(atnd)
            data1 = readInt!(atnd)
            if data1 == 0xFFFF
                data1 = -1
            end
            data2 = readInt!(atnd)
            if data2 == 0xFFFF
                data2 = -1
            end
            lexerAction = lexerActionFactory(atnd, actionType, data1, data2)
            atn.lexerActions[i] = lexerAction
        end
    end
end

function generateRuleBypassTransitions(atnd::ATNDeserializer, atn::AbstractATN)

    count = length(atn.ruleToStartState)
    atn.ruleToTokenType = Vector{Int}(0, count)
    for i in 1:count
        atn.ruleToTokenType[i] = atn.maxTokenType + i + 1
    end
    for i in 1:count
        generateRuleBypassTransition(atnd, atn, i)
    end
end

function generateRuleBypassTransition(atnd::ATNDeserializer, atn::AbstractATN, idx::Int)
    bypassStart = BasicBlockStartState()
    bypassStart.ruleIndex = idx
    addState!(atn, bypassStart)

    bypassStop = BlockEndState()
    bypassStop.ruleIndex = idx
    addState!(atn, bypassStop)

    bypassStart.endState = bypassStop
    defineDecisionState(atn, bypassStart)

    bypassStop.startState = bypassStart

    excludeTransition = nothing

    if atn.ruleToStartState[idx].isPrecedenceRule
        # wrap from the beginning of the rule to the StarLoopEntryState
        endState = nothing
        for state in atn.states
            if stateIsEndStateFor(atnd, state, idx)
                endState = state
                excludeTransition = state.loopBackState.transitions[1]
                break
            end
        end

        if isnothing(excludeTransition)
            error("Couldn't identify final state of the precedence rule prefix section.")
        end
    else
        endState = atn.ruleToStopState[idx+1]
    end

    # all non-excluded transitions that currently target end state need to target blockEnd instead
    for state in atn.states
        for transition in state.transitions
            if transition == excludeTransition
                continue
            end
            if transition.target == endState
                transition.target = bypassStop
            end
        end 
    end
    # all transitions leaving the rule start state need to leave blockStart instead
    ruleToStartState = atn.ruleToStartState[idx+1]
    count = length(ruleToStartState.transitions)
    while count > 0
        addTransition!(bypassStart, ruleToStartState.transitions[count])
        pop!(ruleToStartState.transitions)
    end

    # link the new states
    addTransition!(atn.ruleToStartState[idx+1], EpsilonTransition(bypassStart))
    addTransition(bypassStop, EpsilonTransition(endState))

    matchState = BasicState()
    addState!(atn, matchState)
    addTransition!(matchState, AtomTransition(bypassStop, atn.ruleToTokenType[idx]))
    addTransition!(bypassStart, EpsilonTransition(matchState))
end


function stateIsEndStateFor(atnd::ATNDeserializer, state::ATNState, idx::Int)
    if state.ruleIndex != idx
        return nothing
    end
    if !(state isa StarLoopEntryState)
        return nothing
    end

    maybeLoopEndState = state.transitions[length(state.transitions) - 1].target
    if !(maybeLoopEndState isa LoopEndState)
        return nothing
    end

    if maybeLoopEndState.epsilonOnlyTransitions &&
            (maybeLoopEndState.transitions[1].target isa RuleStopState)
        return state
    else
        return nothing
    end
end


#
# Analyze the {@link StarLoopEntryState} states in the specified ATN to set
# the {@link StarLoopEntryState#isPrecedenceDecision} field to the
# correct value.
#
# @param atn The ATN.
#
function markPrecedenceDecisions(atnd::ATNDeserializer, atn::AbstractATN)
    for state in atn.states
        if !(state isa StarLoopEntryState)
            continue
        end

        # We analyze the ATN to determine if this ATN decision state is the
        # decision for the closure block that determines whether a
        # precedence rule should continue || complete.
        #
        if atn.ruleToStartState[state.ruleIndex+1].isPrecedenceRule
            maybeLoopEndState = state.transitions[length(state.transitions) - 1].target
            if maybeLoopEndState isa LoopEndState
                if maybeLoopEndState.epsilonOnlyTransitions &&
                        maybeLoopEndState.transitions[1].target isa RuleStopState
                    state.isPrecedenceDecision = True
                end
            end
        end
    end
end

function verifyATN(atnd::ATNDeserializer, atn::AbstractATN)
    if !atnd.deserializationOptions.verifyATN
        return
    end
    # verify assumptions
    for state in atn.states
        if isnothing(state)
            continue
        end

        checkCondition(atnd, state.epsilonOnlyTransitions || length(state.transitions) <= 1)

        if state isa PlusBlockStartState
            checkCondition(atnd, !isnothing(state.loopBackState))
        end

        if state isa StarLoopEntryState
            checkCondition(atnd, !isnothing(state.loopBackState))
            checkCondition(atnd, length(state.transitions) == 2)

            if state.transitions[1].target isa StarBlockStartState
                checkCondition(atnd, (state.transitions[2].target isa LoopEndState))
                checkCondition(atnd, !state.nonGreedy)
            elseif state.transitions[1].target isa LoopEndState
                checkCondition(atnd, state.transitions[2].target isa StarBlockStartState)
                checkCondition(atnd, state.nonGreedy)
            else
                error("IllegalState")
            end
        end

        if state isa StarLoopbackState
            checkCondition(atnd, length(state.transitions) == 1)
            checkCondition(atnd, state.transitions[1].target isa StarLoopEntryState)
        end

        if state isa LoopEndState
            checkCondition(atnd, !isnothing(state.loopBackState))
        end

        if state isa RuleStartState
            checkCondition(atnd, !isnothing(state.stopState))
        end

        if state isa BlockStartState
            checkCondition(atnd, !isnothing(state.endState))
        end

        if state isa BlockEndState
            checkCondition(atnd, !isnothing(state.startState))
        end

        if state isa DecisionState
            checkCondition(atnd, length(state.transitions) <= 1 || state.decision >= 0)
        else
            checkCondition(atnd, length(state.transitions) <= 1 || state isa RuleStopState)
        end
    end
end

function checkCondition(atnd::ATNDeserializer, condition::Bool, message::String="")    
    if !condition
        if isempty(message)
            message = "IllegalState"
        end
        error(message)
    end
end


edgeFactories = Function[ (args...)->nothing,
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->EpsilonTransition(target),
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->
                    arg3 != 0 ? RangeTransition(target, Token.EOF, arg2) : RangeTransition(target, arg1, arg2),
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->
                    RuleTransition(atn.states[arg1+1], arg2, arg3, target),
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->
                    PredicateTransition(target, arg1, arg2, arg3 != 0),
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->
                    arg3 != 0 ? AtomTransition(target, TOKEN_EOF) : AtomTransition(target, arg1),
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->
                    ActionTransition(target, arg1, arg2, arg3 != 0),
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->
                    SetTransition(target, sets[arg1+1]),
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->
                    NotSetTransition(target, sets[arg1+1]),
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->
                    WildcardTransition(target),
                    (atn, src, trg, arg1, arg2, arg3, sets, target)->
                    PrecedencePredicateTransition(target, arg1)
                    ]
                    

function edgeFactory(atnd::ATNDeserializer, atn::AbstractATN, type::Int, src::Int, trg::Int, arg1::Int, arg2::Int, arg3::Int, sets::Vector{IntervalSet})
    target = atn.states[trg+1]
    if type > length(edgeFactories) || isnothing(edgeFactories[type+1])
        error("The specified transition type: " * string(type) + " is not valid.")
    else
        return edgeFactories[type+1](atn, src, trg, arg1, arg2, arg3, sets, target)
    end
end
stateFactories = Function[  () -> nothing,
                    () -> BasicState(),
                    () -> RuleStartState(),
                    () -> BasicBlockStartState(),
                    () -> PlusBlockStartState(),
                    () -> StarBlockStartState(),
                    () -> TokensStartState(),
                    () -> RuleStopState(),
                    () -> BlockEndState(),
                    () -> StarLoopbackState(),
                    () -> StarLoopEntryState(),
                    () -> PlusLoopbackState(),
                    () -> LoopEndState()
                ]

function stateFactory(atnd::ATNDeserializer, type::Int, ruleIndex::Int)
    if type> length(stateFactories) || isnothing(stateFactories[type+1])
        error("The specified state type " * string(type) * " is not valid.")
    else
        s = stateFactories[type+1]()
        if !isnothing(s)
            s.ruleIndex = ruleIndex
        end
    end
    return s
end
@se ATNDACTION begin
    CHANNEL = 0     #The type of a {@link LexerChannelAction} action.
    CUSTOM = 1      #The type of a {@link LexerCustomAction} action.
    MODE = 2        #The type of a {@link LexerModeAction} action.
    MORE = 3        #The type of a {@link LexerMoreAction} action.
    POP_MODE = 4    #The type of a {@link LexerPopModeAction} action.
    PUSH_MODE = 5   #The type of a {@link LexerPushModeAction} action.
    SKIP = 6        #The type of a {@link LexerSkipAction} action.
    TYPE = 7        #The type of a {@link LexerTypeAction} action.
end

actionFactories = Function[ (data1, data2)-> LexerChannelAction(data1),
                    (data1, data2)-> LexerCustomAction(data1, data2),
                    (data1, data2)-> LexerModeAction(data1),
                    (data1, data2)-> LEXER_MORE_ACTION,
                    (data1, data2)-> LEXER_POPMODE_ACTION,
                    (data1, data2)-> LexerPushModeAction(data1),
                    (data1, data2)-> LEXER_SKIP_ACTION,
                    (data1, data2)-> LexerTypeAction(data1)
                    ]

function lexerActionFactory(atnd::ATNDeserializer, type::Int, data1::Int, data2::Int)
    if type > length(actionFactories) || isnothing(actionFactories[type+1])
        error("The specified lexer action type " * string(type) * " is not valid.")
    else
        return actionFactories[type+1](data1, data2)
    end
end
