#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
# import sys
# if sys.version_info[1] > 5:
#     from typing import TextIO
# else:
#     from typing.io import TextIO
# from antlr4.BufferedTokenStream import TokenStream
# from antlr4.CommonTokenFactory import TokenFactory
# from antlr4.error.ErrorStrategy import DefaultErrorStrategy
# from antlr4.InputStream import InputStream
# from antlr4.Recognizer import Recognizer
# from antlr4.RuleContext import RuleContext
# from antlr4.ParserRuleContext import ParserRuleContext
# from antlr4.Token import Token
# from antlr4.Lexer import Lexer
# from antlr4.atn.ATNDeserializer import ATNDeserializer
# from antlr4.atn.ATNDeserializationOptions import ATNDeserializationOptions
# from antlr4.error.Errors import UnsupportedOperationException, RecognitionException
# from antlr4.tree.ParseTreePatternMatcher import ParseTreePatternMatcher
# from antlr4.tree.Tree import ParseTreeListener, TerminalNode, ErrorNode

struct TraceListener{T} <: ParseTreeListener where {T<:AbstractParser}
    _parser::AbstractParser

    function TraceListener(parser::T) where {T<:AbstractParser}
        new{T}(parser)
    end
end

function enterEveryRule(tl::TraceListener, ctx)
    open(parser._parseListeners._output, "w") do f
        write(f, "enter   " * tl._parser.ruleNames[ctx.getRuleIndex()] * ", LT(1)=" * tl._parser._input.LT(1).text)
    end
end

function visitTerminal(tl::TraceListener, node)
    open(parser._parseListeners._output, "w") do f
        write(f, "consume " * string(node.symbol) * " rule " * tl._parser.ruleNames[parser._parser._ctx.getRuleIndex()])
    end
end

function visitErrorNode(::TraceListener, node) end


function exitEveryRule(tl::TraceListener, ctx)
    open(parser._parseListeners._output, "w") do f
        write(f, "exit    " * parser._parser.ruleNames[ctx.getRuleIndex()] * ", LT(1)=" * tl._parser._input.LT(1).text)
    end
end

# parser field maps from the serialized ATN string to the deserialized {@link ATN} with
# bypass alternatives.
#
# @see ATNDeserializationOptions#isGenerateRuleBypassTransitions()
#
parserBypassAltsAtnCache = Dict()


# parser is all the parsing support code essentially; most of it is error recovery stuff.#
mutable struct Parser <: AbstractParser
    _recog::Recognizer
     # The input stream
    _input::nTokenStream
    _output::IOBuffer
    # The error handling strategy for the parser. The default value is a new
    # instance of {@link DefaultErrorStrategy}.
    _errHandler::DefaultErrorStrategy
    _precedenceStack::Vector{Int}
    # The {@link ParserRuleContext} object for the currently executing rule.
    _ctx::nParserRuleContext
    # Specifies whether or not the parser should construct a parse tree during
    # the parsing process. The default value is {@code true}.
    buildParseTrees::Bool
    # When {@link #setTrace}{@code (true)} is called, a reference to the
    # {@link TraceListener} is stored here so it can be easily removed in a
    # later call to {@link #setTrace}{@code (false)}. The listener itparser is
    # implemented as a parser listener so parser field is not directly used by
    # other parser methods.
    _tracer::Nothing
    _parseListeners::Vector{AbstractListener}
    # The number of syntax errors reported during parsing. parser value is
    # incremented each time {@link #notifyErrorListeners} is called.
    _syntaxErrors::Int

    function Parser(input::TokenStream, output::IOBuffer=IOBuffer()) 
        parser = new(Recognizer(), input, output, DefaultErrorStrategy(), [0], nothing, true, nothing, [], 0)
        setTokenStream(parser, input)
        return parser
    end
end

# reset the parser's state#
function reset!(parser::Parser)
    if !isnothing(parser._input)
        seek!(parser._input, 0)
    end
    reset!(parser._errHandler, parser)
    parser._ctx = nothing
    parser._syntaxErrors = 0
    setTrace(parser, false)
    parser._precedenceStack = Int[]
    push!(parser._precedenceStack, 0)
    if !isnothing(parser._interp)
        reset!(parser._interp)
    end
end

# Match current input symbol against {@code ttype}. If the symbol type
# matches, {@link ANTLRErrorStrategy#reportMatch} and {@link #consume} are
# called to complete the match process.
#
# <p>If the symbol type does not match,
# {@link ANTLRErrorStrategy#recoverInline} is called on the current error
# strategy to attempt recovery. If {@link #getBuildParseTree} is
# {@code true} and the token index of the symbol returned by
# {@link ANTLRErrorStrategy#recoverInline} is -1, the symbol is added to
# the parse tree by calling {@link ParserRuleContext#addErrorNode}.</p>
#
# @param ttype the token type to match
# @return the matched symbol
# @throws RecognitionException if the current input symbol did not match
# {@code ttype} and the error strategy could not recover from the
# mismatched symbol

function Base.match(parser::T, ttype::Int) where {T<:AbstractParser}
    t = getCurrentToken(parser)
    if t.type==ttype
        reportMatch(parser._errHandler, parser)
        consume(parser)
    else
        t = recoverInline(parser._errHandler, parser)
        if parser.buildParseTrees && t.tokenIndex==-1
            # we must have conjured up a new token during single token insertion
            # if it's not the current symbol
            addErrorNode!(parser._ctx, t)
        end
    end
    return t
end

function Base.match(parser::T, ttype::SuperEnum.Enum) where {T<:AbstractParser}
    Base.match(parser, Int(ttype))
end

# Match current input symbol as a wildcard. If the symbol type matches
# (i.e. has a value greater than 0), {@link ANTLRErrorStrategy#reportMatch}
# and {@link #consume} are called to complete the match process.
#
# <p>If the symbol type does not match,
# {@link ANTLRErrorStrategy#recoverInline} is called on the current error
# strategy to attempt recovery. If {@link #getBuildParseTree} is
# {@code true} and the token index of the symbol returned by
# {@link ANTLRErrorStrategy#recoverInline} is -1, the symbol is added to
# the parse tree by calling {@link ParserRuleContext#addErrorNode}.</p>
#
# @return the matched symbol
# @throws RecognitionException if the current input symbol did not match
# a wildcard and the error strategy could not recover from the mismatched
# symbol

function matchWildcard(parser::T) where {T<:AbstractParser}
    t = getCurrentToken(parser)
    if t.type > 0
        reportMatch(parser._errHandler, parser)
        consume(parser._errHandler)
    else
        t = recoverInline(parser._errHandler, parser)
        if parser.buildParseTrees && t.tokenIndex == -1
            # we must have conjured up a new token during single token insertion
            # if it's not the current symbol
            addErrorNode!(parser._ctx, t)
        end
    end
    return t
end

function getParseListeners(parser::T) where {T<:AbstractParser}
    return isnothing(parser._parseListeners) ? [] : parser._parseListeners
end

# Registers {@code listener} to receive events during the parsing process.
#
# <p>To support output-preserving grammar transformations (including but not
# limited to left-recursion removal, automated left-factoring, and
# optimized code generation), calls to listener methods during the parse
# may differ substantially from calls made by
# {@link ParseTreeWalker#DEFAULT} used after the parse is complete. In
# particular, rule entry and exit events may occur in a different order
# during the parse than after the parser. In addition, calls to certain
# rule entry methods may be omitted.</p>
#
# <p>With the following specific exceptions, calls to listener events are
# <em>deterministic</em>, i.e. for identical input the calls to listener
# methods will be the same.</p>
#
# <ul>
# <li>Alterations to the grammar used to generate code may change the
# behavior of the listener calls.</li>
# <li>Alterations to the command line options passed to ANTLR 4 when
# generating the parser may change the behavior of the listener calls.</li>
# <li>Changing the version of the ANTLR Tool used to generate the parser
# may change the behavior of the listener calls.</li>
# </ul>
#
# @param listener the listener to add
#
# @throws NullPointerException if {@code} listener is {@code null}
#
function addParseListener(parser::T, listener::ParseTreeListener) where {T<:AbstractParser}
    if isnothing(listener)
        throw(ReferenceError("listener"))
    end
    if isnothing(parser._parseListeners)
        parser._parseListeners = []
    end
    push!(parser._parseListeners, listener)
end

#
# Remove {@code listener} from the list of parse listeners.
#
# <p>If {@code listener} is {@code null} or has not been added as a parse
# listener, parser method does nothing.</p>
# @param listener the listener to remove
#
function removeParseListener(parser::T, listener::ParseTreeListener) where {T<:AbstractParser}
    remove(parser._parseListeners, listener)
end

function removeParseListener(::T, ::Nothing) where {T<:AbstractParser} end

# Remove all parse listeners.
function removeParseListeners(parser::T) where {T<:AbstractParser}
    parser._parseListeners = nothing
end

# Notify any parse listeners of an enter rule event.
function triggerEnterRuleEvent(parser::T) where {T<:AbstractParser}
    if !isnothing(parser._parseListeners)
        for listener in parser._parseListeners
            enterEveryRule(listener, parser._ctx)
            enterRule(parser._ctx, listener)
        end
    end
end


#
# Notify any parse listeners of an exit rule event.
#
# @see #addParseListener
#
function triggerExitRuleEvent(parser::T) where {T<:AbstractParser}
    if !isnothing(parser._parseListeners)
        # reverse order walk of listeners
        for listener in reverse(parser._parseListeners)
            exitRule(parser._ctx, listener)
            exitEveryRule(listener, parser._ctx)
        end
    end
end


# Gets the number of syntax errors reported during parsing. This value is
# incremented each time {@link #notifyErrorListeners} is called.
#
# @see #notifyErrorListeners
#
function getNumberOfSyntaxErrors(parser::T) where {T<:AbstractParser}
    return parser._syntaxErrors
end

function getTokenFactory(parser::T) where {T<:AbstractParser}
    return parser._input.tokenSource._factory
end

# Tell our token source and error strategy about a new way to create tokens.#
function setTokenFactory(parser::T, factory::TokenFactory) where {T<:AbstractParser}
    parser._input.tokenSource._factory = factory
end

# The ATN with bypass alternatives is expensive to create so we create it
# lazily.
#
# @throws UnsupportedOperationException if the current parser does not
# implement the {@link #getSerializedATN()} method.
#
function getATNWithBypassAlts(parser::T) where {T<:AbstractParser}
    serializedAtn = getSerializedATN(parser)
    if isnothing(serializedAtn)
        throw(UnsupportedOperationException("The current parser does not support an ATN with bypass alternatives."))
    end
    result = get(parser.bypassAltsAtnCache, serializedAtn, nothing)
    if isnothing(result)
        deserializationOptions = ATNDeserializationOptions()
        deserializationOptions.generateRuleBypassTransitions = true
        result = deserialize(ATNDeserializer(deserializationOptions), serializedAtn)
        parser.bypassAltsAtnCache[serializedAtn] = result
    end
    return result
end

# The preferred method of getting a tree pattern. For example, here's a
# sample use:
#
# <pre>
# ParseTree t = parser.expr();
# ParseTreePattern p = parser.compileParseTreePattern("&lt;ID&gt;+0", MyParser.RULE_expr);
# ParseTreeMatch m = p.match(t);
# String id = m.get("ID");
# </pre>
#
function compileParseTreePattern(parser::T, pattern::String, patternRuleIndex::Int, lexer::Lexer = nothing) where {T<:AbstractParser}
    if isnothing(lexer)
        if !isnothing(getTokenStream(parser))
            tokenSource = getTokenStream(parser).tokenSource
            if tokenSource isa Lexer
                lexer = tokenSource
            end
        end
    end
    if isnothing(lexer)
        throw(UnsupportedOperationException("Parser can't discover a lexer to use"))
    end

    m = ParseTreePatternMatcher(lexer, parser)
    return compile(m, pattern, patternRuleIndex)
end


function getInputStream(parser::T) where {T<:AbstractParser}
    return getTokenStream(parser)
end

function setInputStream(parser::T, input::InputStream) where {T<:AbstractParser}
    setTokenStream(parser, input)
end

function getTokenStream(parser::T) where {T<:AbstractParser}
    return parser._input
end

# Set the token stream and reset the parser.#
function setTokenStream(parser::T, input::TokenStream) where {T<:AbstractParser}
    parser._input = nothing
    reset!(parser)
    parser._input = input
end

# Match needs to return the current input symbol, which gets put
#  into the label for the associated token ref; e.g., x=ID.
#
function getCurrentToken(parser::T) where {T<:AbstractParser}
    return LT(parser._input, 1)
end

function notifyErrorListeners(parser::T, msg::String, offendingToken::Token = nothing, e::RecognitionException = nothing) where {T<:AbstractParser}
    if isnothing(offendingToken)
        offendingToken = getCurrentToken(parser)
    end
    parser._syntaxErrors += 1
    line = offendingToken.line
    column = offendingToken.column
    listener = getErrorListenerDispatch(parser)
    syntaxError(listener, parser, offendingToken, line, column, msg, e)
end

#
# Consume and return the {@linkplain #getCurrentToken current symbol}.
#
# <p>E.g., given the following input with {@code A} being the current
# lookahead symbol, parser function moves the cursor to {@code B} and returns
# {@code A}.</p>
#
# <pre>
#  A B
#  ^
# </pre>
#
# If the parser is not in error recovery mode, the consumed symbol is added
# to the parse tree using {@link ParserRuleContext#addChild!(Token)}, and
# {@link ParseTreeListener#visitTerminal} is called on any parse listeners.
# If the parser <em>is</em> in error recovery mode, the consumed symbol is
# added to the parse tree using
# {@link ParserRuleContext#addErrorNode!(Token)}, and
# {@link ParseTreeListener#visitErrorNode} is called on any parse
# listeners.
#
function consume(parser::T) where {T<:AbstractParser}
    o = getCurrentToken(parser)
    if o.type != TOKEN_EOF
        consume(getInputStream(parser))
    end
    hasListener = !isnothing(parser._parseListeners) && length(parser._parseListeners)>0
    if parser.buildParseTrees || hasListener
        if inErrorRecoveryMode(parser._errHandler, parser)
            node = addErrorNode!(parser._ctx, o)
        else
            node = addTokenNode!(parser._ctx, o)
        end
        if hasListener
            for listener in parser._parseListeners
                if node isa ErrorNode
                    visitErrorNode(listener, node)
                elseif node isa TerminalNode
                    visitTerminal(listener, node)
                end
            end
        end
    end
    return o
end

function addContextToParseTree(parser::T) where {T<:AbstractParser}
    # add current context to parent if we have a parent
    if !isnothing(parser._ctx.parentCtx)
        addChild!(parser._ctx.parentCtx, parser._ctx)
    end
end

# Always called by generated parsers upon entry to a rule. Access field
# {@link #_ctx} get the current context.
#
function enterRule(parser::M, localctx::S , state::Int , ruleIndex::T) where {M<:AbstractParser,S<:ParserRuleContext,T<:SuperEnum.Enum}
    parser.stateNumber = state
    parser._ctx = localctx
    parser._ctx.start = LT(parser._input, 1)
    if parser.buildParseTrees
        addContextToParseTree(parser)
    end
    if !isnothing(parser._parseListeners)
        triggerEnterRuleEvent(parser)
    end
end

function exitRule(parser::T) where {T<:AbstractParser}
    parser._ctx.stop = LT(parser._input, -1)
    # trigger event on _ctx, before it reverts to parent
    if !isnothing(parser._parseListeners)
        triggerExitRuleEvent(parser)
    end
    parser.stateNumber = parser._ctx.invokingState
    parser._ctx = parser._ctx.parentCtx
end

function enterOuterAlt(parser::T, localctx::ParserRuleContext, altNum::Int) where {T<:AbstractParser}
    setAltNumber(localctx, altNum)
    # if we have new localctx, make sure we replace existing ctx
    # that is previous child of parse tree
    if parser.buildParseTrees && parser._ctx != localctx
        if !isnothing(parser._ctx.parentCtx)
            removeLastChild(parser._ctx.parentCtx)
            addChild!(parser._ctx.parentCtx, localctx)
        end
    end
    parser._ctx = localctx
end

# Get the precedence level for the top-most precedence rule.
#
# @return The precedence level for the top-most precedence rule, or -1 if
# the parser context is not nested within a precedence rule.
#
function getPrecedence(parser::T) where {T<:AbstractParser}
    if length(parser._precedenceStack)==0
        return -1
    else
        return parser._precedenceStack[end]
    end
end

function enterRecursionRule(parser::T, localctx::ParserRuleContext, state::Int, ruleIndex::Int, precedence::Int) where {T<:AbstractParser}
    parser.stateNumber = state
    push!(parser._precedenceStack, precedence)
    parser._ctx = localctx
    parser._ctx.start = LT(parser._input, 1)
    if !isnothing(parser._parseListeners)
        triggerEnterRuleEvent(parser) # simulates rule entry for left-recursive rules
    end
end

#
# Like {@link #enterRule} but for recursive rules.
#
function pushNewRecursionContext(parser::T, localctx::ParserRuleContext, state::Int, ruleIndex::Int) where {T<:AbstractParser}
    previous = parser._ctx
    previous.parentCtx = localctx
    previous.invokingState = state
    previous.stop = LT(parser._input, -1)

    parser._ctx = localctx
    parser._ctx.start = previous.start
    if parser.buildParseTrees
        addChild!(parser._ctx, previous)
    end

    if !isnothing(parser._parseListeners)
        triggerEnterRuleEvent(parser) # simulates rule entry for left-recursive rules
    end
end

function unrollRecursionContexts(parser::T, parentCtx::ParserRuleContext) where {T<:AbstractParser}
    pop!(parser._precedenceStack)
    parser._ctx.stop = LT(parser._input, -1)
    retCtx = parser._ctx # save current ctx (return value)
    # unroll so _ctx is as it was before call to recursive method
    if !isnothing(parser._parseListeners)
        while !(parser._ctx isa parentCtx)
            triggerExitRuleEvent(parser)
            parser._ctx = parser._ctx.parentCtx
        end
    else
        parser._ctx = parentCtx
    end

    # hook into tree
    retCtx.parentCtx = parentCtx

    if parser.buildParseTrees && !isnothing(parentCtx)
        # add return ctx into invoking rule's tree
        addChild!(parentCtx, retCtx)
    end
end

function getInvokingContext(parser::T, ruleIndex::Int) where {T<:AbstractParser}
    ctx = parser._ctx
    while !isnothing(ctx)
        if getRuleIndex(ctx) == ruleIndex
            return ctx
        end
        ctx = ctx.parentCtx
    end
    return nothing
end


function precpred(parser::T, localctx::RuleContext , precedence::Int) where {T<:AbstractParser}
    return precedence >= parser._precedenceStack[end]
end

function inContext(::T, ::String) where {T<:AbstractParser}
    # TODO: useful in parser?
    return false
end

#
# Checks whether or not {@code symbol} can follow the current state in the
# ATN. The behavior of parser method is equivalent to the following, but is
# implemented such that the complete context-sensitive follow set does not
# need to be explicitly constructed.
#
# <pre>
# return getExpectedTokens().contains(symbol);
# </pre>
#
# @param symbol the symbol type to check
# @return {@code true} if {@code symbol} can follow the current state in
# the ATN, otherwise {@code false}.
#
function isExpectedToken(parser::T, symbol::Int) where {T<:AbstractParser}
    atn = parser._interp.atn
    ctx = parser._ctx
    s = atn.states[parser.stateNumber+1]
    following = nextTokens(atn, s)
    if symbol in following
        return true
    end
    if !(TOKEN_EPSILON in following)
        return false
    end

    while !isnothing(ctx) && ctx.invokingState>=0 && TOKEN_EPSILON in following
        invokingState = atn.states[ctx.invokingState+1]
        rt = invokingState.transitions[1]
        following = nextTokens(atn, rt.followState)
        if symbol in following
            return true
        end
        ctx = ctx.parentCtx
    end

    if TOKEN_EPSILON in following && symbol == TOKEN_EOF
        return true
    else
        return false 
    end
end

# Computes the set of input symbols which could follow the current parser
# state and context, as given by {@link #getState} and {@link #getContext},
# respectively.
#
# @see ATN#getExpectedTokens(int, RuleContext)
#
function getExpectedTokens(parser)
    return getExpectedTokens(parser._interp.atn, parser.stateNumber, parser._ctx)
end

function getExpectedTokensWithinCurrentRule(parser)
    atn = parser._interp.atn
    s = atn.states[parser.stateNumber+1]
    return nextTokens(atn, s)
end

# Get a rule's index (i.e., {@code RULE_ruleName} field) or -1 if not found.#
function getRuleIndex(parser::T, ruleName::String) where {T<:AbstractParser}
    ruleIndex = get(getRuleIndexMap(parser), ruleName, nothing)
    if !isnothing(ruleIndex)
        return ruleIndex
    else
        return -1
    end
end

# Return List&lt;String&gt; of the rule names in your parser instance
#  leading up to a call to the current rule.  You could override if
#  you want more details such as the file/line info of where
#  in the ATN a rule is invoked.
#
#  this is very useful for error messages.
#
function getRuleInvocationStack(parser::T, p::RuleContext=nothing) where {T<:AbstractParser}
    if isnothing(p)
        p = parser._ctx
    end
    stack = String[]
    while !isnothing(p)
        # compute what follows who invoked us
        ruleIndex = getRuleIndex(p)
        if ruleIndex<0
            push!(stack, "n/a")
        else
            push!(stack, parser.ruleNames[ruleIndex])
        end
        p = p.parentCtx
    end
    return stack
end
    

# For debugging and other purposes.#
function getDFAStrings(parser::T) where {T<:AbstractParser}
    return [ string(dfa) for dfa in parser._interp.decisionToDFA]
end

# For debugging and other purposes.#
function dumpDFA(parser::T) where {T<:AbstractParser}
    seenothing = false
    for i in 1:length(parser._interp.decisionToDFA)
        dfa = parser._interp.decisionToDFA[i]
        if length(dfa.states)>0
            if seenothing
                open(parser._output, "w") do io
                    write(io, "")
                end
            end
            open(parser._output, "w") do io
                write(io, "Decision " * string(dfa.decision) * ":")
                write(io, toString(dfa, parser.literalNames, parser.symbolicNames))
            end
            seenothing = true
        end
    end
end


function getSourceName(parser::T) where {T<:AbstractParser}
    return parser._input.sourceName
end

# During a parse is sometimes useful to listen in on the rule entry and exit
#  events as well as token matches. parser is for quick and dirty debugging.
#
function setTrace(parser::T, trace::Bool) where {T<:AbstractParser}
    if !trace
        removeParseListener(parser, parser._tracer)
        parser._tracer = nothing
    else
        if !isnothing(parser._tracer)
            removeParseListener(parser, parser._tracer)
        end
        parser._tracer = TraceListener(parser)
        addParseListener(parser, parser._tracer)
    end
end

function Base.getproperty(parser::Parser, field::Symbol)
    if hasfield(Parser, field)
        return getfield(parser, field)
    elseif hasfield(Recognizer, field)
        return getfield(getfield(parser, :_recog), field)
    end
    error("type Parser has no field $field")
end

function Base.setproperty!(parser::Parser, field::Symbol, value::S) where {S}
    if hasfield(Parser, field)
        return setfield!(parser, field, value)
    elseif hasfield(Recognizer, field)
        return setfield!(getfield(parser, :_recog), field, value)
    end
    error("type Parser has no field $field")
end

function Base.getproperty(parser::T, field::Symbol) where {T<:AbstractParser}
    if hasfield(T, field)
        return getfield(parser, field)
    elseif field == :state
        return getproperty(getfield(parser, :_parser), :stateNumber)
    else
        return getproperty(getfield(parser, :_parser), field)
    end
end

function Base.setproperty!(parser::T, field::Symbol, value::S) where {T<:AbstractParser, S}
    if hasfield(T, field)
        return setfield!(parser, field, value)
    elseif hasfield(Parser, field)
        return setfield!(getfield(parser, :_parser), field, value)
    end
    Base.setproperty!(getfield(parser, :_parser), field, value)
end