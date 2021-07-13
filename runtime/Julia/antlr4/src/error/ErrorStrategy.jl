#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#
# import sys
# from antlr4.IntervalSet import IntervalSet

# from antlr4.Token import Token
# from antlr4.atn.ATNState import ATNState
# from antlr4.error.Errors import RecognitionException, NoViableAltException, InputMismatchException, \
#     FailedPredicateException, ParseCancellationException

abstract type ErrorStrategy end
function reset(::ErrorStrategy, recognizer::AbstractRecognizer)
    error("Not Implemented")
end

function recoverInline(::ErrorStrategy, recognizer::AbstractRecognizer)
    error("Not Implemented")
end

function recover(::ErrorStrategy, recognizer::AbstractParser, e::RecognitionException)
    error("Not Implemented")
end

function sync(::ErrorStrategy, recognizer::AbstractParser)
    error("Not Implemented")
end

function inErrorRecoveryMode(::ErrorStrategy, recognizer::AbstractParser)
    error("Not Implemented")
end

function reportError(::ErrorStrategy, recognizer::AbstractParser, e::RecognitionException)
    error("Not Implemented")
end


mutable struct DefaultErrorStrategy <: ErrorStrategy
    # Indicates whether the error strategy is currently "recovering from an
    # error". This is used to suppress reporting multiple error messages while
    # attempting to recover from a detected syntax error.
    #
    # @see #inErrorRecoveryMode
    errorRecoveryMode::Bool
    # The index into the input stream where the last error occurred.
    # 	This is used to prevent infinite loops where an error is found
    #  but no token is consumed during recovery...another error is found,
    #  ad nauseum.  This is a failsafe mechanism to guarantee that at least
    #  one token/tree node is consumed for two errors.
    lastErrorIndex::Int
    lastErrorStates::nATNState
    nextTokensContext::Nothing
    nextTokenState::Int
    function DefaultErrorStrategy()
        new(false, -1, nothing, nothing, 0)
    end
end

# <p>The functionault implementation simply calls {@link #endErrorCondition} to
# ensure that the handler is not in error recovery mode.</p>
function reset!(des::DefaultErrorStrategy, recognizer::AbstractParser)
    endErrorCondition(des, recognizer)
end

#
# This method is called to enter error recovery mode when a recognition
# exception is reported.
#
# @param recognizer the parser instance
#
function beginErrorCondition(des::DefaultErrorStrategy, ::AbstractParser)
    des.errorRecoveryMode = true
end

function inErrorRecoveryMode(des::DefaultErrorStrategy, ::AbstractParser)
    return des.errorRecoveryMode
end

#
# This method is called to leave error recovery mode after recovering from
# a recognition exception.
#
# @param recognizer
#
function endErrorCondition(des::DefaultErrorStrategy, ::AbstractParser)
    des.errorRecoveryMode = false
    des.lastErrorStates = nothing
    des.lastErrorIndex = -1
end

#
# {@inheritDoc}
#
# <p>The functionault implementation simply calls {@link #endErrorCondition}.</p>
#
function reportMatch(des::DefaultErrorStrategy, recognizer::AbstractParser)
    endErrorCondition(des, recognizer)
end

#
# {@inheritDoc}
#
# <p>The functionault implementation returns immediately if the handler is already
# in error recovery mode. Otherwise, it calls {@link #beginErrorCondition}
# and dispatches the reporting task based on the runtime type of {@code e}
# according to the following table.</p>
#
# <ul>
# <li>{@link NoViableAltException}: Dispatches the call to
# {@link #reportNoViableAlternative}</li>
# <li>{@link InputMismatchException}: Dispatches the call to
# {@link #reportInputMismatch}</li>
# <li>{@link FailedPredicateException}: Dispatches the call to
# {@link #reportFailedPredicate}</li>
# <li>All other types: calls {@link AbstractParser#notifyErrorListeners} to report
# the exception</li>
# </ul>
#
function reportError(des::DefaultErrorStrategy, recognizer::AbstractParser, e::RecognitionException)
    # if we've already reported an error and have not matched a token
    # yet successfully, don't report any errors.
    if inErrorRecoveryMode(des, recognizer)
        return # don't report spurious errors
    end
    beginErrorCondition(des, recognizer)
    if e isa NoViableAltException
        reportNoViableAlternative(des, recognizer, e)
    elseif e isa InputMismatchException
        reportInputMismatch(des, recognizer, e)
    elseif  e isa FailedPredicateException 
        reportFailedPredicate(des, recognizer, e)
    else
        println("unknown recognition error type: " + string(typeof(e)))
        notifyErrorListeners(recognizer, e.message, e.offendingToken, e)
    end
end

#
# {@inheritDoc}
#
# <p>The functionault implementation resynchronizes the parser by consuming tokens
# until we find one in the resynchronization set--loosely the set of tokens
# that can follow the current rule.</p>
#
function recover(des::DefaultErrorStrategy, recognizer::AbstractParser, e::RecognitionException)
    if des.lastErrorIndex == getInputStream(recognizer).index &&
        !isnothing(des.lastErrorStates) &&
        recognizer.state in des.lastErrorStates
        # uh oh, another error at same token index and previously-visited
        # state in ATN; must be a case where LT(1) is in the recovery
        # token set so nothing got consumed. Consume a single token
        # at least to prevent an infinite loop; this is a failsafe.
        consume(recognizer)
    end

    des.lastErrorIndex = recognizer._input.index
    if isnothing(des.lastErrorStates)
        des.lastErrorStates = []
    end
    push!(des.lastErrorStates, recognizer.state)
    followSet = getErrorRecoverySet(des, recognizer)
    consumeUntil(des, recognizer, followSet)
end

# The functionault implementation of {@link ANTLRErrorStrategy#sync} makes sure
# that the current lookahead symbol is consistent with what were expecting
# at this point in the ATN. You can call this anytime but ANTLR only
# generates code to check before subrules/loops and each iteration.
#
# <p>Implements Jim Idle's magic sync mechanism in closures and optional
# subrules. E.g.,</p>
#
# <pre>
# a : sync ( stuff sync )* ;
# sync : {consume to what can follow sync} ;
# </pre>
#
# At the start of a sub rule upon error, {@link #sync} performs single
# token deletion, if possible. If it can't do that, it bails on the current
# rule and uses the functionault error recovery, which consumes until the
# resynchronization set of the current rule.
#
# <p>If the sub rule is optional ({@code (...)?}, {@code (...)*}, or block
# with an empty alternative), then the expected set includes what follows
# the subrule.</p>
#
# <p>During loop iteration, it consumes until it sees a token that can start a
# sub rule or what follows loop. Yes, that is pretty aggressive. We opt to
# stay in the loop as long as possible.</p>
#
# <p><strong>ORIGINS</strong></p>
#
# <p>Previous versions of ANTLR did a poor job of their recovery within loops.
# A single mismatch token or missing token would force the parser to bail
# out of the entire rules surrounding the loop. So, for rule</p>
#
# <pre>
# classfunction : 'class' ID '{' member* '}'
# </pre>
#
# input with an extra token between members would force the parser to
# consume until it found the next class functioninition rather than the next
# member functioninition of the current class.
#
# <p>This functionality cost a little bit of effort because the parser has to
# compare token set at the start of the loop and at each iteration. If for
# some reason speed is suffering for you, you can turn off this
# functionality by simply overriding this method as a blank { }.</p>
#
function sync(des::DefaultErrorStrategy, recognizer::AbstractParser)
    # If already recovering, don't try to sync
    if inErrorRecoveryMode(des, recognizer)
        return
    end

    s = recognizer._interp.atn.states[recognizer.state+1]
    la = LA(1, getTokenStream(recognizer))
    # try cheaper subset first; might get lucky. seems to shave a wee bit off
    nextTokens = nextTokens(recognizer.atn, s)
    if la in nextTokens
        des.nextTokensContext = nothing
        des.nextTokenState = ATNStateType.INVALID_STATE_NUMBER
        return
    elseif Token.EPSILON in nextTokens
        if isnothing(des.nextTokensContext)
            # It's possible the next token won't match information tracked
            # by sync is restricted for performance.
            des.nextTokensContext = recognizer._ctx
            des.nextTokensState = recognizer._stateNumber
        end
        return
    end

    if s.stateType in [ATNState.BLOCK_START, ATNState.STAR_BLOCK_START,
                            ATNState.PLUS_BLOCK_START, ATNState.STAR_LOOP_ENTRY]
        # report error and recover if possible
        if !isnothing(des.singleTokenDeletion(recognizer))
            return
        else
            throw(InputMismatchException(recognizer))
        end
    elseif s.stateType in [ATNState.PLUS_LOOP_BACK, ATNState.STAR_LOOP_BACK]
        reportUnwantedToken(des, recognizer)
        expecting = getExpectedTokens(recognizer)
        whatFollowsLoopIterationOrRule = addSet(expecting, getErrorRecoverySet(des, recognizer))
        consumeUntil(des, recognizer, whatFollowsLoopIterationOrRule)

    else
        # do nothing if we can't identify the exact kind of ATN state
    end
end

# This is called by {@link #reportError} when the exception is a
# {@link NoViableAltException}.
#
# @see #reportError
#
# @param recognizer the parser instance
# @param e the recognition exception
#
function reportNoViableAlternative(des::DefaultErrorStrategy, recognizer::AbstractParser, e::NoViableAltException)
    tokens = getTokenStream(recognizer)
    if !isnothing(tokens )
        if e.startToken.type==TOKEN_EOF
            input = "<EOF>"
        else
            input = tokens.getText(e.startToken, e.offendingToken)
        end
    else
        input = "<unknown input>"
    end
    msg = "no viable alternative at input " + des.escapeWSAndQuote(input)
    notifyErrorListeners(recognizer, msg, e.offendingToken, e)
    return
end

#
# This is called by {@link #reportError} when the exception is an
# {@link InputMismatchException}.
#
# @see #reportError
#
# @param recognizer the parser instance
# @param e the recognition exception
#
function reportInputMismatch(des::DefaultErrorStrategy, recognizer::AbstractParser, e::InputMismatchException)
    msg = "mismatched input " * getTokenErrorDisplay(des, e.offendingToken) *
            " expecting " * toString(getExpectedTokens(e), recognizer.literalNames, recognizer.symbolicNames)
    notifyErrorListeners(recognizer, msg, e.offendingToken, e)
    return
end

#
# This is called by {@link #reportError} when the exception is a
# {@link FailedPredicateException}.
#
# @see #reportError
#
# @param recognizer the parser instance
# @param e the recognition exception
#
function reportFailedPredicate(des::DefaultErrorStrategy, recognizer, e)
    ruleName = recognizer.ruleNames[recognizer._ctx.getRuleIndex()+1]
    msg = "rule " * ruleName * " " * e.message
    notifyErrorListeners(recognizer, msg, e.offendingToken, e)
    return
end

# This method is called to report a syntax error which requires the removal
# of a token from the input stream. At the time this method is called, the
# erroneous symbol is current {@code LT(1)} symbol and has not yet been
# removed from the input stream. When this method returns,
# {@code recognizer} is in error recovery mode.
#
# <p>This method is called when {@link #singleTokenDeletion} identifies
# single-token deletion as a viable recovery strategy for a mismatched
# input error.</p>
#
# <p>The functionault implementation simply returns if the handler is already in
# error recovery mode. Otherwise, it calls {@link #beginErrorCondition} to
# enter error recovery mode, followed by calling
# {@link AbstractParser#notifyErrorListeners}.</p>
#
# @param recognizer the parser instance
#
function reportUnwantedToken(des::DefaultErrorStrategy, recognizer::AbstractParser)
    if inErrorRecoveryMode(des, recognizer)
        return
    end

    beginErrorCondition(des, recognizer)
    t = getCurrentToken(recognizer)
    tokenName = getTokenErrorDisplay(des, t)
    expecting = getExpectedTokens(des, recognizer)
    msg = "extraneous input " * tokenName * " expecting " *
        toString(expecting, recognizer.literalNames, recognizer.symbolicNames)
    notifyErrorListeners(recognizer, msg, t, nothing)
    return
end

# This method is called to report a syntax error which requires the
# insertion of a missing token into the input stream. At the time this
# method is called, the missing token has not yet been inserted. When this
# method returns, {@code recognizer} is in error recovery mode.
#
# <p>This method is called when {@link #singleTokenInsertion} identifies
# single-token insertion as a viable recovery strategy for a mismatched
# input error.</p>
#
# <p>The functionault implementation simply returns if the handler is already in
# error recovery mode. Otherwise, it calls {@link #beginErrorCondition} to
# enter error recovery mode, followed by calling
# {@link AbstractParser#notifyErrorListeners}.</p>
#
# @param recognizer the parser instance
#
function reportMissingToken(des::DefaultErrorStrategy, recognizer::AbstractParser)
    if inErrorRecoveryMode(des, recognizer)
        return
    end
    beginErrorCondition(des, recognizer)
    t = getCurrentToken(recognizer)
    expecting = getExpectedTokens(des, recognizer)
    msg = "missing " * expecting.toString(recognizer.literalNames, recognizer.symbolicNames) *
            " at " * des.getTokenErrorDisplay(t)
    notifyErrorListeners(recognizer, msg, t, nothing)
end

# <p>The functionault implementation attempts to recover from the mismatched input
# by using single token insertion and deletion as described below. If the
# recovery attempt fails, this method throws an
# {@link InputMismatchException}.</p>
#
# <p><strong>EXTRA TOKEN</strong> (single token deletion)</p>
#
# <p>{@code LA(1)} is not what we are looking for. If {@code LA(2)} has the
# right token, however, then assume {@code LA(1)} is some extra spurious
# token and delete it. Then consume and return the next token (which was
# the {@code LA(2)} token) as the successful result of the match operation.</p>
#
# <p>This recovery strategy is implemented by {@link #singleTokenDeletion}.</p>
#
# <p><strong>MISSING TOKEN</strong> (single token insertion)</p>
#
# <p>If current token (at {@code LA(1)}) is consistent with what could come
# after the expected {@code LA(1)} token, then assume the token is missing
# and use the parser's {@link TokenFactory} to create it on the fly. The
# "insertion" is performed by returning the created token as the successful
# result of the match operation.</p>
#
# <p>This recovery strategy is implemented by {@link #singleTokenInsertion}.</p>
#
# <p><strong>EXAMPLE</strong></p>
#
# <p>For example, Input {@code i=(3;} is clearly missing the {@code ')'}. When
# the parser returns from the nested call to {@code expr}, it will have
# call chain:</p>
#
# <pre>
# stat &rarr; expr &rarr; atom
# </pre>
#
# and it will be trying to match the {@code ')'} at this point in the
# derivation:
#
# <pre>
# =&gt; ID '=' '(' INT ')' ('+' atom)* ';'
#                    ^
# </pre>
#
# The attempt to match {@code ')'} will fail when it sees {@code ';'} and
# call {@link #recoverInline}. To recover, it sees that {@code LA(1)==';'}
# is in the set of tokens that can follow the {@code ')'} token reference
# in rule {@code atom}. It can assume that you forgot the {@code ')'}.
#
function recoverInline(des::DefaultErrorStrategy, recognizer::AbstractParser)
    # SINGLE TOKEN DELETION
    matchedSymbol = singleTokenDeletion(des, recognizer)
    if !isnothing(matchedSymbol)
        # we have deleted the extra token.
        # now, move past ttype token as if all were ok
        consume(recognizer)
        return matchedSymbol
    end

    # SINGLE TOKEN INSERTION
    if singleTokenInsertion(des, recognizer)
        return getMissingSymbol(des, recognizer)
    end

    # even that didn't work; must throw the exception
    throw(InputMismatchException(recognizer))
end

#
# This method implements the single-token insertion inline error recovery
# strategy. It is called by {@link #recoverInline} if the single-token
# deletion strategy fails to recover from the mismatched input. If this
# method returns {@code true}, {@code recognizer} will be in error recovery
# mode.
#
# <p>This method determines whether or not single-token insertion is viable by
# checking if the {@code LA(1)} input symbol could be successfully matched
# if it were instead the {@code LA(2)} symbol. If this method returns
# {@code true}, the caller is responsible for creating and inserting a
# token with the correct type to produce this behavior.</p>
#
# @param recognizer the parser instance
# @return {@code true} if single-token insertion is a viable recovery
# strategy for the current mismatched input, otherwise {@code false}
#
function singleTokenInsertion(des::DefaultErrorStrategy, recognizer::AbstractParser)
    currentSymbolType = LA(getTokenStream(recognizer), 1)
    # if current token is consistent with what could come after current
    # ATN state, then we know we're missing a token; error recovery
    # is free to conjure up and insert the missing token
    atn = recognizer._interp.atn
    currentState = atn.states[recognizer.state+1]
    next = currentState.transitions[1].target
    expectingAtLL2 = nextTokens(atn, next, recognizer._ctx)
    if currentSymbolType in expectingAtLL2
        reportMissingToken(des, recognizer)
        return true
    else
        return false
    end
end

# This method implements the single-token deletion inline error recovery
# strategy. It is called by {@link #recoverInline} to attempt to recover
# from mismatched input. If this method returns null, the parser and error
# handler state will not have changed. If this method returns non-null,
# {@code recognizer} will <em>not</em> be in error recovery mode since the
# returned token was a successful match.
#
# <p>If the single-token deletion is successful, this method calls
# {@link #reportUnwantedToken} to report the error, followed by
# {@link AbstractParser#consume} to actually "delete" the extraneous token. Then,
# before returning {@link #reportMatch} is called to signal a successful
# match.</p>
#
# @param recognizer the parser instance
# @return the successfully matched {@link Token} instance if single-token
# deletion successfully recovers from the mismatched input, otherwise
# {@code null}
#
function singleTokenDeletion(des::DefaultErrorStrategy, recognizer::AbstractParser)
    nextTokenType = LA(2, getTokenStream(recognizer))
    expecting = getExpectedTokens(des, recognizer)
    if nextTokenType in expecting
        reportUnwantedToken(des, recognizer)
        # print("recoverFromMismatchedToken deleting " \
        #     + string(recognizer.getTokenStream().LT(1)) \
        #     + " since " + string(recognizer.getTokenStream().LT(2)) \
        #     + " is what we want", file=sys.stderr)
        consume(recognizer) # simply delete extra token
        # we want to return the token we're actually matching
        matchedSymbol = getCurrentToken(recognizer)
        reportMatch(des, recognizer) # we know current token is correct
        return matchedSymbol
    else
        return nothing
    end
end

# Conjure up a missing token during error recovery.
#
#  The recognizer attempts to recover from single missing
#  symbols. But, actions might refer to that missing symbol.
#  For example, x=ID {f($x);}. The action clearly assumes
#  that there has been an identifier matched previously and that
#  $x points at that token. If that token is missing, but
#  the next token in the stream is what we want we assume that
#  this token is missing and we keep going. Because we
#  have to return some token to replace the missing token,
#  we have to conjure one up. This method gives the user control
#  over the tokens returned for missing tokens. Mostly,
#  you will want to create something special for identifier
#  tokens. For literals such as '{' and ',', the functionault
#  action in the parser or tree parser works. It simply creates
#  a CommonToken of the appropriate type. The text will be the token.
#  If you change what tokens must be created by the lexer,
#  override this method to create the appropriate tokens.
#
function getMissingSymbol(des::DefaultErrorStrategy, recognizer::AbstractParser)
    currentSymbol = getCurrentToken(recognizer)
    expecting = des.getExpectedTokens(recognizer)
    expectedTokenType = expecting[1] # get any element
    if expectedTokenType==TOKEN_EOF
        tokenText = "<missing EOF>"
    else
        name = nothing
        if expectedTokenType < length(recognizer.literalNames)
            name = recognizer.literalNames[expectedTokenType]
        end
        if !isnothing(name) && expectedTokenType < length(recognizer.symbolicNames)
            name = recognizer.symbolicNames[expectedTokenType+1]
        end
        tokenText = "<missing " * string(name) * ">"
    end
    current = currentSymbol
    lookback = LT(-1, getTokenStream(recognizer))
    if current.type==TOKEN_EOF && !isnothing(lookback)
        current = lookback
    end
    return recognizer.getTokenFactory().create(current.source,
        expectedTokenType, tokenText, TOKEN_DEFAULT_CHANNEL,
        -1, -1, current.line, current.column)
end

function getExpectedTokens(::DefaultErrorStrategy, recognizer::AbstractParser)
    return getExpectedTokens(des, recognizer)
end

# How should a token be displayed in an error message? The functionault
#  is to display just the text, but during development you might
#  want to have a lot of information spit out.  Override in that case
#  to use t.toString() (which, for CommonToken, dumps everything about
#  the token). This is better than forcing you to override a method in
#  your token objects because you don't have to go modify your lexer
#  so that it creates a new Java type.
#
function getTokenErrorDisplay(des::DefaultErrorStrategy, t::Token)
    if !isnothing(t)
        return "<no token>"
    end
    s = t.text
    if !isnothing(s)
        if t.type==TOKEN_EOF
            s = "<EOF>"
        else
            s = "<" * string(t.type) * ">"
        end
    end
    return escapeWSAndQuote(des, s)
end

function escapeWSAndQuote(::DefaultErrorStrategy, s::String)
    s = replace(s, "\n","\\n")
    s = replace(s, "\r","\\r")
    s = replace(s, "\t","\\t")
    return "'" * s * "'"
end

#  Compute the error recovery set for the current rule.  During
#  rule invocation, the parser pushes the set of tokens that can
#  follow that rule reference on the stack; this amounts to
#  computing FIRST of what follows the rule reference in the
#  enclosing rule. See LinearApproximator.FIRST().
#  This local follow set only includes tokens
#  from within the rule; i.e., the FIRST computation done by
#  ANTLR stops at the end of a rule.
#
#  EXAMPLE
#
#  When you find a "no viable alt exception", the input is not
#  consistent with any of the alternatives for rule r.  The best
#  thing to do is to consume tokens until you see something that
#  can legally follow a call to r#or* any rule that called r.
#  You don't want the exact set of viable next tokens because the
#  input might just be missing a token--you might consume the
#  rest of the input looking for one of the missing tokens.
#
#  Consider grammar:
#
#  a : '[' b ']'
#    | '(' b ')'
#    ;
#  b : c '^' INT ;
#  c : ID
#    | INT
#    ;
#
#  At each rule invocation, the set of tokens that could follow
#  that rule is pushed on a stack.  Here are the various
#  context-sensitive follow sets:
#
#  FOLLOW(b1_in_a) = FIRST(']') = ']'
#  FOLLOW(b2_in_a) = FIRST(')') = ')'
#  FOLLOW(c_in_b) = FIRST('^') = '^'
#
#  Upon erroneous input "[]", the call chain is
#
#  a -> b -> c
#
#  and, hence, the follow context stack is:
#
#  depth     follow set       start of rule execution
#    0         <EOF>                    a (from main())
#    1          ']'                     b
#    2          '^'                     c
#
#  Notice that ')' is not included, because b would have to have
#  been called from a different context in rule a for ')' to be
#  included.
#
#  For error recovery, we cannot consider FOLLOW(c)
#  (context-sensitive or otherwise).  We need the combined set of
#  all context-sensitive FOLLOW sets--the set of all tokens that
#  could follow any reference in the call chain.  We need to
#  resync to one of those tokens.  Note that FOLLOW(c)='^' and if
#  we resync'd to that token, we'd consume until EOF.  We need to
#  sync to context-sensitive FOLLOWs for a, b, and c: {']','^'}.
#  In this case, for input "[]", LA(1) is ']' and in the set, so we would
#  not consume anything. After printing an error, rule c would
#  return normally.  Rule b would not find the required '^' though.
#  At this point, it gets a mismatched token error and throws an
#  exception (since LA(1) is not in the viable following token
#  set).  The rule exception handler tries to recover, but finds
#  the same recovery set and doesn't consume anything.  Rule b
#  exits normally returning to rule a.  Now it finds the ']' (and
#  with the successful match exits errorRecovery mode).
#
#  So, you can see that the parser walks up the call chain looking
#  for the token that was a member of the recovery set.
#
#  Errors are not generated in errorRecovery mode.
#
#  ANTLR's error recovery mechanism is based upon original ideas:
#
#  "Algorithms + Data Structures = Programs" by Niklaus Wirth
#
#  and
#
#  "A note on error recovery in recursive descent parsers":
#  http:#portal.acm.org/citation.cfm?id=947902.947905
#
#  Later, Josef Grosch had some good ideas:
#
#  "Efficient and Comfortable Error Recovery in Recursive Descent
#  AbstractParsers":
#  ftp:#www.cocolab.com/products/cocktail/doca4.ps/ell.ps.zip
#
#  Like Grosch I implement context-sensitive FOLLOW sets that are combined
#  at run-time upon error to avoid overhead during parsing.
#
function getErrorRecoverySet(::DefaultErrorStrategy, recognizer::AbstractParser)
    atn = recognizer._interp.atn
    ctx = recognizer._ctx
    recoverSet = IntervalSet()
    while !isnothing(ctx) && ctx.invokingState>=0
        # compute what follows who invoked us
        invokingState = atn.states[ctx.invokingState+1]
        rt = invokingState.transitions[1]
        follow = nextTokens(atn, rt.followState)
        addSet(recoverSet, follow)
        ctx = ctx.parentCtx
    end
    removeOne(recoverSet, TOKEN_EPSILON)
    return recoverSet
end

    
# Consume tokens until one matches the given token set.#
function consumeUntil(::DefaultErrorStrategy, recognizer::AbstractParser, set::Set)
    ttype = LA(getTokenStream(recognizer), 1)
    while ttype != TOKEN_EOF && !(ttype in set)
        consume(recognizer)
        ttype = LA(getTokenStream(recognizer), 1)
    end
end


#
# This implementation of {@link ANTLRErrorStrategy} responds to syntax errors
# by immediately canceling the parse operation with a
# {@link ParseCancellationException}. The implementation ensures that the
# {@link AbstractParserRuleContext#exception} field is set for all parse tree nodes
# that were not completed prior to encountering the error.
#
# <p>
# This error strategy is useful in the following scenarios.</p>
#
# <ul>
# <li><strong>Two-stage parsing:</strong> This error strategy allows the first
# stage of two-stage parsing to immediately terminate if an error is
# encountered, and immediately fall back to the second stage. In addition to
# avoiding wasted work by attempting to recover from errors here, the empty
# implementation of {@link BailErrorStrategy#sync} improves the performance of
# the first stage.</li>
# <li><strong>Silent validation:</strong> When syntax errors are not being
# reported or logged, and the parse result is simply ignored if errors occur,
# the {@link BailErrorStrategy} avoids wasting work on recovering from errors
# when the result will be ignored either way.</li>
# </ul>
#
# <p>
# {@code myparser.setErrorHandler(new BailErrorStrategy());}</p>
#
# @see AbstractParser#setErrorHandler(ANTLRErrorStrategy)
#
struct BailErrorStrategy <: ErrorStrategy
    # Instead of recovering from exception {@code e}, re-throw it wrapped
    #  in a {@link ParseCancellationException} so it is not caught by the
    #  rule function catches.  Use {@link Exception#getCause()} to get the
    #  original {@link RecognitionException}.
    #
        # Indicates whether the error strategy is currently "recovering from an
    # error". This is used to suppress reporting multiple error messages while
    # attempting to recover from a detected syntax error.
    #
    # @see #inErrorRecoveryMode
    errorRecoveryMode::Bool
    # The index into the input stream where the last error occurred.
    # 	This is used to prevent infinite loops where an error is found
    #  but no token is consumed during recovery...another error is found,
    #  ad nauseum.  This is a failsafe mechanism to guarantee that at least
    #  one token/tree node is consumed for two errors.
    lastErrorIndex::Int
    lastErrorStates::nATNState
    nextTokensContext::Nothing
    nextTokenState::Int
    function defaultErrorStrategy()
        new(false, -1, nothing, nothing, 0)
    end
end

function recover(des::BailErrorStrategy, recognizer::AbstractParser, e::RecognitionException)
    context = recognizer._ctx
    while !isnothing(context)
        context.exception = e
        context = context.parentCtx
    end
    throw(ParseCancellationException(e))
end

# Make sure we don't attempt to recover inline; if the parser
#  successfully recovers, it won't throw an exception.
#
function recoverInline(des::BailErrorStrategy, recognizer::AbstractParser)
    recover(des, recognizer, InputMismatchException(recognizer))
end

# Make sure we don't attempt to recover from problems in subrules.#
function sync(::DefaultErrorStrategy, ::AbstractParser) end