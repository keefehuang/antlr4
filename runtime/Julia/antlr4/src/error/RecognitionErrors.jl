# The root of the ANTLR exception hierarchy. In general, ANTLR tracks just
#  3 kinds of errors: prediction errors, failed predicate errors, and
#  mismatched input errors. In each case, the parser knows where it is
#  in the input, where it is in the ATN, the rule invocation stack,
#  and what kind of problem occurred.
# <p>If the state number is not known, this method returns -1.</p>


# Gets the set of input symbols which could potentially follow the
# previously matched symbol at the time this exception was thrown.

# <p>If the set of expected tokens is not known and could not be computed,
# this method returns {@code null}.</p>

# @return The set of token types that could potentially follow the current
# state in the ATN, or {@code null} if the information is not available.
# /
function getExpectedTokens(e::RecognitionException)
    if !isnothing(e.recognizer)
        return getExpectedTokens(e.recognizer.atn, e.offendingState, e.ctx)
    else
        return nothing
    end
end

struct LexerNoViableAltException <: RecognitionException
    message::nString
    recognizer::AbstractRecognizer
    input::InputStream
    ctx::nParserRuleContext
    offendingToken::nToken
    offendingState::Int
    startIndex::Int
    deadEndConfigs::ATNConfigSet

    function LexerNoViableAltException(lexer::AbstractLexer, input::InputStream, startIndex::Int, deadEndConfigs::ATNConfigSet)
        if !isnothing(lexer)
            offendingState = lexer._recog.stateNumber
        else
            offendingState = -1
        end
        new(nothing, lexer, input, nothing, nothing, offendingState, startIndex, deadEndConfigs)
    end
end

function Base.string(lnvae::LexerNoViableAltException)
    symbol = ""
    if lnvae.startIndex >= 0 && lnvae.startIndex < lnvae.input.size
        symbol = getText(lnvae.input, lnvae.startIndex, lnvae.startIndex)
        # TODO symbol = Utils.escapeWhitespace(symbol, false);
    end
    return "LexerNoViableAltException('" * symbol * "')"
end

# Indicates that the parser could not decide which of two or more paths
#  to take based upon the remaining input. It tracks the starting token
#  of the offending input and also knows where the parser was
#  in the various paths when the error. Reported by reportNoViableAlternative()
#

struct NoViableAltException <: RecognitionException
    message::nString
    recognizer::AbstractRecognizer
    input::InputStream
    ctx::ParserRuleContext
    offendingToken::nToken
    offendingState::Int
    deadEndConfigs::ATNConfigSet
    startToken::nToken

    function NoViableAltException(recognizer::AbstractParser, input::nTokenStream=nothing, startToken::nToken=nothing,
        offendingToken::nToken=nothing, deadEndConfigs::nATNConfigSet=nothing, ctx::nParserRuleContext=nothing)
        
        if isnothing(ctx)
            ctx = recognizer._ctx
        end
        if isnothing(offendingToken)
            offendingToken = getCurrentToken(recognizer)
        end
        if isnothing(startToken)
            startToken = getCurrentToken(recognizer)
        end
        if isnothing(input)
            input = getInputStream(recognizer)
        end

        if !isnothing(recognizer)
            offendingState = recognizer.state
        else
            offendingState = -1
        end
        new(nothing, recognizer, input, ctx, offendingToken, offendingState, deadEndConfigs, startToken)
    end
end

# This signifies any kind of mismatched input exceptions such as
#  when the current input does not match the expected token.
#
struct InputMismatchException <: RecognitionException 
    message::nString
    recognizer::AbstractRecognizer
    input::InputStream
    ctx::ParserRuleContext
    offendingToken::nToken
    offendingState::Int

    function InputMismatchException(recognizer::Union{AbstractParser,Nothing})
        if !isnothing(recognizer)
            offendingState = recognizer.state
        else
            offendingState = -1
        end
        new(nothing, recognizer, getInputStream(recognizer), recognizer._ctx, getCurrentToken(recognizer), offendingState)
    end
end


# A semantic predicate failed during validation.  Validation of predicates
#  occurs when normally parsing the alternative just like matching a token.
#  Disambiguating predicate evaluation occurs when we test a predicate during
#  prediction.

struct FailedPredicateException <: RecognitionException
    message::nString
    recognizer::AbstractRecognizer
    input::InputStream
    ctx::ParserRuleContext
    offendingToken::nToken
    offendingState::Int
    ruleIndex::Int 
    predicateIndex::Int
    predicate::nString

    function FailedPredicateException(recognizer::AbstractParser, predicate::nString=nothing, message::nString=nothing)
        if !isnothing(recognizer)
            offendingState = recognizer.state
        else
            offendingState = -1
        end

        s = recognizer._interp.atn.states[recognizer.state]
        trans = s.transitions[0]
        
        if trans isa PredicateTransition
            ruleIndex = trans.ruleIndex
            predicateIndex = trans.predIndex
        else
            ruleIndex = 0
            predicateIndex = 0
        end
        predicate = predicate
        message = formatMessage(predicate, message)
        new(message, recognizer, getInputStream(recognizer), recognizer._ctx, getCurrentToken(recognizer), offendingState, ruleIndex, predicateIndex, predicate)
    end
end

function formatMessage(predicate::nString, message::nString)
    if !isnothing(message)
        return message
    else
        return "failed predicate: {" * predicate * "}?"
    end
end