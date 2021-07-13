#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

# need forward delcaration

mutable struct Recognizer <: AbstractRecognizer
    _listeners::Vector{AbstractListener}
    _interp::nATNSimulator
    stateNumber::Int
    function Recognizer()
        new(AbstractListener[], nothing, -1)
    end
    function Recognizer(listener::Vector{AbstractListener}, interp::Nothing, stateNumber::Int)
        new(listener, interp, stateNumber)
    end
end

function extractVersion(version::String)
    pos = findfirst(".", version)[1]
    major = version[1:pos-1]
    version = version[pos+1:end]
    pos = findfirst(version, ".")
    if pos == 1
        pos = findfirst(version, "-")
    end
    if pos == -1
        pos = length(version)
    end
    if isnothing(pos)
        pos = length(version)
    end
    minor = version[1:pos]
    return major, minor
end

function checkVersion(::AbstractRecognizer, toolVersion::String)
    runtimeVersion = "4.9.2"
    rvmajor, rvminor = extractVersion(runtimeVersion)
    tvmajor, tvminor = extractVersion(toolVersion)
    if rvmajor!=tvmajor || rvminor!=tvminor
        println("ANTLR runtime and generated code versions disagree: " * runtimeVersion * "!=" *toolVersion)
    end
end

function addErrorListener!(r::Recognizer, listener::AbstractListener)
    _listeners.append(r._listeners, listener)
end

function removeErrorListener!(r::Recognizer, listerner::AbstractListener)
    remove!(r._listeners, listerner)    
end

function removeErrorListeners!(r::Recognizer)
    r._listeners = AbstractListener[]
end

# seems broken
# function getTokenTypeMap(r::Recognizer)
#     tokenNames = self.getTokenNames()
#     if isnothing(tokenNames)
#         UnsupportedOperationException("The current recognizer does not provide a list of token names.")
#     end
#     result = self.tokenTypeMapCache.get(tokenNames, None)
#     if result is None:
#         result = zip( tokenNames, range(0, length(tokenNames)))
#         result["EOF"] = Token.EOF
#         self.tokenTypeMapCache[tokenNames] = result
#     end
#     return result
# end
# Get a map from rule names to rule indexes.
#
# <p>Used for XPath and tree pattern compilation.</p>
# #
# def getRuleIndexMap(self):
# ruleNames = self.getRuleNames()
# if ruleNames is None:
#     from antlr4.error.Errors import UnsupportedOperationException
#     raise UnsupportedOperationException("The current recognizer does not provide a list of rule names.")
# result = self.ruleIndexMapCache.get(ruleNames, None)
# if result is None:
#     result = zip( ruleNames, range(0, length(ruleNames)))
#     self.ruleIndexMapCache[ruleNames] = result
# return result

# function getTokenType(r::Recognizer, tokenName::String)
#     ttype = self.getTokenTypeMap().get(tokenName, None)
#     if ttype is
#         return ttype
#     else
#     return Token.INVALID_TYPE
#     end
# end


# What is the error header, normally line/character position information?#
function getErrorHeader(r::Recognizer, e::RecognitionException)
    line = getOffendingToken(e).line
    column = getOffendingToken(e).column
    return "line "+line+":"+column
end

    # How should a token be displayed in an error message? The default
    #  is to display just the text, but during development you might
    #  want to have a lot of information spit out.  Override in that case
    #  to use t.toString() (which, for CommonToken, dumps everything about
    #  the token). This is better than forcing you to override a method in
    #  your token objects because you don't have to go modify your lexer
    #  so that it creates a new Java type.
    #
    # @deprecated This method is not called by the ANTLR 4 Runtime. Specific
    # implementations of {@link ANTLRErrorStrategy} may provide a similar
    # feature when necessary. For example, see
    # {@link DefaultErrorStrategy#getTokenErrorDisplay}.
    #
function getTokenErrorDisplay(::Recognizer, t::Token)
    if isnothing(t)
        return "<no token>"
    end
    s = t.text
    if isnothing(s)
        if t.type==eof(Token)
            s = "<EOF>"
        else
            s = "<" * string(t.type) * ">"
        end
    end
    s = replace(s, "\n","\\n")
    s = replace(s, "\r","\\r")
    s = replace(s, "\t","\\t")
    return "'" * s * "'"
end

function getErrorListenerDispatch(r::AbstractRecognizer)
    return ProxyErrorListener(r._listeners)
end

# subclass needs to override these if there are sempreds or actions
# that the ATN interp needs to execute
function sempred(::Recognizer, ::RuleContext, ::Int, ::Int)
    return true
end

function precpred(::Recognizer, ::RuleContext , ::Int)
    return true
end

state(r::Recognizer) = r.stateNumber

# Indicate that the recognizer has changed internal state that is
#  consistent with the ATN state passed in.  This way we always know
#  where we are in the ATN as the parser goes along. The rule
#  context objects form a stack that lets us see the stack of
#  invoking rules. Combine this and we have complete ATN
#  configuration information.

function state!(r::Recognizer, atnState::Int)
    r.stateNumber = atnState
end


function Base.getproperty(recog::Recognizer, field::Symbol)
    return getfield(recog, field)
end

function Base.setproperty!(recog::Recognizer, field::Symbol, value::S) where {S}
    setfield!(recog, field, value)
end

function Base.getproperty(recog::T, field::Symbol) where {T<:AbstractRecognizer}
    if hasfield(T, field)
        return getfield(recog, field)
    else
        return getfield(getfield(recog, :_recog), field)
    end
end
function Base.setproperty!(recog::T, field::Symbol, value::S) where {T<:AbstractRecognizer, S}
    if hasfield(T, field)
        return setfield!(recog, field, value)
    elseif hasfield(Recognizer, field)
        return setfield!(getfield(recog, :_recog), field, value)
    end
    error("type Recognizer has no field $field")
end