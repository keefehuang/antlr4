# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.

#* A rule invocation record for parsing.
#
#  Contains all of the information about the current rule not stored in the
#  RuleContext. It handles parse tree children list, Any ATN state
#  tracing, and the default values available for rule indications:
#  start, stop, rule index, current alt number, current
#  ATN state.
#
#  Subclasses made for each rule and grammar track the parameters,
#  return values, locals, and labels specific to that rule. These
#  are the objects that are returned from rules.
#
#  Note text is not an actual field of a rule return value; it is computed
#  from start and stop using the input stream's toString() method.  I
#  could add a ctor to this so that we can pass in and store the input
#  stream, but I'm not sure we want to do that.  It would seem to be undefined
#  to get the .text property anyway if the rule matches tokens from multiple
#  input streams.
#
#  I do not use getters for fields of objects that are used simply to
#  group values such as this aggregate.  The getters/setters are there to
#  satisfy the superclass interface.
using Base.Iterators: filter

abstract type ParserRuleContext <: RuleContext end

const nParserRuleContext = Union{Nothing,ParserRuleContext}
mutable struct ParserRuleContextVars
   rulevars::RuleContextVars
   children::Vector{ParseTree}
   start::nToken
   stop::nToken
   exception::Union{Exception,Nothing}
   function ParserRuleContextVars(parent::Union{ParserRuleContext,Nothing}=nothing, invokingStateNumber::nInt=nothing)
        new(RuleContextVars(parent, invokingStateNumber), ParseTree[], nothing, nothing)
   end
end

function Base.getproperty(parserRuleContext::T, field::Symbol) where {T<:ParserRuleContext}
    if hasfield(T, field)
        return getfield(parserRuleContext, field)
    else
        return Base.getproperty(getfield(parserRuleContext, :parserrulevars), field)
    end
end

function Base.setproperty!(parserRuleContext::T, field::Symbol, value::S) where {S,T<:ParserRuleContext}
    if hasfield(T, field)
        return setfield!(parserRuleContext, field, value)
    elseif hasfield(ParserRuleContextVars, field)
        return Base.setproperty!(getfield(parserRuleContext, :parserrulevars), field, value)
    end
    error("type ParserRuleContext has no field $field")
end

function Base.getproperty(parserRuleVars::ParserRuleContextVars, field::Symbol) 
    if hasfield(ParserRuleContextVars, field)
        return getfield(parserRuleVars, field)
    else
        return getfield(getfield(parserRuleVars, :rulevars), field)
    end
end

function Base.setproperty!(parserRuleVars::ParserRuleContextVars, field::Symbol, value::S) where {S}
    if hasfield(ParserRuleContextVars, field)
        return setfield!(parserRuleVars, field, value)
    elseif hasfield(RuleContextVars, field)
        return setfield!(getfield(parserRuleVars, :rulevars), field, value)
    end
    error("type ParserRuleContextVars has no field $field")
end


const nParserRuleContext = Union{ParserRuleContext,Nothing}

function copyFrom!(pr::ParserRuleContextVars, ctx::ParserRuleContextVars)
    pr.rulecontext = copy(ctx.rulecontext)
    pr.children = ParseTree[]
    pr.start = ctx.start
    pr.stop = ctx.stop

    if !isnothing(ctx.children)
        pr.children = ParseTree[]
        for child in ctx.children
            if child isa ErrorNodeImpl
                append!(pr.children, child)
                child.parentCtx = pr
            end
        end
    end
end

function addChild!(pr::ParserRuleContextVars, child::ParseTree)
    push!(pr.children, child)
    return child
end

function addChild!(pr::T, child::ParseTree) where {T<:ParserRuleContext}
    addChild!(pr.parserrulevars, child)
end

#* Used by enterOuterAlt to toss out a RuleContext previously added as
#  we entered a rule. If we have # label, we will need to remove
#  generic ruleContext object.
#/
function removeLastChild!(pr::ParserRuleContextVars)
    if !isempty(pr.children)
        pop!(pr.children)
    end
end

function removeLastChild!(pr::T) where {T<:ParserRuleContext}
    removeLastChild!(pr.parserrulevars)
end

function addTokenNode!(pr::T, token::S) where {T<:ParserRuleContext,S<:AbstractToken}
    node = TerminalNodeImpl(token)
    node.parentCtx = pr
    addChild!(pr.parserrulevars, node)
    return node
end

function addErrorNode!(pr::T, badToken::Token) where {T<:ParserRuleContext}
    node = ErrorNodeImpl(badToken)
    addChild!(pr.parserrulevars, node)
    node.parentCtx = pr
    return node
end

function getChild(pr::ParserRuleContextVars, i::Int, ttype::Type=Nothing)
    if ttype == Nothing
        return get(pr.children, i, nothing)
    else
        for child in getChildren(pr)
            if !(child isa ttype)
                continue
            end
            if i==0
                return child
            end
            i -= 1
        end
        return nothing
    end
end

getChildren(pr::ParserRuleContextVars, predicate::Function) = filter(pr.children, predicate)

getChildren(pr::ParserRuleContextVars) = pr.children

getChildren(pr::ParserRuleContext) = getChildren(pr.parserrulevars)

function getToken(pr::ParserRuleContextVars, ttype::Int, i::Int)
    for child in getChildren(pr)
        if !(child isa TerminalNode)
            continue
        end
        if child.symbol.type != ttype
            continue
        end
        if i==0
            return child
        end
        i -= 1
    end
    return nothing
end

function getTokens(pr::ParserRuleContextVars, ttype::Int)
    if isempty(getChildren(pr))
        return Token[]
    end
    tokens = Token[]
    for child in getChildren(pr)
        if !(child isa TerminalNode)
            continue
        end
        if child.symbol.type != ttype
            continue
        end
        append!(tokens, child)
    end
    return tokens
end

function getTypedRuleContext(pr::ParserRuleContextVars, ctxType::Type, i::Int)
    return getChild(pr, i, ctxType)
end

function getTypedRuleContexts(pr::ParserRuleContextVars, ctxType::Type)
    children = getChildren(pr)
    if isempty(children)
        return ParseTree[]
    end
    contexts = ParseTree[]
    for child in children
        if !(child isa ctxType)
            continue
        end
        append!(context, child)
    end
    return contexts
end

function getChildCount(pr::ParserRuleContextVars)
    return len(pr.children)
end

function getSourceInterval(pr::ParserRuleContextVars)
    if isnothing(start) || isnothing(stop)
        return INVALID_INTERVAL
    end
    return (pr.start.tokenIndex, pr.stop.tokenIndex)
end

struct InterpreterRuleContext  <: ParserRuleContext 
    parserrulecontext::ParserRuleContextVars
    ruleIndex::Int
    function InterpreterRuleContext(parent::ParserRuleContext, invokingStateNumber::Int, ruleIndex::Int)
        new(ParserRuleContextVars(parent, invokingStateNumber), ruleIndex)
    end
end

