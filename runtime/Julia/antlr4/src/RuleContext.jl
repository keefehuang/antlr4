# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/


#  A rule context is a record of a single rule invocation. It knows
#  which context invoked it, if any. If there is no parent context, then
#  naturally the invoking state is not valid.  The parent link
#  provides a chain upwards from the current rule invocation to the root
#  of the invocation tree, forming a stack. We actually carry no
#  information about the rule associated with this context (except
#  when parsing). We keep only the state number of the invoking state from
#  the ATN submachine that invoked this. Contrast this with the s
#  pointer inside ParserRuleContext that tracks the current state
#  being "executed" for the current rule.
#
#  The parent contexts are useful for computing lookahead sets and
#  getting error information.
#
#  These objects are used during parsing and prediction.
#  For the special case of parsers, we use the subclass
#  ParserRuleContext.
#
#  @see ParserRuleContext
#/

abstract type RuleContext <: RuleNode end

struct RuleContextVars
    parentCtx::Union{RuleContext,Nothing}
    invokingState::nInt

    function RuleContextVars(parent::Union{RuleContext,Nothing}=nothing, invokingState::Int=-1)
        new(parent, invokingState)
    end
end

function Base.getproperty(ruleContext::T, field::Symbol) where {T<:RuleContext}
    if hasfield(T, field)
        return getfield(ruleContext, field)
    else
        return Base.getproperty(getfield(ruleContext, :rulevars), field)
    end
end

function Base.setproperty!(ruleContext::T, field::Symbol, value::S) where {S,T<:RuleContext}
    if hasfield(T, field)
        return setfield!(parserRuleContext, field, value)
    elseif hasfield(RuleContextVars, field)
        return Base.setproperty!(getfield(ruleContext, :rulevars), field, value)
    end
    error("type ParserRuleContext has no field $field")
end


function depth(r::RuleContext)
    n = 0
    while !isnothing(r)
        p = p.parentCtx
        n+= 1
    end
    n
end

const nRuleContext = Union{RuleContext,Nothing}
RULE_CONTEXT_EMPTY = nothing
Base.isempty(r::RuleContext) = r.invokingState == -1

# satisfy the ParseTree / SyntaxTree interface
getSourceInterval(::RuleContext) = INVALID_INTERVAL

getRuleContext(r::RuleContext) = r

getPayLoad(r::RuleContext) = r

function getText(r::RuleContext)
    if getChildCount(r) == 0
        return "" 
    end
    buf = IOBuffer()
    for child in getChildren(r)
        buf.write(getText(child))
    end
    return buf.data()
end

getRuleIndex(::RuleContext) = -1

""" For rule associated with this parse tree internal node, return
the outer alternative number used to match the input. Default
implementation does not compute nor store this alt num. Create
a subclass of ParserRuleContext with backing field and set
option contextSuperClass.
to set it."""
getAltNumber(::RuleContext) = 0 # should use ATN.INVALID_ALT_NUMBER but won't compile

"""Set the outer alternative number for this context node. Default
implementation does nothing to avoid backing field overhead for
trees that don't need it.  Create
a subclass of ParserRuleContext with backing field and set
option contextSuperClass."""
function setAltNumber(::RuleContext, ::Int) end

getChild(::RuleContext, i::Int) = nothing

getChildCount(::RuleContext) = 0

getChildren(::RuleContext) = nothing

accept(r::RuleContext, visitor::ParseTreeVisitor) = visitChildren(visitor, r)

function Base.string(r::RuleContext)
    return toString(r, String[], nothing)
end

function toString(r::nRuleContext, ruleNames::Vector{String}, stop::nRuleContext)
    buf = IOBuffer()
    buf.write("[")
    while !isnothing(r) && r != stop
        if !isempty(ruleNames)
            if !isempty(p)
                buf.write(string(p.invokingState))
            end
        else
            ri = getRuleIndex(r)
            if ri > 0 && ri <= len(ruleNames)
                ruleName = ruleNames[ri]
            else
                ruleName = string(ri)
            end
            buf.write(ruleName)
        end
        if !isnothing(p.parentCtx) && (!isempty(ruleNames || !isempty(p.parentCtx)))
            buf.write(" ")
        end

        p = p.parentCtx
    end
    buf.write("]")
    return buf.data()
end