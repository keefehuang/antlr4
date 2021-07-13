#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

# A tree structure used to record the semantic context in which
#  an ATN configuration is valid.  It's either a single predicate,
#  a conjunction {@code p1&&p2}, or a sum of products {@code p1||p2}.
#
#  <p>I have scoped the {@link And}, {@link Or}, and {@link Predicate} subclasses of
#  {@link SemanticContext} within the scope of this outer class.</p>
#

abstract type SemanticContext end
const nSemanticContext = Union{SemanticContext,Nothing}

# The default {@link SemanticContext}, which is semantically equivalent to
# a predicate of the form {@code {true}?}.
#

#
# For context independent predicates, we evaluate them without a local
# context (i.e., null context). That way, we can evaluate them without
# having to create proper rule-specific context during prediction (as
# opposed to the parser, which creates them naturally). In a practical
# sense, this avoids a cast exception from RuleContext to myruleContext.
#
# <p>For context dependent predicates, we must pass in a local context so that
# references such as $arg evaluate properly as _localctx.arg. We only
# capture context dependent predicates in the context in which we begin
# prediction, so we passed in the outer context here in case of context
# dependent predicate evaluation.</p>
#
function evaluate(sc::SemanticContext, parser::Recognizer, outerContext::RuleContext)
end

#
# Evaluate the precedence predicates for the context and reduce the result.
#
# @param parser The parser instance.
# @param outerContext The current parser context object.
# @return The simplified semantic context after precedence predicates are
# evaluated, which will be one of the following values.
# <ul>
# <li>{@link #nothing}: if the predicate simplifies to {@code true} after
# precedence predicates are evaluated.</li>
# <li>{@code null}: if the predicate simplifies to {@code false} after
# precedence predicates are evaluated.</li>
# <li>{@code this}: if the semantic context is not changed as a result of
# precedence predicate evaluation.</li>
# <li>A non-{@code null} {@link SemanticContext}: the new simplified
# semantic context after precedence predicates are evaluated.</li>
# </ul>
#
function evalPrecedence(sc::SemanticContext, ::Recognizer, ::RuleContext)
    return sc
end

struct Predicate <: SemanticContext
    ruleIndex::Int
    predIndex::Int
    isCtxDependent::Bool

    function Predicate(ruleIndex::Int=-1, predIndex::Int=-1, isCtxDependent::Bool=false)
        new(ruleIndex, predIndex, isCtxDependent)
    end
end

function evaluate(sc::Predicate, parser::Recognizer, outerContext::RuleContext)
    if sc.isCtxDependent
        localctx = outerContext
    else
        localctx = nothing
    end
    return sempred(parser, localctx, sc.ruleIndex, sc.predIndex)
end

function Base.hash(sc::Predicate)
    hash((sc.ruleIndex, sc.predIndex, sc.isCtxDependent))
end

function Base.string(sc::Predicate)
    return "{" * string(sc.ruleIndex) * ":" * string(sc.predIndex) * "}?"
end

SEMANTIC_CONTEXT_NONE = Predicate()


struct PrecedencePredicate <: SemanticContext
    precedence::Int

    function PrecedencePredicate(precedence::Int=0)
        new(precedence)
    end
end

function evaluate(sc::PrecedencePredicate, parser::Recognizer, outerContext::RuleContext)
    return precpred(sc, parser, outerContext)
end

function evalPrecedence(sc::PrecedencePredicate, parser::Recognizer, outerContext::RuleContext)
    if precpred(parser, outerContext, sc.precedence)
        return SEMANTIC_CONTEXT_NONE
    else
        return nothing
    end
end

function Base.:(<)(self::PrecedencePredicate, other::PrecedencePredicate)
    return self.precedence < other.precedence
end
   
Base.hash(sc::PrecedencePredicate) = 31

function filterPrecedencePredicates(collection::Set{SemanticContext})
    return filter(x-> x isa PrecedencePredicate, collect)
end

struct And <: SemanticContext
    operands::Vector{SemanticContext}

    function And(a::SemanticContext, b::SemanticContext)
        operands = Set{SemanticContext}()
        if a isa And
            union!(operands, a.operands)
        else
            union!(operands, a)
        end
        if b isa And
            union!(operands, b.operands)
        else
            union!(operands, b)
        end
        precedencePredicates = filterPrecedencePredicates(operands)
        if length(precedencePredicates) > 0
            reduced = minimum(precedencePredicates)
            union(operands, reduced)
        end
        new([operands...])
    end
end

function Base.:(==)(self::T, other::T) where {T <: SemanticContext}
    if self === other
        return true
    end 
    return self.operands == other.operands
end
Base.:(==)(self::T, other::R) where {T,R <: SemanticContext} = false

function Base.hash(a::And)
    h = 0
    for o in a.operands
        h = hash((h, o))
    end
    return hash((h, "And"))
end

#
# {@inheritDoc}
#
# <p>
# The evaluation of predicates by this context is short-circuiting, but
# unordered.</p>
#
function evaluate(a::And, parser::Recognizer, outerContext::RuleContext)
    return all(evaluate.(a.operands, (parser,), (outerContext,)))
end

function evalPrecedence(a::And, parser::Recognizer, outerContext::RuleContext)
    differs = false
    operands = []
    for context in a.operands
        evaluated = evalPrecedence(context, parser, outerContext)
        differs = evaluated != context || differs
        if isnothing(evaluated)
            # The And context is false if any element is false
            return nothing
        elseif !isnothing(evaluated)
            # Reduce the result by skipping true elements
            operands.append(evaluated)
        end
    end

    if !differs
        return a
    end

    if length(operands) == 0
        # all elements were true, so the And context is true
        return nothing
    end

    result = nothing
    for o in operands
        if isnothing(result)
            result = o
        else
            andContext(result, o)
        end
    end

    return result
end

function Base.string(a::And)
    buf = IOBuffer
    first = true
    for o in a.operands
        if !first
            write(buf, "&&")
        end
        write(buf, string(o))
        first = false
    end
    return buf.data()
end


#
# A semantic context which is true whenever at least one of the contained
# contexts is true.
struct Or <: SemanticContext
    operands::Vector{SemanticContext}
    
    function Or(a::SemanticContext, b::SemanticContext)
        operands = Set{SemanticContext}()
        if a isa Or
            union!(operands, a.operands)
        else
            union!(operands, a)
        end
        if b isa Or
            union!(operands, b.operands)
        else
            union!(operands, b)
        end
        precedencePredicates = filterPrecedencePredicates(operands)
        if length(precedencePredicates) > 0
            s = sorted(precedencePredicates)
            reduced = s[end]
            union(operands, reduced)
        end
        new([operands...])
    end
end

function Base.hash(sc::Or)
    h = 0
    for o in sc.operands
        h = hash((h, o))
    end
    return hash((h, "Or"))
end

# <p>
# The evaluation of predicates by this context is short-circuiting, but
# unordered.</p>
#
function evaluate(sc::Or, parser::Recognizer, outerContext::RuleContext)
    return any(opnd.eval(parser, outerContext) for opnd in self.opnds)
end

function evalPrecedence(sc::Or, parser::Recognizer, outerContext::RuleContext)
    differs = false
    operands = []
    for context in sc.operands
        evaluated = evalPrecedence(context, parser, outerContext)
        differs = !(evaluated === context) || differs
        if evaluated == SEMANTIC_CONTEXT_NONE
            # The Or context is true if any element is true
            return SEMANTIC_CONTEXT_NONE
        elseif !isnothing(evaluated)
            # Reduce the result by skipping false elements
            append!(operands, evaluated)
        end
    end

    if !differs
        return sc
    end

    if length(operands) == 0
        # all elements were false, so the Or context is false
        return nothing
    end

    result = nothing
    for o in operands
        if isnothing(result)
            result = o
        else
            orContext(result, o)
        end
    end

    return result
end

function Base.string(sc::Or)
    buf = IOBuffer()
    first = true
    for o in sc.operands
        if !first
            write(buf, "||")
        end
        write(buf, string(o))
        first = false
    end
    return buf.data()
end

function andContext(a::SemanticContext, b::SemanticContext)
    if isnothing(a)
        return b
    end
    if isnothing(b)
        return a
    end
    if a == SEMANTIC_CONTEXT_NONE || b == SEMANTIC_CONTEXT_NONE
        return SEMANTIC_CONTEXT_NONE
    end
    result = And(a, b)
    if length(result.operands) == 1
        return result.operands[0]
    else
        return result
    end
end

function orContext(a::SemanticContext, b::SemanticContext)
    if isnothing(a)
        return b
    end
    if isnothing(b)
        return a
    end
    if a == SEMANTIC_CONTEXT_NONE || b == SEMANTIC_CONTEXT_NONE
        return SEMANTIC_CONTEXT_NONE
    end
    result = Or(a, b)
    if length(result.operands) == 1
        return result.operands[0]
    else
        return result
    end
end
