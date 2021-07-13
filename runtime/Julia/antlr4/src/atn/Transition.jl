#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

#  An ATN transition between any two ATN states.  Subclasses define
#  atom, set, epsilon, action, predicate, rule transitions.
#
#  <p>This is a one way link.  It emanates from a state (usually via a list of
#  transitions) and has a target state.</p>
#
#  <p>Since we never have to change the ATN transitions once we construct it,
#  we can fix these transitions as specific classes. The DFA transitions
#  on the other hand need to update the labels as it adds transitions to
#  the states. We'll use the term Edge for the DFA to distinguish them from
#  ATN transitions.</p>
#
# from antlr4.IntervalSet import IntervalSet
# from antlr4.Token import Token

# # need forward declarations
# from antlr4.atn.SemanticContext import Predicate, PrecedencePredicate

using SuperEnum

@se TransitionConstant EPSILON RANGE RULE PREDICATE ATOM ACTION SET NOT_SET WILDCARD PRECEDENCE

transitionSerializationNames = [
    "INVALID",
    "EPSILON",
    "RANGE",
    "RULE",
    "PREDICATE",
    "ATOM",
    "ACTION",
    "SET",
    "NOT_SET",
    "WILDCARD",
    "PRECEDENCE"
]

abstract type Transition end
abstract type AbstractPredicateTransition <: Transition end

struct AtomTransition <: Transition 
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    label_::Int
    serializationType::TransitionConstant.TransitionConstantEnum

    function AtomTransition(target::ATNState, label::Int)
        label_ = IntervalSet()
        addOne!(label_, label)
        new(target, false, label_, label, TransitionConstant.ATOM)
    end
end

function makeLabel(tr::AtomTransition)
    s = IntervalSet()
    addOne!(s, tr.label_)
    return s
end

function matches(tr::AtomTransition, symbol::Int, minVocabSymbol::Int, maxVocabSymbol::Int)
    return tr.label_ == symbol
end

function Base.string(tr::AtomTransition)
    return string(tr.label_)
end

struct RuleTransition <: Transition 
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    ruleIndex::Int
    precedence::Int
    followState::ATNState
    serializationType::TransitionConstant.TransitionConstantEnum

    function RuleTransition(target::ATNState, ruleIndex::Int, precedence::Int, followState::ATNState)
        new(target, true, nothing, ruleIndex, precedence, followState, TransitionConstant.RULE)
    end
end

matches(::RuleTransition, ::Int, ::Int, ::Int) = false

function Base.string(tr::RuleTransition)
    return "RuleTransition"
end

struct EpsilonTransition <: Transition
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    serializationType::TransitionConstant.TransitionConstantEnum
    outermostPrecedenceReturn::Int

    function EpsilonTransition(target::ATNState, outermostPrecedenceReturn::Int=-1)
        new(target, true, nothing, TransitionConstant.EPSILON, outermostPrecedenceReturn)
    end
end

matches(::EpsilonTransition, ::Int, ::Int, ::Int) = false

Base.string(::EpsilonTransition) = "epsilon"

struct RangeTransition <: Transition
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    serializationType::TransitionConstant.TransitionConstantEnum
    start::Int
    stop::Int

    function RangeTransition(target::ATNState, start::Int, stop::Int)
        s = IntervalSet()
        addRange!(s, start:stop)
        new(target, false, s, TransitionConstant.EPSILON, start, stop)
    end
end    

function makeLabel(tr::RangeTransition)
    s = IntervalSet()
    addRange!(s, tr.start:tr.stop)
    return s
end

function matches(tr::RangeTransition, symbol::Int, minVocabSymbol::Int,  maxVocabSymbol::Int)
    return symbol >= tr.start && symbol <= tr.stop
end

function Base.string(tr::RangeTransition)
    return "'" * string(tr.start) * "'..'" * string(tr.stop) * "'"
end

struct PredicateTransition <: AbstractPredicateTransition
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    serializationType::TransitionConstant.TransitionConstantEnum
    ruleIndex::Int
    predIndex::Int
    isCtxDependent::Bool
    
    function PredicateTransition(target::ATNState, ruleIndex::Int, predIndex::Int, isCtxDependent::Bool)
        new(target, false, nothing, TransitionConstant.PREDICATE, ruleIndex, predIndex, isCtxDependent)
    end
end

function matches(::PredicateTransition, ::Int, ::Int, ::Int)
    return false
end

function getPredicate(tr::PredicateTransition)
    return Predicate(tr.ruleIndex, tr.predIndex, tr.isCtxDependent)
end

function Base.string(tr::PredicateTransition)
    return "pred_" * string(tr.ruleIndex) * ":" * string(tr.predIndex)
end

struct ActionTransition <: Transition
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    serializationType::TransitionConstant.TransitionConstantEnum
    ruleIndex::Int
    actionIndex::Int
    isCtxDependent::Bool
    function ActionTransition(target::ATNState, ruleIndex::Int, actionIndex::Int=-1, isCtxDependent::Bool=false)
        new(target, true, nothing, TransitionConstant.ACTION, ruleIndex, actionIndex, isCtxDependent)
    end
end

matches(tr::ActionTransition, ::Int, ::Int, ::Int) = false

function Base.string(tr::ActionTransition)
    return "action_" * string(tr.ruleIndex) * ":" * string(tr.actionIndex)
end

# A transition containing a set of values.
struct SetTransition <: Transition
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    serializationType::TransitionConstant.TransitionConstantEnum

    function SetTransition(target::ATNState, set::IntervalSet)
        new(target, false, set, TransitionConstant.SET)
    end

    function SetTransition(target::ATNState, ::Nothing)
        set = IntervalSet()
        addRange!(set, TOKEN_INVALID_TYPE:TOKEN_INVALID_TYPE+1)
        new(target, false, set, TransitionConstant.SET)
    end
end

function matches( tr::SetTransition, symbol::Int, ::Int,  ::Int)
    return symbol in tr.label
end

function Base.string(tr::SetTransition)
    return string(tr.label)
end

struct NotSetTransition <: Transition
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    serializationType::TransitionConstant.TransitionConstantEnum

    function NotSetTransition(target::ATNState, set::IntervalSet)
        new(target, false, set, TransitionConstant.NOT_SET)
    end
end

function matches(tr::NotSetTransition, symbol::Int, minVocabSymbol::Int,  maxVocabSymbol::Int)
    return symbol >= minVocabSymbol && symbol <= maxVocabSymbol && !(symbol in tr.label)
end

function Base.string(tr::NotSetTransition)
    return "~" * string(tr.label)
end


struct WildcardTransition <: Transition
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    serializationType::TransitionConstant.TransitionConstantEnum

    function WildcardTransition(target::ATNState)
        new(target, false, nothing, TransitionConstant.WILDCARD)
    end
end

function matches(::WildcardTransition, symbol::Int, minVocabSymbol::Int, maxVocabSymbol::Int)
    return symbol >= minVocabSymbol && symbol <= maxVocabSymbol
end

Base.string(::WildcardTransition) = "."

struct PrecedencePredicateTransition <: AbstractPredicateTransition
    target::ATNState
    isEpsilon::Bool
    label::Union{IntervalSet,Nothing}
    serializationType::TransitionConstant.TransitionConstantEnum
    precedence::Int

    function PrecedencePredicateTransition(target::ATNState, precedence::Int)
        new(target, true, nothing, TransitionConstant.PRECEDENCE, precedence)
    end
end

matches(::PrecedencePredicateTransition, ::Int, ::Int, ::Int) = false

getPredicate(tr::PrecedencePredicateTransition) = PrecedencePredicate(tr.precedence)

Base.string(tr::PrecedencePredicateTransition) = tr.precedence * " >= _p"

serializationTypes = Dict(
    EpsilonTransition=> TransitionConstant.EPSILON,
             RangeTransition=> TransitionConstant.RANGE,
             RuleTransition=> TransitionConstant.RULE,
             PredicateTransition=> TransitionConstant.PREDICATE,
             AtomTransition=> TransitionConstant.ATOM,
             ActionTransition=> TransitionConstant.ACTION,
             SetTransition=> TransitionConstant.SET,
             NotSetTransition=> TransitionConstant.NOT_SET,
             WildcardTransition=> TransitionConstant.WILDCARD,
             PrecedencePredicateTransition=> TransitionConstant.PRECEDENCE
)

function Base.show(io::IO, ::MIME"text/plain", t::T) where {T<:Transition}
    println(io, string(t))
end