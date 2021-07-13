#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/

# Map a predicate to a predicted alternative.#/
# from io import StringIO
# from antlr4.atn.ATNConfigSet import ATNConfigSet
# from antlr4.atn.SemanticContext import SemanticContext

struct PredPrediction
    alt::SemanticContext
    pred::Int
    function PredPrediction(pred::SemanticContext, alt::Int)
        new(alt, pred)
    end
end

Base.string(p::PredPrediction) = "(" * string(p.pred) * ", " * string(p.alt) *  ")"

# A DFA state represents a set of possible ATN configurations.
#  As Aho, Sethi, Ullman p. 117 says "The DFA uses its state
#  to keep track of all possible states the ATN can be in after
#  reading each input symbol.  That is to say, after reading
#  input a1a2..an, the DFA is in a state that represents the
#  subset T of the states of the ATN that are reachable from the
#  ATN's start state along some path labeled a1a2..an."
#  In conventional NFA&rarr;DFA conversion, therefore, the subset T
#  would be a bitset representing the set of states the
#  ATN could be in.  We need to track the alt predicted by each
#  state as well, however.  More importantly, we need to maintain
#  a stack of states, tracking the closure operations as they
#  jump from rule to rule, emulating rule invocations (method calls).
#  I have to add a stack to simulate the proper lookahead sequences for
#  the underlying LL grammar from which the ATN was derived.
#
#  <p>I use a set of ATNConfig objects not simple states.  An ATNConfig
#  is both a state (ala normal conversion) and a RuleContext describing
#  the chain of rules (if any) followed to arrive at that state.</p>
#
#  <p>A DFA state may have multiple references to a particular state,
#  but with different ATN contexts (with same or different alts)
#  meaning that state was reached via a different set of rule invocations.</p>
#/

mutable struct DFAState
    stateNumber::Integer
    configs::ATNConfigSet
    edges::Vector
    isAcceptState::Bool
    prediction::Int
    lexerActionExecutor::Any
    requiresFullContext::Bool
    predicates::Vector
    
    function DFAState(;stateNumber::Integer=-1, configs::ATNConfigSet=ATNConfigSet())
        new(stateNumber, configs, [], false, 0, nothing, false, [])
    end
end

const nDFAState = Union{DFAState,Nothing}

function getAltSet(d::DFAState)
    if !isnothing(d.configs)
        return set(cfg.alt for cfg in self.configs)
    end
    return nothing
end

function Base.hash(d::DFAState)
    hash(d.configs)
end

function Base.:(==)(self::DFAState, other::DFAState)
    if self === other
        return true
    end
    return self.configs == other.configs
end

function Base.string(d::DFAState)
    buf = IOBuffer()
    write(buf, string(d.stateNumber))
    write(buf, ":")
    write(buf, string(d.configs))
    if d.isAcceptState
        write(buf, "=>")
        if !isempty(d.predicates)
            write(buf, string(d.predicates))
        else
            write(buf, string(d.prediction))
        end
    end
    return String(take!(buf))
end

function Base.show(io::IO, ::MIME"text/plain", dfaState::DFAState)
    println(io, string(dfaState))
end
