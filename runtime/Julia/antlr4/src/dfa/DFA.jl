#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.

mutable struct DFA
    # From which ATN state did we create this DFA?
    atnStartState::DecisionState 
    decision::Int
    # A set of all DFA states. Use {@link Map} so we can get old state back
    #  ({@link Set} only allows you to see if it's there).
    _states::Dict
    s0::nDFAState
    # {@code true} if this DFA is for a precedence decision; otherwise,
    # {@code false}. This is the backing field for {@link #isPrecedenceDfa},
    # {@link #setPrecedenceDfa}.
    precedenceDfa::Bool

    function DFA(atnStartState::DecisionState, decision::Int=0)
        precedenceDfa = false
        precedenceState = nothing
        if atnStartState isa StarLoopEntryState
            if !isnothing(atnStartState.isPrecedenceDecision) && atnStartState.isPrecedenceDecision
                precedenceDfa = true
                precedenceState = DFAState(configs=ATNConfigSet())
                precedenceState.edges = []
                precedenceState.isAcceptState = false
                precedenceState.requiresFullContext = false
            end
        end
        new(atnStartState, decision, Dict(), precedenceState, precedenceDfa)
    end
end

const nDFA = Union{DFA,Nothing}

# Get the start state for a specific precedence value.
#
# @param precedence The current precedence.
# @return The start state corresponding to the specified precedence, or
# {@code null} if no start state exists for the specified precedence.
#
# @throws IllegalStateException if this is not a precedence DFA.
# @see #isPrecedenceDfa()
function getPrecedenceStartState(dfa::DFA, precedence::Int)
    if !dfa.precedenceDfa
        IllegalStateException("Only precedence DFAs may contain a precedence start state.")
    end

    # s0.edges is never null for a precedence DFA
    if precedence < 0 || precedence >= length(self.s0.edges)
        return nothing
    end
    return dfa.s0.edges[precedence]
end

# Set the start state for a specific precedence value.
#
# @param precedence The current precedence.
# @param startState The start state corresponding to the specified
# precedence.
#
# @throws IllegalStateException if this is not a precedence DFA.
# @see #isPrecedenceDfa()
#
function setPrecedenceStartState!(dfa::DFA, precedence::Int, startState::DFAState)
    if !dfa.precedenceDfa
        IllegalStateException("Only precedence DFAs may contain a precedence start state.")
    end

    if precedence < 0
        return
    end
    # synchronization on s0 here is ok. when the DFA is turned into a
    # precedence DFA, s0 will be initialized once and not updated again
    # s0.edges is never null for a precedence DFA
    if precedence > length(self.s0.edges)
        ext = Vector{Nothing}(nothing, precedence + 1 - length(self.s0.edges))
        append!(dfa.s0.edges, ext)
    end
    self.s0.edges[precedence] = startState
    return
end
    
#
# Sets whether this is a precedence DFA. If the specified value differs
# from the current DFA configuration, the following actions are taken;
# otherwise no changes are made to the current DFA.
#
# <ul>
# <li>The {@link #states} map is cleared</li>
# <li>If {@code precedenceDfa} is {@code false}, the initial state
# {@link #s0} is set to {@code null}; otherwise, it is initialized to a new
# {@link DFAState} with an empty outgoing {@link DFAState#edges} array to
# store the start states for individual precedence values.</li>
# <li>The {@link #precedenceDfa} field is updated</li>
# </ul>
#
# @param precedenceDfa {@code true} if this is a precedence DFA; otherwise,
# {@code false}

function setPrecedenceDfa!(dfa::DFA, precedenceDfa::Bool)
    if dfa.precedenceDfa != precedenceDfa
        dfa._states = Dict()
        if precedenceDfa
            precedenceState = DFAState(configs=ATNConfigSet())
            precedenceState.edges = []
            precedenceState.isAcceptState = false
            precedenceState.requiresFullContext = false
            dfa.s0 = precedenceState
        else
            self.s0 = nothing
        end
        dfa.precedenceDfa = precedenceDfa
    end
end

# Return a list of all states in this DFA, ordered by state number.
function sortedStates(dfa::DFA)
    return getindex.(sort(collect(dfa._states), by=x->x[1]; lt=(x,y)->(x.stateNumber<y.stateNumber)), 2)
end