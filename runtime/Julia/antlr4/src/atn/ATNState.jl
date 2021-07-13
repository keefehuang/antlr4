#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

# The following images show the relation of states and
# {@link ATNState#transitions} for various grammar conmutable structs.
#
# <ul>
#
# <li>Solid edges marked with an &#0949; indicate a required
# {@link EpsilonTransition}.</li>
#
# <li>Dashed edges indicate locations where any transition derived from
# {@link Transition} might appear.</li>
#
# <li>Dashed nodes are place holders for either a sequence of linked
# {@link BasicState} states or the inclusion of a block representing a nested
# conmutable struct in one of the forms below.</li>
#
# <li>Nodes showing multiple outgoing alternatives with a {@code ...} support
# any number of alternatives (one or more). Nodes without the {@code ...} only
# support the exact number of alternatives shown in the diagram.</li>
#
# </ul>
#
# <h2>Basic Blocks</h2>
#
# <h3>Rule</h3>
#
# <embed src="images/Rule.svg" type="image/svg+xml"/>
#
# <h3>Block of 1 or more alternatives</h3>
#
# <embed src="images/Block.svg" type="image/svg+xml"/>
#
# <h2>Greedy Loops</h2>
#
# <h3>Greedy Closure: {@code (...)*}</h3>
#
# <embed src="images/ClosureGreedy.svg" type="image/svg+xml"/>
#
# <h3>Greedy Positive Closure: {@code (...)+}</h3>
#
# <embed src="images/PositiveClosureGreedy.svg" type="image/svg+xml"/>
#
# <h3>Greedy Optional: {@code (...)?}</h3>
#
# <embed src="images/OptionalGreedy.svg" type="image/svg+xml"/>
#
# <h2>Non-Greedy Loops</h2>
#
# <h3>Non-Greedy Closure: {@code (...)*?}</h3>
#
# <embed src="images/ClosureNonGreedy.svg" type="image/svg+xml"/>
#
# <h3>Non-Greedy Positive Closure: {@code (...)+?}</h3>
#
# <embed src="images/PositiveClosureNonGreedy.svg" type="image/svg+xml"/>
#
# <h3>Non-Greedy Optional: {@code (...)??}</h3>
#
# <embed src="images/OptionalNonGreedy.svg" type="image/svg+xml"/>
#


INITIAL_NUM_TRANSITIONS = 4

@se ATNStateType INVALID_TYPE BASIC RULE_START BLOCK_START PLUS_BLOCK_START STAR_BLOCK_START TOKEN_START RULE_STOP BLOCK_END STAR_LOOP_BACK STAR_LOOP_ENTRY PLUS_LOOP_BACK LOOP_END
const nATNStateType = Union{ATNStateType.ATNStateTypeEnum,Nothing}
ATNserializationNames = [
        "INVALID",
        "BASIC",
        "RULE_START",
        "BLOCK_START",
        "PLUS_BLOCK_START",
        "STAR_BLOCK_START",
        "TOKEN_START",
        "RULE_STOP",
        "BLOCK_END",
        "STAR_LOOP_BACK",
        "STAR_LOOP_ENTRY",
        "PLUS_LOOP_BACK",
        "LOOP_END" ]

INVALID_STATE_NUMBER = -1

mutable struct ATNStateVariables
    atn::nATN
    stateNumber::Int
    stateType::nATNStateType
    ruleIndex::Int
    epsilonOnlyTransitions::Bool
    transitions::Vector{Transition}
    nextTokenWithinRule::nToken

    function ATNStateVariables(;atn::nATN=nothing, stateNumber::Int=-1,
                    stateType::nATNStateType=nothing, ruleIndex::Int=0, epsilonOnlyTransitions::Bool=false,
                    transitions::Vector{Transition}=Transition[], nextTokenWithinRule::nToken=nothing)
        new(atn, stateNumber, stateType, ruleIndex, epsilonOnlyTransitions, transitions, nextTokenWithinRule)
    end

    function ATNStateVariables(stateType::nATNStateType)
        new(nothing, INVALID_STATE_NUMBER, stateType, 0, false, Transition[], nothing)
    end
end

function Base.hash(var::ATNStateVariables)
    return var.stateNumber
end

function Base.:(==)(self::ATNStateVariables, other::ATNStateVariables)
    return self.stateNumber == other.stateNumber
end

function onlyHasEpsilonTransitions(var::ATNStateVariables)
    return var.epsilonOnlyTransitions
end
   
function isNonGreedyExitState(::ATNStateVariables)
    return false
end

function Base.string(var::ATNStateVariables)
    return string(var.atn)
end

function addTransition!(var::ATNStateVariables, trans::Transition, index::Int=-1)
    if length(var.transitions) == 0
        var.epsilonOnlyTransitions = trans.isEpsilon
    elseif var.epsilonOnlyTransitions != trans.isEpsilon
        var.epsilonOnlyTransitions = false
    end

    if index == -1
        push!(var.transitions, trans)
    else
        append!(var.transitions, trans)
    end
end

function addTransition!(atnstate::ATNState, trans::Transition, index::Int=-1)
    addTransition!(atnstate.atnvar, trans, index)
end


function Base.hash(state::ATNState)
    return hash(state.atnvar)
end

function Base.:(==)(self::T, other::T) where {T<:ATNState}
    return self.atnvar == other.atnvar
end

Base.:(==)(::T, ::S) where {T,S<:ATNState} = false

function onlyHasEpsilonTransitions(state::ATNState)
    return onlyHasEpsilonTransitions(state.atnvar)
end

function Base.string(state::ATNState)
    return string(state.atnvar)
end

mutable struct BasicState <: ATNState
    atnvar::ATNStateVariables
    function BasicState()
        new(ATNStateVariables(stateType=ATNStateType.BASIC))
    end
end
    
# Terminal node of a simple {@code (a|b|c)} block.
mutable struct BlockEndState <: ATNState
    atnvar::ATNStateVariables
    startState::nATNState

    function BlockEndState()
        new(ATNStateVariables(stateType=ATNStateType.BLOCK_END), nothing)
    end
end

const nBlockEndState = Union{BlockEndState,Nothing}

mutable struct BasicBlockStartState <: BlockStartState
    atnvar::ATNStateVariables
    decision::Int
    nonGreedy::Bool
    endState::nBlockEndState

    function BasicBlockStartState()
        new(ATNStateVariables(stateType=ATNStateType.BLOCK_START), -1, false, nothing)
    end
end

# The last node in the ATN for a rule, unless that rule is the start symbol.
#  In that case, there is one transition to EOF. Later, we might encode
#  references to all calls to this rule to compute FOLLOW sets for
#  error handling.
#
mutable struct RuleStopState <: ATNState
    atnvar::ATNStateVariables

    function RuleStopState()
        new(ATNStateVariables(stateType=ATNStateType.RULE_STOP))
    end
end

const nRuleStopState = Union{RuleStopState,Nothing}

mutable struct RuleStartState <: ATNState
    atnvar::ATNStateVariables
    stopState::nRuleStopState
    isPrecedenceRule::Bool    

    function RuleStartState()
        new(ATNStateVariables(stateType=ATNStateType.RULE_START), nothing, false)
    end
end

# Decision state for {@code A+} and {@code (A|B)+}.  It has two transitions:
#  one to the loop back to start of the block and one to exit.
#
mutable struct PlusLoopbackState <: DecisionState
    atnvar::ATNStateVariables
    decision::Int
    nonGreedy::Bool

    function PlusLoopbackState()
        new(ATNStateVariables(stateType=ATNStateType.PLUS_LOOP_BACK), -1, false)
    end
end

# Start of {@code (A|B|...)+} loop. Technically a decision state, but
#  we don't use for code generation; somebody might need it, so I'm defining
#  it for completeness. In reality, the {@link PlusLoopbackState} node is the
#  real decision-making note for {@code A+}.
#
mutable struct PlusBlockStartState <: BlockStartState
    atnvar::ATNStateVariables
    decision::Int
    nonGreedy::Bool
    endState::nATNState
    loopBackState::nATNState
    

    function PlusBlockStartState()
        new(ATNStateVariables(stateType=ATNStateType.PLUS_BLOCK_START), -1, false, nothing, nothing)
    end
end

# The block that begins a closure loop.
mutable struct StarBlockStartState <: BlockStartState
    atnvar::ATNStateVariables
    decision::Int
    nonGreedy::Bool
    endState::nATNState

    function StarBlockStartState()
        new(ATNStateVariables(stateType=ATNStateType.STAR_BLOCK_START), -1, false, nothing)
    end
end

# The block that begins a closure loop.
mutable struct StarLoopbackState <: ATNState
    atnvar::ATNStateVariables

    function StarLoopbackState()
        new(ATNStateVariables(stateType=ATNStateType.STAR_LOOP_BACK))
    end
end

mutable struct StarLoopEntryState <: DecisionState
    atnvar::ATNStateVariables
    decision::Int
    nonGreedy::Bool
    loopBackState::nATNState
    isPrecedenceDecision::nBool

    function StarLoopEntryState()
        new(ATNStateVariables(stateType=ATNStateType.STAR_LOOP_ENTRY), -1, false, nothing, nothing)
    end
end

# Mark the end of a * or + loop.
mutable struct LoopEndState <: ATNState
    atnvar::ATNStateVariables
    loopBackState::nATNState
    function LoopEndState()
        new(ATNStateVariables(stateType=ATNStateType.LOOP_END), nothing)
    end
end

# The Tokens rule start state linking to each lexer rule start state */
mutable struct TokensStartState <: DecisionState
    atnvar::ATNStateVariables
    decision::Int
    nonGreedy::Bool

    function TokensStartState()
        new(ATNStateVariables(stateType=ATNStateType.TOKEN_START), -1, false)
    end
end

function Base.getproperty(atnstate::T, field::Symbol) where {T<:ATNState}
    if hasfield(T, field)
        return getfield(atnstate, field)
    else
        return getfield(getfield(atnstate, :atnvar), field)
    end
end

function Base.setproperty!(atnstate::T, field::Symbol, value::S) where {T<:ATNState, S}
    if hasfield(T, field)
        return setfield!(atnstate, field, value)
    elseif hasfield(ATNStateVariables, field)
        return setfield!(getfield(atnstate, :atnvar), field, value)
    end
    error("type ATNState has no field $field")
end

function Base.string(atnvar::ATNStateVariables)
    buf = IOBuffer()
    write(buf, "")
    write(buf, string(atnvar.stateNumber))
    write(buf, ", ")
    write(buf, string(Int(atnvar.stateType)))
    write(buf, ", ")
    write(buf, string(atnvar.ruleIndex))
    write(buf, ", ")
    write(buf, string(atnvar.epsilonOnlyTransitions))
    write(buf, "")
    String(take!(buf))
end

function Base.string(atnstate::T) where {T<:ATNState}
    buf = IOBuffer()
    # write(buf, "[")
    write(buf, string(atnstate.atnvar))
    
    if hasfield(T, :decision)
        write(buf, ", decision: ")
        write(buf, string(atnstate.decision))
    end
    
    if hasfield(T, :nonGreedy)
        write(buf, ", nonGreedy: ")
        write(buf, string(atnstate.nonGreedy))
    end

    if hasfield(T, :isPrecedenceRule)
        write(buf, ", isPrecedence: ")
        write(buf, string(atnstate.isPrecedenceRule))
    end
    # write(buf, "]")
    String(take!(buf))
end

function Base.show(io::IO, ::MIME"text/plain", atnState::ATNState)
    println(io, string(atnState))
end