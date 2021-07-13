#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/

# A DFA walker that knows how to dump them to serialized strings.#/
struct DFASerializer
    dfa::DFA
    literalNames::Vector{String}
    symbolicNames::Vector{String}
    function DFASerializer(dfa::DFA; literalNames::Vector{String}=String[], symbolicNames::Vector{String}=String[])
        new(dfa, literalNames, symbolicNames)
    end
end

function getEdgeLabel(dfaSerializer::DFASerializer, i::Int)
    if i==0
        return "EOF"
    end
    if !isnothing(dfaSerializer.literalNames) && i <= length(dfaSerializer.literalNames)
        return dfaSerializer.literalNames[i]
    elseif !isnothing(dfaSerializer.symbolicNames) && i<= length(dfaSerializer.symbolicNames)
        return dfaSerializer.symbolicNames[i]
    else
        return string(i)
    end
end

function getStateString(::DFASerializer, s::DFAState)
    n = s.stateNumber
    baseStateStr = (s.isAcceptState ? ":" : "") * "s" * string(n) * ( s.requiresFullContext ? "^" : "")
    if s.isAcceptState
        if !isnothing(s.predicates)
            return baseStateStr * "=>" * join(string.(s.predicates))
        else
            return baseStateStr * "=>" * string(s.prediction)
        end
    else
        return baseStateStr
    end
end

function Base.string(dfaSerializer::DFASerializer)
    if isnothing(dfaSerializer.dfa.s0)
        return nothing
    end

    buf = IOBuffer()
    for s in sortedStates(dfaSerializer.dfa)
        n = 0
        if !isnothing(s.edges)
            n = length(s.edges)
        end
        for i in 1:n
            t = s.edges[i]
            if !isnothing(t) && t.stateNumber != 0x7FFFFFFF
                write(buf, getStateString(dfaSerializer, s))
                label = getEdgeLabel(dfaSerializer, i)
                write(buf, "-")
                write(buf, label)
                write(buf, "->")
                write(buf, getStateString(dfaSerializer, t))
                write(buf, "\n")
            end
        end
    end
    output = string(take!(buf))
    if length(output) == 0
        return nothing
    else
        return output
    end
end

function getEdgeLabel(::Lexer, dfaSerializer::DFASerializer, i::Int)
    return "'" * Char(i) * "'"
end