function Base.string(dfa::DFA)
    return toString(dfa)
end

function toString(dfa::DFA, literalNames::Vector{String}=String[], symbolicNames::Vector{String}=String[])
    if isnothing(dfa.s0)
        return ""
    end
    return string(DFASerializer(dfa; literalNames, symbolicNames))
end

function toLexerString(dfa::DFA)
    if isnothing(dfa.s0)
        return ""
    end
    return string(DFASerializer(dfa))
end