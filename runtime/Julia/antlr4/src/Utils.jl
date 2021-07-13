# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

const nInt = Union{Int,Nothing}
const nString = Union{String,Nothing}
const nBool = Union{Bool,Nothing}
const nSet = Union{Set,Nothing}

abstract type AbstractRuleContext end
abstract type TokenSource end
abstract type AbstractRecognizer <: TokenSource end
abstract type AbstractLexer <: AbstractRecognizer end
abstract type AbstractParser <: AbstractRecognizer end
abstract type AbstractInputStream end
abstract type AbstractListener end



const nAbstractParser = Union{AbstractParser,Nothing}

abstract type AbstractATN end
const nATN = Union{AbstractATN,Nothing}

abstract type ATNState end
abstract type AbstractATNSimulator end
const nATNState = Union{ATNState,Nothing}
abstract type DecisionState <: ATNState end
abstract type BlockStartState <: DecisionState end

abstract type ATNSimulator end
const nATNSimulator = Union{ATNSimulator,Nothing}

default_token_channel(AbstractLexer) = 1


function str_list(val::Vector{T}) where {T}
    buf = IOBuffer()
    buf.write('[')
    first = true
    for item in val
        if !first
            buf.write(", ")
        end
        buf.write(string(item))
        first = false
    end
    buf.write(']')
    return buf.data()
end

function escapeWhitespace(s::String, escapeSpaces::Bool)
    buf = IOBuffer()
    for c in s
        if c==' ' && escapeSpaces
            buf.write('\u00B7')
        elseif c=='\t'
            buf.write("\\t")
        elseif c=='\n'
            buf.write("\\n")
        elseif c=='\r'
            buf.write("\\r")
        else
            buf.write(c)
        end
    end
    return buf.data()
end

ord(c::Char) = Int(codepoint(c))

function Base.:<<(a::Int64, b::T) where {T<:SuperEnum.Enum}
    <<(a, Int(b))
end

Base.:(==)(k::Int, e::T) where {T<:SuperEnum.Enum} = k == Int(e)