# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

# A token has properties: text, type, line, character position in the line
# (so we can ignore tabs), token channel, index, and source from which
# we obtained this token.


abstract type AbstractToken end

mutable struct Token <: AbstractToken
    source::Tuple
    type::nInt
    channel::nInt
    start::nInt
    stop::nInt
    tokenIndex::nInt
    line::nInt
    column::nInt
    _text::nString

    function Token()
        new(nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing)
    end

    function Token(source::Tuple, type::nInt, channel::nInt, start::nInt, stop::nInt, tokenIndex::nInt, line::nInt, column::nInt, text::nString)
        new(source, type, channel, start, stop, tokenIndex, line, column, text)
    end
end

getTokenSource(t::Token) = t.source[1]
getInputStream(t::Token) = t.source[2]
text(t::Token) = t._text

function text!(t::Token, s::String)
    t._text = s
end

TOKEN_INVALID_TYPE = 0

# During lookahead operations, this "token" signifies we hit rule end ATN state
# and did not follow it despite needing to.
TOKEN_EPSILON = -2
MIN_USER_TOKEN_TYPE = 1
TOKEN_EOF = -1

# All tokens go to the parser (unless skip() is called in that rule)
# on a particular "channel".  The parser tunes to a particular channel
# so that whitespace etc... can go to the parser on a "hidden" channel.
TOKEN_DEFAULT_CHANNEL = 0
TOKEN_HIDDEN_CHANNEL = 1


mutable struct CommonToken <: AbstractToken
    _token::Token
    # An empty {@link Pair} which is used as the default value of
    # {@link #source} for tokens that do not have a source.
    function CommonToken(;source::Tuple=(nothing, nothing), type::nInt=nothing, channel::Integer=default_channel(Token), start::Integer=1, stop::Integer=1)
        line = nothing
        column = nothing
        if !isnothing(source[1])
            line = getLine(source[1])
            column = getColumn(source[1])
        else
            column = -1
        end
        t = new(Token(source, type, channel, start, stop, -1, line, column, nothing))
        t._text = text(t)
        return t
    end

    function CommonToken(type::Integer, channel::Integer = default_channel(Token), start::Integer=1, stop::Integer=1)
        line = nothing
        column = nothing
        
        new(Token((nothing, nothing), type, channel, start, stop, -1, line, column, nothing))
    end

    function CommonToken(t::Token)
        new(Token(t.source, t.type, t.channel, t.start, t.stop, t.tokenIndex, t.line, t.column, t._text))
    end
end

function Base.getproperty(token::T, field::Symbol) where {T<:AbstractToken}
    if hasfield(T, field)
        return getfield(token, field)
    else
        return getfield(getfield(token, :_token), field)
    end
end

function Base.setproperty!(token::T, field::Symbol, value::S) where {T<:AbstractToken, S}
    if hasfield(T, field)
        return setfield!(token, field, value)
    elseif hasfield(Token, field)
        return setfield!(getfield(token, :_token), field, value)
    end
    error("type Token has no field $field")
end

getTokenSource(t::CommonToken) = getTokenSource(t._token)
getInputStream(t::CommonToken) = getInputStream(t._token)

function text(t::CommonToken) 
    if !isnothing(text(t._token))
        return text(t._token)
    end
    input = getInputStream(t)
    if isnothing(input)
        return nothing
    end
    n = size(input)
    if t._token.start < n && t._token.stop < n
        return getText(input, t._token.start, t._token.stop)
    else
        return "<EOF>"
    end
end

text!(t::CommonToken, s::String) = text!(t._token, s)


# Constructs a new {@link CommonToken} as a copy of another {@link Token}.
#
# <p>
# If {@code oldToken} is also a {@link CommonToken} instance, the newly
# constructed token will share a reference to the {@link #text} field and
# the {@link Pair} stored in {@link #source}. Otherwise, {@link #text} will
# be assigned the result of calling {@link #getText}, and {@link #source}
# will be constructed from the result of {@link Token#getTokenSource} and
# {@link Token#getInputStream}.</p>
#
# @param oldToken The token to copy.
function clone(t::CommonToken)
    t_new = CommonToken(t._token)
    return t_new
end

function Base.show(io::IO, t::CommonToken)
    t = t._token
    write(io, "[@")
    write(io, string(t.tokenIndex))
    write(io, ",")
    write(io, string(t.start))
    write(io, ":")
    write(io, string(t.stop))
    write(io, "='")
    txt = t._text
    if !isnothing(txt)
        txt = replace(txt, "\n"=>"\\n")
        txt = replace(txt, "\r"=>"\\r")
        txt = replace(txt, "\t"=>"\\t")
    else
        txt = "<no text>"
    end
    write(io, txt)        
    write(io, "',<")
    write(io, string(t.type))
    write(io, ">")
    if t.channel > 0
        write(io, ",channel=")
        write(io, string(t.channel))
    end
    write(io, ",")
    write(io, string(t.line))
    write(io, ":")
    write(io, string(t.column))
    write(io, "]")

end

function Base.string(t::CommonToken)
    t = t._token
    io = IOBuffer()
    write(io, "[@")
    write(io, string(t.tokenIndex))
    write(io, ",")
    write(io, string(t.start))
    write(io, ":")
    write(io, string(t.stop))
    write(io, "='")
    txt = t._text
    if !isnothing(txt)
        txt = replace(txt, "\n"=>"\\n")
        txt = replace(txt, "\r"=>"\\r")
        txt = replace(txt, "\t"=>"\\t")
    else
        txt = "<no text>"
    end
    write(io, txt)
    write(io, "',<")
    write(io, string(t.type))
    write(io, ">")
    if t.channel > 0
        write(io, ",channel=")
        write(io, string(t.channel))
    end
    write(io, ",")
    write(io, string(t.line))
    write(io, ":")
    write(io, string(t.column))
    write(io, "]")
    return String(take!(io))
end

const nToken = Union{AbstractToken,Nothing}
