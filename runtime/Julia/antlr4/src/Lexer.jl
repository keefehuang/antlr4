# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/

# A lexer is recognizer that draws input symbols from a character stream.
#  lexer grammars result in a subclass of lexer object. A Lexer object
#  uses simplified match() and error recovery mechanisms in the interest
#  of speed.
#/
using SuperEnum
MIN_CHAR_VALUE = 0x0000
MAX_CHAR_VALUE = 0x10FFFF

LEXER_DEFAULT_MODE = 1

@se LEXER_ENUM begin
    MORE = -2
    SKIP = -3
end
mutable struct Lexer <: AbstractLexer
    _recog::Recognizer
    _input::InputStream
    _output::Union{IO,IOBuffer}
    _factory::CommonTokenFactory
    _tokenFactorySourcePair::Tuple{TokenSource,InputStream}
    _token::nToken
    _tokenStartCharIndex::Int
    _tokenStartLine::Int
    _tokenStartColumn::Int
    _hitEOF::Bool
    _channel::Int
    _type::Int
    _modeStack::Vector{Int}
    _mode::Int
    _text::nString

    function Lexer(input::InputStream, output::Union{IO,IOBuffer}=Base.stdout)
        lexer_init(new(), input, output)
    end
end

function lexer_init(l::Lexer, input::InputStream, output::Union{IO,IOBuffer}=Base.stdout)
    l._recog = Recognizer()
    l._input = input
    l._output = output
    l._factory = default(CommonTokenFactory)
    l._tokenFactorySourcePair = (l, input)
    l._token = nothing
    l._tokenStartCharIndex = -1
    l._tokenStartLine = -1
    l._tokenStartColumn = -1
    l._hitEOF = false
    l._channel = TOKEN_DEFAULT_CHANNEL
    l._type = TOKEN_INVALID_TYPE
    l._modeStack = Int[]
    l._mode = LEXER_DEFAULT_MODE
    l._text = nothing
    return l
end

function reset!(lexer::Lexer)
            # wack Lexer state variables
    if !isnothing(lexer._input)
        lexer._input.seek(0) # rewind the input
    end
    lexer._token = nothing
    lexer._type = TOKEN_INVALID_TYPE
    lexer._channel = TOKEN_DEFAULT_CHANNEL
    lexer._tokenStartCharIndex = -1
    lexer._tokenStartColumn = -1
    lexer._tokenStartLine = -1
    lexer._text = nothing

    lexer._hitEOF = false
    lexer._mode = LEXER_DEFAULT_MODE
    lexer._modeStack = []

    reset!(lexer._interp)
end

# Return a token from lexer source; i.e., match a token on the char
#  stream.

# Instruct the lexer to skip creating a token for current lexer rule
#  and look for another token.  nextToken() knows to keep looking when
#  a lexer rule finishes with token set to SKIP_TOKEN.  Recall that
#  if token==null at end of any token rule, it creates one for you
#  and emits it.
#/
function skip(lexer::AbstractLexer)
    lexer._type = Int(LEXER_ENUM.SKIP)
end

function more(lexer::AbstractLexer)
    lexer._type = Int(LEXER_ENUM.MORE)
end

function nextToken(lexer::Lexer)
    if isnothing(lexer._input)
        IllegalStateException("nextToken requires a non-null input stream.")
    end

    # Mark start location in char stream so unbuffered streams are
    # guaranteed at least have text of current token
    tokenStartMarker = mark(lexer._input)
    try
        while true
            
            if lexer._hitEOF
                emitEOF(lexer)
                return lexer._token
            end
            lexer._token = nothing
            lexer._channel = TOKEN_DEFAULT_CHANNEL
            lexer._tokenStartCharIndex = lexer._input._index
            lexer._tokenStartColumn = lexer._interp.column
            lexer._tokenStartLine = lexer._interp.line
            lexer._text = nothing
            continueOuter = false
            while true
                lexer._type = TOKEN_INVALID_TYPE
                ttype = LEXER_ENUM.SKIP

                try
                    ttype = match(lexer._interp, lexer._input, lexer._mode)
                catch e
                    notifyListeners(lexer, e)		# report error                    
                    recover(lexer, e)
                end
                if LA(lexer._input, 1) == TOKEN_EOF
                    lexer._hitEOF = true
                end
                if lexer._type == TOKEN_INVALID_TYPE
                    lexer._type = ttype
                end
                if lexer._type == LEXER_ENUM.SKIP
                    continueOuter = true
                    break
                end
                if lexer._type != LEXER_ENUM.MORE
                    break
                end
            end

            if continueOuter
                continue
            end
            if isnothing(lexer._token)
                emit(lexer)
            end
            return lexer._token
        end
    finally
        # make sure we release marker after match or
        # unbuffered char stream will keep buffering
        release!(lexer._input, tokenStartMarker)
    end
end

function nextToken(lexer::T) where {T <: AbstractLexer}
    nextToken(lexer._lexer)
end

function pushMode(lexer::Lexer, m::Int)
    if lexer._interp.debug
        open(lexer._output, "w") do io 
            write(io, "pushMode " * string(m))
        end
    end
    append!(lexer._modeStack, lexer._mode)
    mode(lexer, m)
end

function popMode(lexer::Lexer)
    if length(lexer._modeStack)==0
        error("Empty Stack")
    end
    if lexer._interp.debug
        open(lexer._output, "w") do io
            write(io, "popMode back to " * lexer._modeStack[:end-1])
        end
    end
    mode(lexer, pop!(lexer._modeStack))
    return lexer._mode
end

# Set the char stream and reset the lexer#/
function inputStream(lexer::Lexer)
    return lexer._input
end

function inputStream!(lexer::Lexer, input::InputStream)
    lexer._input = nothing
    lexer._tokenFactorySourcePair = (lexer, lexer._input)
    reset!(lexer)
    lexer._input = input
    lexer._tokenFactorySourcePair = (lexer, lexer._input)
end

function sourceName(lexer::Lexer)
    return lexer._input.sourceName
end

# By default does not support multiple emits per nextToken invocation
#  for efficiency reasons.  Subclass and override lexer method, nextToken,
#  and getToken (to push tokens into a list and pull from that list
#  rather than a single variable as lexer implementation does).
#/
function emitToken!(lexer::Lexer, token::T) where {T<:AbstractToken}
    lexer._token = token
end


# The standard method called to automatically emit a token at the
#  outermost lexical rule.  The token object should point into the
#  char buffer start..stop.  If there is a text override in 'text',
#  use that to set the token's text.  Override lexer method to emit
#  custom Token objects or provide a new factory.
#/
function emit(lexer::Lexer)
    t = create(lexer._factory, lexer._tokenFactorySourcePair, lexer._type, lexer._text, lexer._channel, lexer._tokenStartCharIndex,
                                getCharIndex(lexer)-1, lexer._tokenStartLine, lexer._tokenStartColumn)
    emitToken!(lexer, t)
    return t
end

function emitEOF(lexer::Lexer)
    cpos = lexer._interp.column
    lpos = lexer._interp.line
    eof = create(lexer._factory, lexer._tokenFactorySourcePair, TOKEN_EOF, nothing, TOKEN_DEFAULT_CHANNEL, lexer._input._index,
                                lexer._input._index-1, lpos, cpos)
    emitToken!(lexer, eof)
    return eof
end

type(lexer::Lexer) = lexer._type
function type!(lexer::Lexer, type::Int)
    lexer._type = type
end

getLine(lexer::Lexer) = lexer._interp.line
function setLine!(lexer::Lexer, line::Int)
    lexer._interp.line = line
end

getColumn(lexer::Lexer) = lexer._interp.column
function setColumn!(lexer::Lexer, column::Int)
    lexer._interp.column = column
end

# What is the index of the current character of lookahead?#/
function getCharIndex(lexer)
    return lexer._input._index
end

# Return the text matched so far for the current token or any
#  text override.
function text(lexer::Lexer)
    if !isnothing(lexer._text)
        return lexer._text
    end
    return getText(lexer._interp, lexer._input)
end

# Set the complete text of lexer token; it wipes any previous
#  changes to the text.
function text!(lexer::Lexer, txt::String)
    lexer._text = txt
end

# Return a list of all Token objects in input char stream.
#  Forces load of all tokens. Does not include EOF token.
#/
function getAllTokens(lexer::Lexer)
    tokens = Token[]
    t = nextToken(lexer)
    while t.type != TOKEN_EOF
        append!(tokens, t)
        t = nextToken(lexer)
    end
    return tokens
end

function notifyListeners(lexer::Lexer, e::Exception)
    start = lexer._tokenStartCharIndex
    stop = lexer._input._index
    text = getText(lexer._input, start, stop)
    msg = "token recognition error at: '" * getErrorDisplay(lexer, text) * "'"
    listener = getErrorListenerDispatch(lexer)
    syntaxError(listener, lexer, nothing, lexer._tokenStartLine, lexer._tokenStartColumn, msg, e)
end

function getErrorDisplay(lexer::Lexer, s::String)
    buf = IOBuffer()
    for c in s
        write(buf, getErrorDisplayForChar(lexer, c))
    end
    return string(take!(buf))
end

function getErrorDisplayForChar(::Lexer, c::Char)
    if ord(c[1])==TOKEN_EOF
        return "<EOF>"
    elseif c == '\n'
        return "\\n"
    elseif c == '\t'
        return "\\t"
    elseif c == '\r'
        return "\\r"
    end
    return c
end

function getCharErrorDisplay(lexer::Lexer, c::String)
    return "'" * getErrorDisplayForChar(lexer, c) * "'"
end

# Lexers can normally match any char in it's vocabulary after matching
#  a token, so do the easy thing and just kill a character and hope
#  it all works out.  You can instead use the rule invocation stack
#  to do sophisticated error recovery if you are in a fragment rule.
#/
function recover(lexer::AbstractLexer, re::RecognitionException)
    if LA(lexer._input, 1) != TOKEN_EOF
        if re isa LexerNoViableAltException
                # skip a char and try again
                consume(lexer._interp, lexer._input)
        else
            # TODO: Do we lose character or line position information?
            consume(lexer._input)
        end
    end
end

function recover(::AbstractLexer, e::Exception) 
    throw(e)
end

function Base.getproperty(lexer::Lexer, field::Symbol)
    if hasfield(Lexer, field)
        return getfield(lexer, field)
    elseif hasfield(Recognizer, field)
        return getfield(getfield(lexer, :_recog), field)
    end
    error("type Lexer has no field $field")
end

function Base.setproperty!(lexer::Lexer, field::Symbol, value::S) where {S}
    if hasfield(Lexer, field)
        return setfield!(lexer, field, value)
    elseif hasfield(Recognizer, field)
        return setfield!(getfield(lexer, :_recog), field, value)
    end
    error("type Lexer has no field $field")
end

function Base.getproperty(lexer::T, field::Symbol) where {T<:AbstractLexer}
    if hasfield(T, field)
        return getfield(lexer, field)
    else
        return getfield(getfield(lexer, :_lexer), field)
    end
    getproperty(lexer._lexer, field)
end

function Base.setproperty!(lexer::T, field::Symbol, value::S) where {T<:AbstractLexer, S}
    if hasfield(T, field)
        return setfield!(lexer, field, value)
    elseif hasfield(Lexer, field)
        return setfield!(getfield(lexer, :_lexer), field, value)
    end
    Base.setproperty!(lexer._lexer, field, value)
end