#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.

# This implementation of {@link TokenStream} loads tokens from a
# {@link TokenSource} on-demand, and places the tokens in a buffer to provide
# access to any previous token by index.
#
# <p>
# This token stream ignores the value of {@link Token#getChannel}. If your
# parser requires the token stream filter tokens to only those on a particular
# channel, such as {@link Token#DEFAULT_CHANNEL} or
# {@link Token#HIDDEN_CHANNEL}, use a filtering token stream such a
# {@link CommonTokenStream}.</p>

abstract type TokenStream end
const nTokenStream = Union{TokenStream,Nothing}

mutable struct BufferedTokenStream <: TokenStream
    tokenSource::AbstractLexer
    tokens::Vector{nToken}
    index::nInt
    fetchedEOF::Bool

    function BufferedTokenStream(tokenSource::AbstractLexer)
        # The {@link TokenSource} from which tokens for this stream are fetched.

        # A collection of all tokens fetched from the token source. The list is
        # considered a complete view of the input once {@link #fetchedEOF} is set
        # to {@code true}.

        # The index into {@link #tokens} of the current token (next token to
        # {@link #consume}). {@link #tokens}{@code [}{@link #p}{@code ]} should be
        # {@link #LT LT(1)}.
        #
        # <p>This field is set to -1 when the stream is first constructed || when
        # {@link #setTokenSource} is called, indicating that the first token has
        # not yet been fetched from the token source. For additional information,
        # see the documentation of {@link IntStream} for a description of
        # Initializing Methods.</p>

        # Indicates whether the {@link Token#EOF} token has been fetched from
        # {@link #tokenSource} and added to {@link #tokens}. This field improves
        # performance for the following cases:
        #
        # <ul>
        # <li>{@link #consume}: The lookahead check in {@link #consume} to prevent
        # consuming the EOF symbol is optimized by checking the values of
        # {@link #fetchedEOF} and {@link #p} instead of calling {@link #LA}.</li>
        # <li>{@link #fetch}: The check to prevent adding multiple EOF symbols into
        # {@link #tokens} is trivial with this field.</li>
        # <ul>
        new(tokenSource, Token[], -1, false)
    end
end

mark(ts::BufferedTokenStream) = 0

function release(::BufferedTokenStream, ::Int)
    return
end

function reset(ts::BufferedTokenStream)
    seek!(ts, 1)
end

function seek!(ts::BufferedTokenStream, index::Int)
    lazyInit(ts)
    ts.index = adjustSeekIndex(ts, index)
    return
end

function Base.get(ts::BufferedTokenStream, index::Int)
    lazyInit(ts)
    ts.tokens[index]
end

function consume(ts::BufferedTokenStream)
    skipEofCheck = false
    if ts.index >= 1
        if ts.fetchedEOF
            # the last token in tokens is EOF. skip check if p indexes any
            # fetched token except the last.
            skipEofCheck = ts.index < length(ts.tokens) - 1
        else
           # no EOF token in tokens. skip check if p indexes a fetched token.
            skipEofCheck = ts.index < length(ts.tokens)
        end
    else
        # not yet initialized
        skipEofCheck = false
    end

    if !skipEofCheck && LA(ts, 1) == TOKEN_EOF
        IllegalStateException("cannot consume EOF")
    end

    if sync!(ts, ts.index + 1)
        ts.index = adjustSeekIndex(ts, ts.index + 1)
    end
end

# Make sure index {@code i} in tokens has a token.
#
# @return {@code true} if a token is located at index {@code i}, otherwise
#    {@code false}.
# @see #get(int i)
#/
function sync!(ts::BufferedTokenStream, i::Int)
    n = i - length(ts.tokens) + 1 # how many more elements we need?
    if n > 0 
        fetched = fetch(ts, n)
        return fetched >= n
    end
    return false
end

# Add {@code n} elements to buffer.
#
# @return The actual number of elements added to the buffer.
#/
function fetch(ts::BufferedTokenStream, n::Int)
    if ts.fetchedEOF
        return 0
    end
    for i in 1:n
        t = nextToken(ts.tokenSource)
        t.tokenIndex = length(ts.tokens)
        push!(ts.tokens, t)
        if typeof(t) == TOKEN_EOF
            ts.fetchedEOF = true
            return i + 1
        end
    end
    return n
end

# Get all tokens from start..stop inclusively#/
function getTokens(ts::BufferedTokenStream, start::Int, stop::Int, types::nSet=nothing)
    if start<1 || stop<1
        return nothing
    end
    lazyInit(ts)
    subset = CommonToken[]
    if stop >= length(ts.tokens)
        stop = length(ts.tokens)
    end
    for i in start:stop
        t = ts.tokens[i]
        if t.type==TOKEN_EOF
            break
        end
        if isnothing(types) || t.type in types
            push!(subset, t)
        end
    end
    return subset
end

function LT(ts::BufferedTokenStream, k::Int)
    lazyInit(ts)
    if k==0
        return nothing
    end
    if k < 0
        return LB(ts, -k)
    end
    i = ts.index + k - 1
    sync!(ts, i)
    if i > length(ts.tokens) # return EOF token
        # EOF must be last token
        return ts.tokens[end]
    end
    return ts.tokens[i+1]
end

function LA(ts::T, i::Int) where {T<:TokenStream}
    return LT(ts, i).type
end

function LB(ts::T, k::Int) where {T<:TokenStream}
    if (ts.index-k) < 1
        return nothing
    end
    return ts.tokens[ts.index-k+1]
end

# Allowed derived classes to modify the behavior of operations which change
# the current stream position by adjusting the target token index of a seek
# operation. The default implementation simply returns {@code i}. If an
# exception is thrown in this method, the current stream index should not be
# changed.
#
# <p>For example, {@link CommonTokenStream} overrides this method to ensure that
# the seek target is always an on-channel token.</p>
#
# @param i The target token index.
# @return The adjusted target token index.

function adjustSeekIndex(::BufferedTokenStream, i::Int)
    i
end

function lazyInit(ts::BufferedTokenStream)
    if ts.index == -1
        setup!(ts)
    end
end

function setup!(ts::BufferedTokenStream)
    sync!(ts, 0)
    ts.index = adjustSeekIndex(ts, 0)
end

# Reset this token stream by setting its token source.#/
function setTokenSource!(ts::BufferedTokenStream, tokenSource::AbstractLexer)
    ts.tokenSource = tokenSource
    ts.tokens = Token[]
    ts.index = -1
    ts.fetchedEOF = False
end


# Given a starting index, return the index of the next token on channel.
#  Return i if tokens[i] is on channel.  Return -1 if there are no tokens
#  on channel between i and EOF.
#/
function nextTokenOnChannel(ts::BufferedTokenStream, i::Int, channel::Int)
    sync!(ts, i)
    if i>=length(ts.tokens)
        return -1
    end
    token = ts.tokens[i]
    while token.channel != channel
        if token.type == eof(Token)
            return -1
        end
        i += 1
        sync!(ts, i)
        token = ts.tokens[i]
    end
    return i
end

# Given a starting index, return the index of the previous token on channel.
#  Return i if tokens[i] is on channel. Return -1 if there are no tokens
#  on channel between i and 0.
function previousTokenOnChannel(ts::BufferedTokenStream, i::Int, channel::Int)
    while i>0 && ts.tokens[i].channel!=channel
        i -= 1
    end
    return i
end

# Collect all tokens on specified channel to the right of
#  the current token up until we see a token on DEFAULT_TOKEN_CHANNEL or
#  EOF. If channel is -1, find any non default channel token.
function getHiddenTokensToRight(ts::BufferedTokenStream, tokenIndex::Int, channel::Int=-1)
    lazyInit(ts)
    if tokenIndex<0 || tokenIndex>length(ts.tokens)
        error(string(tokenIndex) * " not in 1.." * string(length(self.tokens)))
    end
    
    nextOnChannel = nextTokenOnChannel(ts, tokenIndex + 1, default_token_channel(Lexer))
    from_ = tokenIndex+1
    # if none onchannel to right, nextOnChannel=-1 so set to = last token
    to = nextOnChannel
    if nextOnChannel==-1
        to = (length(self.tokens))  
    end

    return filterForChannel(ts, from_, to, channel)
end

# Collect all tokens on specified channel to the left of
#  the current token up until we see a token on DEFAULT_TOKEN_CHANNEL.
#  If channel is -1, find any non default channel token.
function getHiddenTokensToLeft(ts::BufferedTokenStream, tokenIndex::Int, channel::Int=-1)
    lazyInit(ts)
    if tokenIndex<0 || tokenIndex>length(self.tokens)
        error(string(tokenIndex) * " not in 1.." * str(length(ts.tokens)))
    end
    prevOnChannel = self.previousTokenOnChannel(tokenIndex - 1, default_token_channel(AbstractLexer))
    if prevOnChannel == tokenIndex - 1
        return nothing
    end
    # if none on channel to left, prevOnChannel=-1 then from=0
    from_ = prevOnChannel+1
    to = tokenIndex-1
    return self.filterForChannel(from_, to, channel)
end


function filterForChannel(ts::BufferedTokenStream, left::Int, right::Int, channel::Int)
    hidden = Token[]
    for i in left:right
        t = ts.tokens[i]
        if channel==-1
            if t.channel!= default_token_channel(AbstractLexer)
                hidden.append(t)
            end
        elseif t.channel==channel
                hidden.append(t)
        end
    end
    if length(hidden)==0
        return nothing
    end
    return hidden
end

function getSourceName(ts)
    return getSourceName(ts.tokenSource)
end 

# Get the text of all tokens in this buffer.#/
function getText(ts, start::nInt=nothing, stop::nInt=nothing)
    lazyInit(ts)
    fill(ts)
    if start isa Token
        start = start.tokenIndex
    elseif isnothing(start)
        start = 0
    end
    if stop isa Toke
        stop = stop.tokenIndex
    elseif isnothing(stop) || stop > length(ts.tokens)
        stop = length(ts.tokens)
    end

    if start < 0 || stop < 0 || stop < start
        return ""
    end
    buf = IOBuffer
    for i in start:stop
        t = ts.tokens[i]
        if t.type==eof(Token)
            break
        end
        buf.write(t.text)
    end
    return buf.data()
end


# Get all tokens from lexer until EOF#/
function fill(ts)
    lazyInit(ts)
    while fetch(ts, 1000)==1000
    end
end