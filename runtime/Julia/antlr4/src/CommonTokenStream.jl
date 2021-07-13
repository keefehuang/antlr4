#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/

#
# This class extends {@link BufferedTokenStream} with functionality to filter
# token streams to tokens on a particular channel (tokens where
# {@link Token#getChannel} returns a particular value).
#
# <p>
# This token stream provides access to all tokens by index or when calling
# methods like {@link #getText}. The channel filtering is only used for code
# accessing tokens via the lookahead methods {@link #LA}, {@link #LT}, and
# {@link #LB}.</p>
#
# <p>
# By default, tokens are placed on the default channel
# ({@link Token#DEFAULT_CHANNEL}), but may be reassigned by using the
# {@code ->channel(HIDDEN)} lexer command, or by using an embedded action to
# call {@link Lexer#setChannel}.
# </p>
#
# <p>
# Note: lexer rules which use the {@code ->skip} lexer command or call
# {@link Lexer#skip} do not produce tokens at all, so input text matched by
# such a rule will not be available as part of the token stream, regardless of
# channel.</p>
#/

struct CommonTokenStream <: TokenStream
    channel::nInt
    bts::BufferedTokenStream
    function CommonTokenStream(lexer::AbstractLexer, channel::Int=TOKEN_DEFAULT_CHANNEL)
        new(channel, BufferedTokenStream(lexer))
    end
end

function seek!(ts::CommonTokenStream, index::Int)
    seek!(ts.bts, index)
end

function adjustSeekIndex(ts::CommonTokenStream, i::Int)
    nextTokenOnChannel(ts.bts, i, ts.channel)
end

function LB(ts::CommonTokenStream, k::Int)::nToken
    bts = ts.bts
    if k==0 || (bts.index-k) < 0
        return nothing
    end
    i = bts.index
    n = 1
    # find k good tokens looking backwards
    while n <= k
        # skip off-channel tokens
        i = previousTokenOnChannel(bts, i - 1, ts.channel)
        n += 1
    end
    if i < 0
        return nothing
    end
    return ts.bts.tokens[i+1]
end

function LT(ts::CommonTokenStream, k::Int)
    lazyInit(ts.bts)
    if k == 0
        return nothing
    end

    if k < 0
        return LB(ts, -k)
    end
    i = ts.bts.index
    n = 1 # we know tokens[pos] is a good one
    # find k good tokens
    while n < k
        # skip off-channel tokens, but make sure to not look past EOF
        if sync(ts, i + 1)
            i = nextTokenOnChannel(bts, i + 1, ts.channel)
            n += 1
        end
    end
    return ts.bts.tokens[i+1]
end

# Count EOF just once.#/
function getNumberOfOnChannelTokens(ts::CommonTokenStream)
    bts = ts.bts
    n = 0
    fill(bts)
    for i in 1:len(ts.tokens)
        t = ts.tokens[i]
        if t.channel==ts.channel
            n += 1
        end
        if t.type==eof(Token)
            break
        end
    end
    return n
end

consume(ts::CommonTokenStream) = consume(ts.bts)