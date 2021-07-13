#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

#
# This default implementation of {@link TokenFactory} creates
# {@link CommonToken} objects.
#
abstract type TokenFactory end

struct CommonTokenFactory <: TokenFactory
    copyText::Bool

        # Indicates whether {@link CommonToken#setText} should be called after
        # constructing tokens to explicitly set the text. This is useful for cases
        # where the input stream might not be able to provide arbitrary substrings
        # of text from the input after the lexer creates a token (e.g. the
        # implementation of {@link CharStream#getText} in
        # {@link UnbufferedCharStream} throws an
        # {@link UnsupportedOperationException}). Explicitly setting the token text
        # allows {@link Token#getText} to be called at any time regardless of the
        # input stream implementation.
        #
        # <p>
        # The default value is {@code false} to avoid the performance and memory
        # overhead of copying text for every token unless explicitly requested.</p>
    function CommonTokenFactory(copyText::Bool = false)
        new(copyText)
    end
end

function create(ctf::CommonTokenFactory, source, type::nInt, text::nString, channel::Int, start::Int, stop::Int, line::Int, column::Int)
    t = CommonToken(source=source, type=type, channel=channel, start=start, stop=stop)
    t.line = line
    t.column = column
    if !isnothing(text)
        t.text = text
    elseif ctf.copyText && !isnothing(source[2])
        t.text = getText(source[2], start, stop)
    end
    return t
end

function createThin(::CommonTokenFactory, type::Int, text::String)
    t = CommonToken(type=type)
    t.text = text
    return t
end

default(::CommonTokenFactory) = nothing
default(::Type{CommonTokenFactory}) = CommonTokenFactory()
