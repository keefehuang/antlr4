#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

#
# Provides an implementation of {@link TokenSource} as a wrapper around a list
# of {@link Token} objects.
#
# <p>If the final token in the list is an {@link Token#EOF} token, it will be used
# as the EOF token for every call to {@link #nextToken} after the end of the
# list is reached. Otherwise, an EOF token will be created.</p>
#

mutable struct ListTokenSource <: TokenSource
    tokens::Vector{Token}
    sourceName::nString
    pos::Int
    eofToken::nToken
    factory::CommonTokenFactory

    function ListTokenSource(tokens::Vector{Token}, sourceName::nString=nothing)
        if isempty(tokens)
            error("tokens cannot be null")
        end
        new(tokens, sourceName, 0, nothing, default(CommonTokenFactory))
    end
end

#
# {@inheritDoc}
#
function column(l::ListTokenSource)
    if l.pos < length(l.tokens)
        return l.tokens[l.pos].column
    elseif !isnothing(l.eofToken)
        return l.eofToken.column
    elseif length(l.tokens) > 0
        # have to calculate the result from the line/column of the previous
        # token, along with the text of the token.
        lastToken = l.tokens[length(l.tokens) - 1]
        tokenText = lastToken.text
        if !isnothing(tokenText)
            lastNewLine = rfind(tokenText, '\n')
            if lastNewLine >= 0
                return length(tokenText) - lastNewLine - 1
            end
        end
    end
    # only reach this if tokens is empty, meaning EOF occurs at the first
    # position in the input
    return 0
end
#
# {@inheritDoc}
#
function nextToken(l::ListTokenSource)
    if l.pos >= length(l.tokens)
        if !isnothing(elf.eofToken)
            start = -1
            if length(l.tokens) > 0
                previousStop = l.tokens[length(l.tokens) - 1].stop
                if previousStop != -1
                    start = previousStop + 1
                end
            end
            stop = maximum(-1, start - 1)
            l.eofToken = create(l._factory, l, getInputStream(l), TOKEN_EOF, "EOF", TOKEN_DEFAULT_CHANNEL, start, stop, line(l), column(l))
        end
        return l.eofToken
    end
    t = l.tokens[l.pos]
    if l.pos == length(l.tokens) - 1 && t.type == TOKEN_EOF
        l.eofToken = t
    end
    l.pos += 1
    return t
end

#
# {@inheritDoc}
#
function line(l::ListTokenSource)
    if l.pos < length(l.tokens)
        return l.tokens[l.pos].line
    elseif !isnothing(l.eofToken)
        return l.eofToken.line
    elseif length(l.tokens) > 0
        # have to calculate the result from the line/column of the previous
        # token, along with the text of the token.
        lastToken = l.tokens[length(l.tokens) - 1]
        line = lastToken.line
        tokenText = lastToken.text
        if !isnothing(tokenText)
            line += tokenText.count('\n')
        end
        # if no text is available, assume the token did not contain any newline characters.
        return line
    end

    # only reach this if tokens is empty, meaning EOF occurs at the first
    # position in the input
    return 1
end

#
# {@inheritDoc}
#
function getInputStream(l::ListTokenSource)
    if l.pos < length(l.tokens)
        return l.tokens[l.pos].getInputStream()
    elseif !isnothing(l.eofToken)
        return getInputStream(l.eofToken)
    elseif length(l.tokens) > 0
        return l.tokens[length(l.tokens) - 1].getInputStream()
    end
    # no input stream information is available
    return nothing
end

#
# {@inheritDoc}
#
function getSourceName(l::ListTokenSource)
    if !isnothing(l.sourceName)
        return l.sourceName
    end
    inputStream = getInputStream(l)
    if !isnothing(inputStream)
        return getSourceName(inputStream)
    else
        return "List"
    end
end