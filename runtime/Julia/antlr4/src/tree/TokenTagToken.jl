#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

#
# A {@link Token} object representing a token of a particular type; e.g.,
# {@code <ID>}. These tokens are created for {@link TagChunk} chunks where the
# tag corresponds to a lexer rule or token type.
#
struct TokenTagToken <: AbstractToken
    ctoken::CommonToken
    tokenName::nString
    label::nString

    function TokenTagToken(tokenName::String, type::Int, label::nString)
        if isnothing(label)
            ttext = "<" * tokenName * ">"
        else
            ttext = "<" * self.label * ":" * self.tokenName * ">"
        end
        ct = CommonToken(type=type)
        text!(ct, ttext)
        new(CommonToken(ct, tokenName, label))
    end
end

function getText(ttt::TokenTagToken)
    if isnothing(ttt.label)
        return "<" * ttt.tokenName * ">"
    end
    return "<" * ttt.label * ":" * ttt.tokenName * ">"
end

function Base.string(ttt::TokenTagToken)
    return ttt.tokenName * ":" * string(ttt.type)
end