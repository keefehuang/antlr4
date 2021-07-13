#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

#
# A {@link Token} object representing an entire subtree matched by a parser
# rule; e.g., {@code <expr>}. These tokens are created for {@link TagChunk}
# chunks where the tag corresponds to a parser rule.
#
struct RuleTagToken <: AbstractToken
    token::Token
    label::nString
    ruleName::String

    function RuleTagToken(ruleName::String, bypassTokenType::Int, label::nString=nothing)
        if len(ruleName) == 0
            error("ruleName cannot be null or empty")
        end
        if isnothing(label)
            text = "<" * ruleName * ">"
        else
            text = "<" * label * ":" * ruleName * ">"
        end
        new(Token((nothing,nothing), nothing, bypassTokenType, TOKEN_DEFAULT_CHANNEL, -1, -1, 0, -1, text), label, ruleName)
    end
end

function getText(rtt::RuleTagToken)
    if isnothing(rtt.label)
        return "<" * rtt.ruleName * ">"
    end 
    return "<" * rtt.label * ":" * rtt.ruleName * ">"
end