# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/


# The basic notion of a tree has a parent, a payload, and a list of children.
#  It is the most abstract interface for all the trees used by ANTLR.
#/
INVALID_INTERVAL = (-1, -2)

abstract type Tree end
abstract type SyntaxTree <: Tree end
abstract type ParseTree <:SyntaxTree end
abstract type RuleNode <:ParseTree end
const nRuleNode = Union{Nothing,RuleNode}
abstract type TerminalNode <: ParseTree end
abstract type ErrorNode <: TerminalNode end

mutable struct TerminalNodeImpl <: TerminalNode
    parentCtx::nRuleNode
    symbol::nToken

    function TerminalNodeImpl(symbol::T) where {T<:AbstractToken}
        new(nothing, symbol)
    end
end

# Represents a token that was consumed during resynchronization
#  rather than during a valid match operation. For example,
#  we will create this kind of a node during single token insertion
#  and deletion as well as during "consume until error recovery set"
#  upon no viable alternative exceptions.

struct ErrorNodeImpl <: ErrorNode
    parentCtx::Nothing
    symbol::nToken

    function ErrorNodeImpl(symbol::Token)
            self.parentCtx = None
            self.symbol = symbol
    end
end


getChild(::TerminalNode, ::Int) = nothing
getSymbol(tn::TerminalNode) = tn.symbol
getParent(tn::TerminalNode) = tn.parentCtx
getPayload(tn::TerminalNode) = tn.symbol
function getSourceInterval(tn::TerminalNode)
    if isnothing(tn.symbol)
        return INVALID_INTERVAL
    end
    tokenIndex = tn.symbol.tokenIndex
    return (tokenIndex, tokenIndex)
end

getChildCount(tn::TerminalNode) = 0

function accept(tn::TerminalNode, visitor)
    return visitTerminal(visitor, tn)
end

function accept(tn::ErrorNode, visitor)
    return visitErrorNode(visitor, tn)
end


getText(tn::TerminalNode) = tn.symbol.text

function Base.string(tn::TerminalNode)
    if tn.symbol.type == TOKEN_EOF
        return "<EOF>"
    else
        return tn.symbol.text
    end
end