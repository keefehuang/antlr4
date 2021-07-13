abstract type ParseTreeListener end

function visitTerminal(::T, node::TerminalNode) where {T<:ParseTreeListener}
end

function visitErrorNode(::T, node::ErrorNode) where {T<:ParseTreeListener}
end

function enterEveryRule(::T, ctx::ParserRuleContext) where {T<:ParseTreeListener}
end

function exitEveryRule(::T, ctx::ParserRuleContext) where {T<:ParseTreeListener}
end

