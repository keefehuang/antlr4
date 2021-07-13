abstract type Visitor end
abstract type ParseTreeVisitor <: Visitor end

defaultResult(::ParseTreeVisitor) = nothing

function visit(v::T, tn::Tree) where {T<:ParseTreeVisitor}
    return accept(tree, v)
end

function visitChildren(t::T, node::Token) where {T<:ParseTreeVisitor}
    result = defaultResult(t)
    n = getChildCount(node)
    for i in 1:n
        if !shouldVisitNextChild(t, node, result)
            return result
        end

        c = getChild(node, i)
        childResult = accept(c, t)
        result = aggregateResult(t, result, childResult)
    end
end

function visitTerminal(t::T, tn::Tree) where {T<:ParseTreeVisitor}
    return defaultResult(t)
end

function visitErrorNode(t::T, node) where {T<:ParseTreeVisitor}
    return defaultResult(t)
end

function aggregateResult(::T, aggregate, nextResult) where {T<:ParseTreeVisitor}
    return nextResult
end

function shouldVisitNextChild(::T, node, currentResult) where {T<:ParseTreeVisitor}
    return true
end

