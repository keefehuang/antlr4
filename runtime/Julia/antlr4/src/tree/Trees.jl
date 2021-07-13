#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#
function toStringTree(t::Tree, ruleNames::Vector{String}= [], recog::nAbstractParser=nothing)
    if !isnothing(recog)
        ruleNames = recog.rulesNames
    end
    s = escapeWhitespace(getNodeText(cls, t, ruleNames), false)
    if t.getChildCount == 0
        return s
    end
    buf = IOBuffer()
    buf.write("(")
    buf.write(s)
    buf.write(' ')
    for i in 0:t.getChildCount()
        if i > 0
            buf.write(' ')
        end
        buf.write(toStringTree(cls, getChild(t, i), ruleNames))
    end
    buf.write(")")
    return buf.data()
end

function getNodeText(t::Tree, ruleNames::Vector{String}=[], recog::nAbstractParser=nothing)
    if !isnothing(recog)
        ruleNames = recog.ruleNames
    end
    if !isempty(ruleNames)
        if t isa RuleNode
            if getAltNumber(t)!=0 # should use ATN.INVALID_ALT_NUMBER but won't compile
                return ruleNames[getRuleIndex(t)] * ":" * string(getAltNumber(t))
            end
            return ruleNames[t.getRuleIndex()]
        elseif t isa ErrorNode
            return string(t)
        elseif t isa TerminalNode
            if !isnothing(t.symbol)
                return t.symbol.text
            end
        end
    end
    # no recog for rule names
    payload = getPayLoad(t)
    if payload isa Token
        return payload.text
    end
    return string(getPayload(t))
end

# Return ordered list of all children of this node
function getChildren(t::Tree)
    return [getChild(t, i) for i in 1:getChildCount(t)]
end

function getAncestors(t::Tree)
    ancestors = Tree[]
    t = getParent(t)
    while !isnothing(t)
        prepend!(ancestors, t) # insert at start
        t = getParent(t)
    end
    return ancestors
end

function findAllTokenNodes(t::ParseTree, ttype::Int)
    return findAllNodes(t, ttype, true)
end

function findAllRuleNodes(t::ParseTree, ruleIndex::Int)
    return findAllNodes(t, ruleIndex, false)
end

function findAllNodes(t::ParseTree, index::Int, findTokens::Bool)
    nodes = []
    _findAllNodes(t, index, findTokens, nodes)
    return nodes
end

function _findAllNodes(t::ParseTree, index::Int, findTokens::Bool, nodes::Vector{Tree})
    # check this node (the root) first
    if findTokens && t isa TerminalNode
        if t.symbol.type == index
            push!(nodes, t)
        end
    elseif  !findTokens && t isa ParserRuleContext
        if t.ruleIndex == index
            push!(nodes, t)
        end
    end
    # check children
    for i in 1:getChildCount(t)
        _findAllNodes(getChild(t, i), index, findTokens, nodes)
    end
end


function descendants(t::ParseTree)
    nodes = [t]
    for i in 1:getChildCount(t)
        append!(nodes, descendants(getChild(t, i)))
    end
    return nodes
end