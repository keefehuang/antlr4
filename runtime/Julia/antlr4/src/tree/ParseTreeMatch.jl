#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#


#
# Represents the result of matching a {@link ParseTree} against a tree pattern.
#
# from io import StringIO
# from antlr4.tree.ParseTreePattern import ParseTreePattern
# from antlr4.tree.Tree import ParseTree

#
# Constructs a new instance of {@link ParseTreeMatch} from the specified
# parse tree and pattern.
#
# @param tree The parse tree to match against the pattern.
# @param pattern The parse tree pattern.
# @param labels A mapping from label names to collections of
# {@link ParseTree} objects located by the tree pattern matching process.
# @param mismatchedNode The first node which failed to match the tree
# pattern during the matching process.
#
# @exception IllegalArgumentException if {@code tree} is {@code null}
# @exception IllegalArgumentException if {@code pattern} is {@code null}
# @exception IllegalArgumentException if {@code labels} is {@code null}
#
struct ParseTreeMatch
    tree::ParseTree
    pattern::ParseTreePattern
    labels::Dict{String,ParseTree}
    mismatchedNode::ParseTree
    function ParseTreeMatch(tree::ParseTree, pattern::ParseTreePattern, labels::Dict, mismatchedNode::ParseTree)
        new(tree, pattern, labels, mismatchedNode)
    end
end

#
# Get the last node associated with a specific {@code label}.
#
# <p>For example, for pattern {@code <id:ID>}, {@code get("id")} returns the
# node matched for that {@code ID}. If more than one node
# matched the specified label, only the last is returned. If there is
# no node associated with the label, this returns {@code null}.</p>
#
# <p>Pattern tags like {@code <ID>} and {@code <expr>} without labels are
# considered to be labeled with {@code ID} and {@code expr}, respectively.</p>
#
# @param label The label to check.
#
# @return The last {@link ParseTree} to match a tag with the specified
# label, or {@code null} if no parse tree matched a tag with the label.
#
function Base.get(ptm::ParseTreeMatch, label::String)
    parseTrees = get(ptm.labels, label, nothing)
    if isnothing(parseTrees) && length(parseTrees)==0
        return nothing
    else
        return parseTrees[length(parseTrees)]
    end
end

#
# Return all nodes matching a rule or token tag with the specified label.
#
# <p>If the {@code label} is the name of a parser rule or token in the
# grammar, the resulting list will contain both the parse trees matching
# rule or tags explicitly labeled with the label and the complete set of
# parse trees matching the labeled and unlabeled tags in the pattern for
# the parser rule or token. For example, if {@code label} is {@code "foo"},
# the result will contain <em>all</em> of the following.</p>
#
# <ul>
# <li>Parse tree nodes matching tags of the form {@code <foo:anyRuleName>} and
# {@code <foo:AnyTokenName>}.</li>
# <li>Parse tree nodes matching tags of the form {@code <anyLabel:foo>}.</li>
# <li>Parse tree nodes matching tags of the form {@code <foo>}.</li>
# </ul>
#
# @param label The label.
#
# @return A collection of all {@link ParseTree} nodes matching tags with
# the specified {@code label}. If no nodes matched the label, an empty list
# is returned.
#
function getAll(ptm::ParseTreeMatch, label::String)
    nodes = get(ptm.labels, label, nothing)
    if isnothing(nodes)
        return list()
    else
        return nodes
    end
end


#
# Gets a value indicating whether the match operation succeeded.
#
# @return {@code true} if the match operation succeeded; otherwise,
# {@code false}.
#
function succeeded(ptm::Union{ParseTreeMatch,Nothing})
    return isnothing(ptm.mismatchedNode)
end

#
# {@inheritDoc}
#
function Base.string(ptm::ParseTreeMatch)
    buf = IOBuffer()
    write(buf, "Match ")
    write(buf, succeeded(ptm) ? "succeeded" : "failed")
    write(buf, "; found ")
    write(buf, string(length(ptm.labels)))
    write(buf, " labels")
    return buf.data()
end
