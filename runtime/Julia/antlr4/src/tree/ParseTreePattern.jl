#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

#
# A pattern like {@code <ID> = <expr>;} converted to a {@link ParseTree} by
# {@link ParseTreePatternMatcher#compile(String, int)}.
#
# from antlr4.tree.ParseTreePatternMatcher import ParseTreePatternMatcher
# from antlr4.tree.Tree import ErrorNode, ParseTree
# from antlr4.xpath.XPath import XPath

struct ParseTreePattern  <: AbstractPattern
    matcher::ParseTreePatternMatcher
    patternRuleIndex::Int
    pattern::String
    patternTree::ParseTree

    # Construct a new instance of the {@link ParseTreePattern} class.
    #
    # @param matcher The {@link ParseTreePatternMatcher} which created this
    # tree pattern.
    # @param pattern The tree pattern in concrete syntax form.
    # @param patternRuleIndex The parser rule which serves as the root of the
    # tree pattern.
    # @param patternTree The tree pattern in {@link ParseTree} form.
    #

    function ParseTreePattern(matcher::ParseTreePatternMatcher, pattern::String, patterRuleIndex::Int, patternTree::ParseTree)
        new(matcher, patternRuleIndex, pattern, patternTree)
    end
end


#
# Match a specific parse tree against this tree pattern.
#
# @param tree The parse tree to match against this tree pattern.
# @return A {@link ParseTreeMatch} object describing the result of the
# match operation. The {@link ParseTreeMatch#succeeded()} method can be
# used to determine whether or not the match was successful.
#
function match(ptp::ParseTreePattern, tree::ParseTree)
    return match(ptp.matcher, tree, ptp)
end

#
# Determine whether or not a parse tree matches this tree pattern.
#
# @param tree The parse tree to match against this tree pattern.
# @return {@code true} if {@code tree} is a match for the current tree
# pattern; otherwise, {@code false}.
#
function matches(ptp::ParseTreePattern, tree::ParseTree)
    return succeeded(match(ptp.matcher, tree, ptp))
end

# Find all nodes using XPath and then try to match those subtrees against
# this tree pattern.
#
# @param tree The {@link ParseTree} to match against this pattern.
# @param xpath An expression matching the nodes
#
# @return A collection of {@link ParseTreeMatch} objects describing the
# successful matches. Unsuccessful matches are omitted from the result,
# regardless of the reason for the failure.
#
function findAll(ptp::ParseTreePattern, tree::ParseTree, xpath::String)
    subtrees = findAll(XPath, tree, xpath, ptp.matcher.parser)
    matches = Vector()
    for t in subtrees
        match = match(ptp, t)
        if succeeded(match)
            push!(matches, match)
        end
    end
    return matches
end

