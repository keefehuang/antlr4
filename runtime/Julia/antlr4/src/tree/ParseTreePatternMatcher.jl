#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

#
# A tree pattern matching mechanism for ANTLR {@link ParseTree}s.
#
# <p>Patterns are strings of source input text with special tags representing
# token or rule references such as:</p>
#
# <p>{@code <ID> = <expr>;}</p>
#
# <p>Given a pattern start rule such as {@code statement}, this object constructs
# a {@link ParseTree} with placeholders for the {@code ID} and {@code expr}
# subtree. Then the {@link #match} routines can compare an actual
# {@link ParseTree} from a parse with this pattern. Tag {@code <ID>} matches
# any {@code ID} token and tag {@code <expr>} references the result of the
# {@code expr} rule (generally an instance of {@code ExprContext}.</p>
#
# <p>Pattern {@code x = 0;} is a similar pattern that matches the same pattern
# except that it requires the identifier to be {@code x} and the expression to
# be {@code 0}.</p>
#
# <p>The {@link #matches} routines return {@code true} or {@code false} based
# upon a match for the tree rooted at the parameter sent in. The
# {@link #match} routines return a {@link ParseTreeMatch} object that
# contains the parse tree, the parse tree pattern, and a map from tag name to
# matched nodes (more below). A subtree that fails to match, returns with
# {@link ParseTreeMatch#mismatchedNode} set to the first tree node that did not
# match.</p>
#
# <p>For efficiency, you can compile a tree pattern in string form to a
# {@link ParseTreePattern} object.</p>
#
# <p>See {@code TestParseTreeMatcher} for lots of examples.
# {@link ParseTreePattern} has two static helper methods:
# {@link ParseTreePattern#findAll} and {@link ParseTreePattern#match} that
# are easy to use but not super efficient because they create new
# {@link ParseTreePatternMatcher} objects each time and have to compile the
# pattern in string form before using it.</p>
#
# <p>The lexer and parser that you pass into the {@link ParseTreePatternMatcher}
# constructor are used to parse the pattern in string form. The lexer converts
# the {@code <ID> = <expr>;} into a sequence of four tokens (assuming lexer
# throws out whitespace or puts it on a hidden channel). Be aware that the
# input stream is reset for the lexer (but not the parser; a
# {@link ParserInterpreter} is created to parse the input.). Any user-defined
# fields you have put into the lexer might get changed when this mechanism asks
# it to scan the pattern string.</p>
#
# <p>Normally a parser does not accept token {@code <expr>} as a valid
# {@code expr} but, from the parser passed in, we create a special version of
# the underlying grammar representation (an {@link ATN}) that allows imaginary
# tokens representing rules ({@code <expr>}) to match entire rules. We call
# these <em>bypass alternatives</em>.</p>
#
# <p>Delimiters are {@code <} and {@code >}, with {@code \} as the escape string
# by default, but you can set them to whatever you want using
# {@link #setDelimiters}. You must escape both start and stop strings
# {@code \<} and {@code \>}.</p>
#
# from antlr4.CommonTokenStream import CommonTokenStream
# from antlr4.InputStream import InputStream
# from antlr4.ParserRuleContext import ParserRuleContext
# from antlr4.Lexer import Lexer
# from antlr4.ListTokenSource import ListTokenSource
# from antlr4.Token import Token
# from antlr4.error.ErrorStrategy import BailErrorStrategy
# from antlr4.error.Errors import RecognitionException, ParseCancellationException
# from antlr4.tree.Chunk import TagChunk, TextChunk
# from antlr4.tree.RuleTagToken import RuleTagToken
# from antlr4.tree.TokenTagToken import TokenTagToken
# from antlr4.tree.Tree import ParseTree, TerminalNode, RuleNode

# need forward declaration
# Parser = nothing
# ParseTreePattern = nothing

abstract type AbstractPattern end

struct CannotInvokeStartRule <: Exception end
struct StartRuleDoesNotConsumeFullPattern <: Exception end

struct ParseTreePatternMatcher 
    lexer::AbstractLexer
    parser::AbstractParser
    start::String
    stop::String
    escape::String
    # Constructs a {@link ParseTreePatternMatcher} or from a {@link Lexer} and
    # {@link Parser} object. The lexer input stream is altered for tokenizing
    # the tree patterns. The parser is used as a convenient mechanism to get
    # the grammar name, plus token, rule names.

    function ParseTreePatternMatcher(lexer::AbstractLexer, parser::AbstractParser)
        new(lexer, parser, "<", ">", "\\")
    end
end

# Set the delimiters used for marking rule and token tags within concrete
# syntax used by the tree pattern parser.
#
# @param start The start delimiter.
# @param stop The stop delimiter.
# @param escapeLeft The escape sequence to use for escaping a start or stop delimiter.
#
# @exception IllegalArgumentException if {@code start} is {@code null} or empty.
# @exception IllegalArgumentException if {@code stop} is {@code null} or empty.
#
function setDelimiters(ptpm::ParseTreePatternMatcher, start::String, stop::String, escapeLeft::String)
    if length(start)==0
        error("start cannot be null or empty")
    end
    if length(stop)==0
        error("stop cannot be null or empty")
    end
    ptpm.start = start
    ptpm.stop = stop
    ptpm.escape = escapeLeft
end

# Does {@code pattern} matched as rule {@code patternRuleIndex} match {@code tree}?#
function matchesRuleIndex(ptpm::ParseTreePatternMatcher, tree::ParseTree, pattern::String, patternRuleIndex::Int)
    p = compileTreePattern(ptpm, pattern, patternRuleIndex)
    return matches(ptpm, tree, p)
end

# Does {@code pattern} matched as rule patternRuleIndex match tree? Pass in a
#  compiled pattern instead of a string representation of a tree pattern.
#
function matchesPattern(ptpm::ParseTreePatternMatcher, tree::ParseTree, pattern::AbstractPattern)
    mismatchedNode = matchImpl(ptpm, tree, pattern.patternTree, Dict())
    return isnothing(mismatchedNode)
end

#
# Compare {@code pattern} matched as rule {@code patternRuleIndex} against
# {@code tree} and return a {@link ParseTreeMatch} object that contains the
# matched elements, or the node at which the match failed.
#
function matchRuleIndex(ptpm::ParseTreePatternMatcher, tree::ParseTree, pattern::String, patternRuleIndex::Int)
    p = compileTreePattern(ptpm, pattern, patternRuleIndex)
    return matchPattern(ptpm, tree, p)
end

#
# Compare {@code pattern} matched against {@code tree} and return a
# {@link ParseTreeMatch} object that contains the matched elements, or the
# node at which the match failed. Pass in a compiled pattern instead of a
# string representation of a tree pattern.
#
function matchPattern(ptpm::ParseTreePatternMatcher, tree::ParseTree, pattern::AbstractPattern)
    labels = Dict()
    mismatchedNode = matchImpl(ptpm, tree, pattern.patternTree, labels)
    return ParseTreeMatch(tree, pattern, labels, mismatchedNode)
end

#
# For repeated use of a tree pattern, compile it to a
# {@link ParseTreePattern} using this method.
#
function compileTreePattern(ptpm::ParseTreePatternMatcher, pattern::String, patternRuleIndex::Int)
    tokenList = tokenize(ptpm, pattern)
    tokenSrc = ListTokenSource(tokenList)
    tokens = CommonTokenStream(tokenSrc)
    parserInterp = ParserInterpreter(ptpm.parser.grammarFileName, ptpm.parser.tokenNames,
                            ptpm.parser.ruleNames, ptpm.parser.getATNWithBypassAlts(),tokens)
    tree = nothing
    try
        setErrorHandler(parserInterp, BailErrorStrategy())
        tree = parse(parserInterp, patternRuleIndex)
    catch e 
        if e isa ParseCancellationException
            throw(e.cause)
        elseif e isa RecognitionException
            throw(e)
        else
            throw(CannotInvokeStartRule())
        end
    end

    # Make sure tree pattern compilation checks for a complete parse
    if tokens.LA(1) != Token.EOF
        throw(StartRuleDoesNotConsumeFullPattern())
    end

    return ParseTreePattern(ptpm, pattern, patternRuleIndex, tree)
end

#
# Recursively walk {@code tree} against {@code patternTree}, filling
# {@code match.}{@link ParseTreeMatch#labels labels}.
#
# @return the first node encountered in {@code tree} which does not match
# a corresponding node in {@code patternTree}, or {@code null} if the match
# was successful. The specific node returned depends on the matching
# algorithm used by the implementation, and may be overridden.
#
function matchImpl(ptpm::ParseTreePatternMatcher, tree::ParseTree, patterntree::ParseTree, labels::Dict)
    if isnothing(tree)
        error("tree cannot be null")
    end
    if isnothing(patternTree)
        error("patternTree cannot be null")
    end

    # x and <ID>, x and y, or x and x; or could be mismatched types
    if tree isa  TerminalNode && patternTree isa  TerminalNode 
        mismatchedNode = nothing
        # both are tokens and they have same type
        if tree.symbol.type == patternTree.symbol.type
            if  patternTree.symbol isa  TokenTagToken  # x and <ID>
                tokenTagToken = patternTree.symbol
                # track label->list-of-nodes for both token name and label (if any)
                map(ptpm, labels, tokenTagToken.tokenName, tree)
                if !isnothing(tokenTagToken.label)
                    map(ptpm, labels, tokenTagToken.label, tree)
                end
            elseif tree.getText()==patternTree.getText()
                # x and x
                pass
            else
                # x and y
                if isnothing(mismatchedNode)
                    mismatchedNode = tree
                end
            end
        else
            if isnothing(mismatchedNode)
                mismatchedNode = tree

            end
        end
        return mismatchedNode
    end

    if tree isa  ParserRuleContext && patternTree isa  ParserRuleContext
        mismatchedNode = nothing
        # (expr ...) and <expr>
        ruleTagToken = getRuleTagToken(ptpm, patternTree)
        if !isnothing(ruleTagToken)
            m = nothing
            if tree.ruleContext.ruleIndex == patternTree.ruleContext.ruleIndex
                # track label->list-of-nodes for both rule name and label (if any)
                map(ptpm, labels, ruleTagToken.ruleName, tree)
                if !isnothing(ruleTagToken.label)
                    map(ptpm, labels, ruleTagToken.label, tree)
                end
            else
                if isnothing(mismatchedNode)
                    mismatchedNode = tree
                end
            end

            return mismatchedNode
        end

        # (expr ...) and (expr ...)
        if getChildCount(tree) != getChildCount(patternTree)
            if isnothing(mismatchedNode)
                mismatchedNode = tree
            end
            return mismatchedNode
        end

        n = tree.getChildCount()
        for i in 1:n
            childMatch = matchImpl(ptpm, tree.getChild(i), patternTree.getChild(i), labels)
            if !isnothing(childMatch)
                return childMatch
            end
        end

        return mismatchedNode
    end

    # if nodes aren't both tokens or both rule nodes, can't match
    return tree
end

function map(ptpm::ParseTreePatternMatcher, labels, label, tree)
    v = get(labels, label, nothing)
    if isnothing(v)
        v = list()
        labels[label] = v
    end
    push!(v, tree)
end

# Is {@code t} {@code (expr <expr>)} subtree?#
function getRuleTagToken(ptpm::ParseTreePatternMatcher, tree::ParseTree)
    if  tree isa  RuleNode 
        if tree.getChildCount()==1 && getChild(tree, 0) isa TerminalNode 
            c = getChild(tree, 0)
            if  c.symbol isa  RuleTagToken 
                return c.symbol
            end
        end
    end
    return nothing
end

function tokenize(ptpm::ParseTreePatternMatcher, pattern::String)
    # split pattern into chunks: sea (raw input) and islands (<ID>, <expr>)
    chunks = split(ptpm, pattern)

    # create token stream from text and tags
    tokens = list()
    for chunk in chunks
        if  chunk isa  TagChunk 
            # add special rule token or conjure up new token from name
            if isuppercase(chunk.tag[1])
                ttype = getTokenType(ptpm.parser, chunk.tag)
                if ttype==Token.INVALID_TYPE
                    error("Unknown token " * string(chunk.tag) * " in pattern: " * pattern)
                end
                tokens.append(TokenTagToken(chunk.tag, ttype, chunk.label))
            elseif islowercase(chunk.tag[1])
                ruleIndex = getRuleIndex(ptpm.parser, chunk.tag)
                if ruleIndex==-1
                    error("Unknown rule " * string(chunk.tag) * " in pattern: " * pattern)
                end
                ruleImaginaryTokenType = getATNWithBypassAlts(ptpm.parser).ruleToTokenType[ruleIndex]
                push!(tokens, RuleTagToken(chunk.tag, ruleImaginaryTokenType, chunk.label))
            else
                error("invalid tag: " * string(chunk.tag) * " in pattern: " * pattern)
            end
        else
            ptpm.lexer.setInputStream(InputStream(chunk.text))
            t = nextToken(ptpm.lexer)
            while t.type!=Token.EOF
                push!(tokens, t)
                t = nextToken(ptpm.lexer)
            end
        end
    end
    return tokens
end

# Split {@code <ID> = <e:expr> ;} into 4 chunks for tokenizing by {@link #tokenize}.#
function split(ptpm::ParseTreePatternMatcher, pattern::String)
    p = 0
    n = length(pattern)
    chunks = TextChunk[]
    # find all start and stop indexes first, then collect
    starts = Int[]
    stops = Int[]
    while p < n 
        if p == find(pattern, ptpm.escape + ptpm.start, p)
            p += length(ptpm.escape) + length(ptpm.start)
        elseif p == find(pattern, ptpm.escape + ptpm.stop, p)
            p += length(ptpm.escape) + length(ptpm.stop)
        elseif p == find(pattern, ptpm.start, p)
            push!(starts, p)
            p += length(ptpm.start)
        elseif p == find(pattern, ptpm.stop, p)
            push!(stops, p)
            p += length(ptpm.stop)
        else
            p += 1
        end
    end

    nt = length(starts)

    if nt > length(stops)
        error("unterminated tag in pattern: " * pattern)
    end
    if nt < length(stops)
        error("missing start tag in pattern: " * pattern)
    end

    for i in 1:nt
        if starts[i] >= stops[i]
            error("tag delimiters out of order in pattern: " * pattern)
        end
    end

    # collect into chunks now
    if nt==1
        push!(chunks, TextChunk(pattern))
    end

    if nt>1 && starts[1]>0 # copy text up to first tag into chunks
        text = pattern[1:starts[1]]
        chunks.add(TextChunk(text))
    end

    for i in 1:nt
        # copy inside of <tag>
        tag = pattern[starts[i] + length(ptpm.start) : stops[i]]
        ruleOrToken = tag
        label = nothing
        colon = find(tag, ':')
        if colon >= 0
            label = tag[0:colon]
            ruleOrToken = tag[colon+1 : length(tag)]
            append!(chunks, TagChunk(label, ruleOrToken))
        end
        if i+1 < length(starts)
            # copy from end of <tag> to start of next
            text = pattern[stops[i] + length(ptpm.stop) : starts[i + 1]]
            push!(chunks, TextChunk(text))
        end
    end
    if nt > 1
        afterLastTag = stops[nt - 1] + length(ptpm.stop)
        if afterLastTag < n  # copy text from end of last tag to end
            text = pattern[afterLastTag : n]
            push!(chunks, TextChunk(text))
        end
    end

    # strip out the escape sequences from text chunks but not tags
    for i in 1:length(chunks)
        c = chunks[i]
        if  c isa  TextChunk 
            unescaped = replace!(c.text, ptpm.escape, "")
            if length(unescaped) < length(c.text)
                chunks[i] = TextChunk(unescaped)
            end
        end
    end
    return chunks
end
