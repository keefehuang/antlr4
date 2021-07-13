#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

abstract type RewriteOperation end

function execute(rop::RewriteOperation, buf)
    rop.index
end

function Base.string(rop::RewriteOperation)
    return "<$(rop.name)@$(rop.tokens.get(rop.index))\"$(rop.text)\">"
end

struct InsertBeforeOp <: RewriteOperation
    tokens::Vector{nToken}
    index::nInt
    text::nString
    instructionIndex::nInt

    function RewriteOperation(tokens::Vector{nToken}, index::nInt, text::nString="")
        new(tokens, index, test, 0)
    end
end

function execute(op::InsertBeforeOp, buf::IOBuffer)
    buf.write(op.text)
    if get(op.tokens, op.index).type != eof(Token)
        buf.write(get(op.tokens, op.index).text)
    end
    return op.index + 1
end

struct InsertAfterOp <: RewriteOperation
    tokens::Vector{nToken}
    index::nInt
    text::nString
    instructionIndex::nInt

    function RewriteOperation(tokens::Vector{nToken}, index::nInt, text::nString="")
        new(tokens, index, test, 0)
    end
end

function execute(op::InsertAfterOp, buf::IOBuffer)
    buf.write(op.text)
    if get(op.tokens, op.index).type != eof(Token)
        buf.write(get(op.tokens, op.index).text)
    end
    return op.index + 1
end

struct ReplaceOp <: RewriteOperation
    tokens::Vector{nToken}
    index::nInt
    text::nString
    instructionIndex::nInt
    last_index::nInt

    function RewriteOperation(from_idx::Int, to_idx::Int, tokens::Vector{nToken}, text::nString="")
        new(tokens, from_idx, text, 0, to_idx)
    end
end

function execute(op::ReplaceOp, buf::IOBuffer)
    if !isempty(op.text)
        buf.write(op.text)
    end
    return op.last_index + 1
end

function Base.string(op::ReplaceOp)
    if !isempty(op.text)
        return "<ReplaceOp@$(get(op.tokens, self.index))..$(get(op.tokens, op.last_index))\"$(op.text)\">"
    end
    ""
end

struct TokenStreamRewriter
    tokens::Vector{nToken}
    programs::Any
    lastRewriteTokenIndices::Any
    
    function TokenStreamRewriter(tokens)
        new(tokens, Dict("default"=>[]), Dict())
    end
end

minTokenIndex(::TokenStreamRewriter) = 0
programInitSize(::TokenStreamRewriter) = 100

getTokenStream(tsr::TokenStreamRewriter) = tsr.tokens
DEFAULT_PROGRAM_NAME = "default"
PROGRAM_INIT_SIZE = 100
MIN_TOKEN_INDEX = 0

function rollback(tsr::TokenStreamRewriter, instruction_index::Int, program_name::String)
    ins = tsr.programs.get(program_name, nothing)
    if !isnothing(ins)
        tsr.programs[program_name] = ins[PROGRAM_INIT_SIZE: instruction_index]
    end
end

function deleteProgram(tsr::TokenStreamRewriter, program_name::String=DEFAULT_PROGRAM_NAME)
    rollback(tsr, minTokenIndex(MIN_TOKEN_INDEX), program_name)
end

function insertAfter(tsr::TokenStreamRewriter, index::Int, text::String, program_name::String=DEFAULT_PROGRAM_NAME)
    op = InsertAfterOp(tsr.tokens, index + 1, text)
    rewrites = getProgram(tsr, program_name)
    op.instructionIndex = len(rewrites)
    append!(rewrites, op)
end

function insertAfterToken(tsr::TokenStreamRewriter, token::Token, text::String, program_name::String=DEFAULT_PROGRAM_NAME)
    insertAfter(tsr, token.tokenIndex, text, program_name)
end

function insertBefore(tsr::TokenStreamRewriter, program_name::String, index::Int, text::String)
    op = InsertBeforeOp(tsr.tokens, index, text)
    rewrites = self.getProgram(program_name)
    op.instructionIndex = len(rewrites)
    append!(rewrites, op)
end

function insertBeforeIndex(tsr::TokenStreamRewriter, index::Int, text::String)
    insertBefore(tsr, DEFAULT_PROGRAM_NAME, index, text)
end

function insertBeforeToken(tsr::TokenStreamRewriter, token::Token, text::String, program_name::String=DEFAULT_PROGRAM_NAME)
    insertBefore(tsr, program_name, token.tokenIndex, text)
end

function Base.replace(tsr::TokenStreamRewriter, program_name::String, from_idx::Int, to_idx::Int, text::String)
    if any((from_idx > to_idx, from_idx < 0, to_idx < 0, to_idx >= len(self.tokens.tokens)))
        error(
            "replace: range invalid: $(from_idx)..$(to_idx)(size=$(len(tsr.tokens.tokens)))")
    end
    op = ReplaceOp(from_idx, to_idx, tsr.tokens, text)
    rewrites = getProgram(tsr, program_name)
    op.instructionIndex = len(rewrites)
    append!(rewrites, op)
end


function replaceIndex(tsr::TokenStreamRewriter, index::Int, text::String)
    replace(tsr, DEFAULT_PROGRAM_NAME, index, index, text)
end

function replaceRange(tsr::TokenStreamRewriter, from_idx::Int, to_idx::Int, text::String)
    replace(tsr, DEFAULT_PROGRAM_NAME, from_idx, to_idx, text)
end

function replaceSingleToken(tsr::TokenStreamRewriter, token::Token, text::String)
    replace(tsr, DEFAULT_PROGRAM_NAME, token.tokenIndex, token.tokenIndex, text)
end

function replaceRangeTokens(tsr::TokenStreamRewriter, from_token::Token, to_token::Token, text::String, program_name::String=DEFAULT_PROGRAM_NAME)
    replace(tsr, program_name, from_token.tokenIndex, to_token.tokenIndex, text)
end

function delete(tsr::TokenStreamRewriter, program_name::String, from_idx::Int, to_idx::Int)
    if from_idx isa Token
        replace(tsr, program_name, from_idx.tokenIndex, to_idx.tokenIndex, "")
    else
        replace(tsr, program_name, from_idx, to_idx, "")
    end
end

function deleteToken(tsr::TokenStreamRewriter, token::Token)
    delete(tsr, DEFAULT_PROGRAM_NAME, token, token)
end

function deleteIndex(tsr::TokenStreamRewriter, index::Int)
    delete(tsr, DEFAULT_PROGRAM_NAME, index, index)
end

function lastRewriteTokenIndex(tsr::TokenStreamRewriter, program_name::String=DEFAULT_PROGRAM_NAME)
    return tsr.lastRewriteTokenIndexes.get(program_name, -1)
end

function setLastRewriteTokenIndex(tsr::TokenStreamRewriter, program_name::String, i::Int)
    tsr.lastRewriteTokenIndexes[program_name] = i
end

function getProgram(tsr::TokenStreamRewriter, program_name::String)
    return setdefault(tsr.programs, program_name, [])
end

function getDefaultText(tsr::TokenStreamRewriter)
    return getText(tsr, DEFAULT_PROGRAM_NAME, 0, len(self.tokens.tokens))
end

function getText(tsr::TokenStreamRewriter, program_name::String, start::Int, stop::Int)
    """
    :return: the text in tokens[start, stop](closed interval)
    """
    rewrites = get(tsr.programs, program_name)

    # ensure start/end are in range
    if stop > len(self.tokens.tokens)
        stop = len(self.tokens.tokens)
    end

    if start < 0
        start = 0
    end

    # if no instructions to execute
    if isempty(rewrites)
        return getText(tsr.tokens, start, stop)
    end
    buf = IOBuffer()
    indexToOp = _reduceToSingleOperationPerIndex(tsr, rewrites)
    i = start
    while all((i <= stop, i < len(self.tokens.tokens)))
        op = pop!(indexToOp, i, nothing)
        token = get(tsr.tokens, i)
        if isnothing(op)
            if token.type != eof(Token)
                buf.write(token.text)
            end
            i += 1
        else
            i = execute(op, buf)
        end
    end
    if stop == len(self.tokens.tokens)
        for op in indexToOp.values()
            if op.index >= len(self.tokens.tokens)-1
                buf.write(op.text)
            end
        end
    end

    return buf.data()
end

function _reduceToSingleOperationPerIndex(tsr::TokenStreamRewriter, rewrites)
    # Walk replaces
    for (i, rop) in enumerate(rewrites)
        if any((isnothing(rop), !(rop isa ReplaceOp)))
            continue
        end
        # Wipe prior inserts within range
        inserts = [op for op in rewrites[1:i] if op isa InsertBeforeOp]
        for iop in inserts
            if iop.index == rop.index
                rewrites[iop.instructionIndex] = nothing
                rop.text = iop.text * rop.text
            elseif all((iop.index > rop.index, iop.index <= rop.last_index))
                rewrites[iop.instructionIndex] = nothing
            end
        end

        # Drop any prior replaces contained within
        prevReplaces = [op for op in rewrites[1:i] if op isa ReplaceOp]
        for prevRop in prevReplaces
            if all((prevRop.index >= rop.index, prevRop.last_index <= rop.last_index))
                rewrites[prevRop.instructionIndex] = nothing
                continue
            end
            isDisjoint = any((prevRop.last_index<rop.index, prevRop.index>rop.last_index))
            if all((isnothing(prevRop.text), isnothing(rop.text), !isDisjoint))
                rewrites[prevRop.instructionIndex] = nothing
                rop.index = min(prevRop.index, rop.index)
                rop.last_index = min(prevRop.last_index, rop.last_index)
                println("New rop $(rop)")
            elseif !isDisjoint
                error("replace op boundaries of {} overlap with previous {}".format(rop, prevRop))
            end
        end
    end

    # Walk inserts
    for (i, iop) in enumerate(rewrites)
        if any(isnothing(iop), !(iop isa InsertBeforeOp))
            continue
        end
        prevInserts = [op for op in rewrites[1:i] if op isa InsertBeforeOp]
        for (prev_index, prevIop) in enumerate(prevInserts)
            if prevIop.index == iop.index && prevIop isa InsertBeforeOp
                iop.text *= prevIop.text
                rewrites[prev_index] = nothing
            elseif prevIop.index == iop.index && prevIop isa InsertAfterOp
                iop.text = prevIop.text * iop.text
                rewrites[prev_index] = nothing
            end
        end
        # look for replaces where iop.index is in range; error
        prevReplaces = [op for op in rewrites[1:i] if isinstance(op, TokenStreamRewriter.ReplaceOp)]
        for rop in prevReplaces
            if iop.index == rop.index
                rop.text = iop.text * rop.text
                rewrites[i] = nothing
                continue
            end
            if all((iop.index >= rop.index, iop.index <= rop.last_index))
                error("insert op $iop within boundaries of previous $rop")
            end
        end
    end
    reduced = Dict()
    for (i, op) in enumerate(rewrites)
        if isnothing(op) 
            continue
        end
        if reduced.get(op.index)
           error("should be only one op per index")
        end
        reduced[op.index] = op
    end
    return reduced
end