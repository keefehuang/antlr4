#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#
abstract type Chunk end

struct TagChunk <: Chunk
    tag::String
    label::nString

    function TagChunk(tag::String, label::nString=nothing)
        new(tag, label)
    end
end

function Base.string(tc::TagChunk)
    if isnothing(label)
        return tc.tag
    else
        return tc.label * ":" *tc.tag
    end
end

struct TextChunk <: Chunk
    text::String

    function TextChunk(text::String)
        new(text)
    end
end

function Base.string(tc::TextChunk)
    return "'"  + tc.text + "'"
end