#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#


#
#  Vacuum all input from a string and then treat it like a buffer.
#


mutable struct InputStream
    name::String
    strdata::String
    _index::nInt
    data::Vector{Int}
    _size::nInt
    function InputStream(strdata::String)
        data = [ord(c) for c in strdata]
        new("<empty>", strdata, 0, data, length(strdata))
    end
end

function _loadString!(is::InputStream)
    is._index = 0
    is.data = [ord(c) for c in is.strdata]
    is._size = len(is.data)
end

index(is::InputStream) = is._index
size(is::InputStream) = is._size

function reset!(is::InputStream)
    is._index = 0
end

function consume(is::InputStream)
    if is._index >= is._size
        @assert LA(is, 1) === TOKEN_EOF
        error("cannot consume EOF")
    end
    is._index += 1
end

function LA(is::InputStream, offset::Int)
    if offset == 0
        return 0 #undefined
    end
    if offset < 0
        offset += 1 # e.g., translate LA(-1) to use offset=0
    end
    pos = is._index + offset - 1
    if pos < 0 || pos >= is._size # invalid
        return TOKEN_EOF
    end
    return is.data[pos+1]
end

function LT(is::InputStream, offset::Int)
    return LA(is, offset)
end

function mark(::InputStream)
    -1
end

function release!(::InputStream, ::Int) end

# consume() ahead until p==_index; can't just set p=_index as we must
# update line and column. If we seek backwards, just set p
#
function seek!(is::InputStream, _index::Int)
    if _index<=is._index
        is._index = _index # just jump; don't update stream state (line, ...)
        return
    end
    # seek forward
    is._index = min(_index, is._size)
end

function getText(is::InputStream, start::Int, stop::Int)
    if stop > is._size
        stop = is._size-1
    end
    if start > is._size
        return ""
    end
    return is.strdata[start+1:stop+1]
end

function Base.string(is::InputStream)
    is.strdata
end