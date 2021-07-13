#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#
struct IntervalSet
    intervals::Vector{UnitRange}
    readOnly::Bool

    function IntervalSet()
        new(UnitRange[], false)
    end
end

function Base.length(is::IntervalSet)
    if isempty(is.intervals)
        return 0
    end
    sum(length.(is.intervals))
end

function Base.getindex(is::IntervalSet, i::Int)
    if i >= 1 && i <= length(is.intervals)
        return is.intervals[i]
    end
    TOKEN_INVALID_TYPE
end

function Base.setindex!(is::IntervalSet, v::UnitRange, i::Int) 
    is.intervals[i] = v
end

Base.firstindex(is::IntervalSet) = 1
Base.lastindex(is::IntervalSet) = length(is.intervals)
function Base.iterate(is::IntervalSet, state=1) 
    return state > length(is.intervals) ? nothing : (is.intervals[state], state + 1)
end

function reduce!(is::IntervalSet, k::Int)
    # only need to reduce if k is not the last
        if k < length(is.intervals)
            l = is[k]
            r = is[k+1]
            # if r contained in l
            if l.stop >= r.stop
                popat!(is.intervals, k+1)
                reduce!(is, k)
            elseif l.stop >= r.start
                is[k] = l.start:r.stop
                popat!(is.intervals, k+1)
            end
        end
    end

function addRange!(is::IntervalSet, v::UnitRange)
    if isempty(is.intervals)
        push!(is.intervals, v)
    else
        # find insert pos        
        for (k,i) in enumerate(is)
            # distinct range -> insert
            if v.stop < i.start
                insert!(is.intervals, k, v)
                return
            # contiguous range -> adjust
            elseif v.stop == i.start
                is.intervals[k] = v.start:i.stop
                return
            # overlapping range -> adjust and reduce
            elseif v.start <= i.stop
                is.intervals[k] = minimum([i.start,v.start]):maximum([i.stop,v.stop])
                reduce!(is, k)
                return
            end
        end
        # greater than any existing
        push!(is.intervals, v)
    end
end

function addOne!(is::IntervalSet, v::Int)
    addRange!(is, v:v)
end

function addSet!(is::IntervalSet, other::IntervalSet)
    if !isempty(other.intervals)
        for i in other.intervals
            addRange!(is, i)
        end
    end
    return is
end


function Base.in(range::UnitRange, is::IntervalSet)
    if isnothing(is.intervals)
        return false
    end
    range in is.intervals
end

function Base.in(item::Integer, is::IntervalSet)
    if isnothing(is.intervals)
        return false
    end
    return any(item in i for i in is.intervals)
end

function removeOne!(is::IntervalSet, v::Int)
    if !isempty(is.intervals)
        for (k,i) in enumerate(is)
            # intervals is ordered
            if v < i.start
                return
            # check for single value range
            elseif v == i.start && v == i.stop
                popat!(is.intervals, k)
                return
            # check for lower boundary
            elseif v == i.start
                is[k] = i.start+1:i.stop
                return
            # check for upper boundary
            elseif v == i.stop
                is[k] = i.start:i.stop-1
                return
            # split existing range
            elseif v < i.stop
                x = i.start:v-1
                is[k] = v + 1:i.stop
                insert!(is.intervals, k, x)
                return
            end
            k += 1
        end
    end
end

function removeRange!(is::IntervalSet, v::UnitRange)
    if v.start == v.stop-1
        removeOne!(is, v.start)
    end
    k = 1
    while k <= length(is.intervals)
        i = is[k]
        # intervals are ordered
        if v.stop < i.start
            return
        # check for including range, split it
        elseif v.start > i.start && v.stop < i.stop
            is[k] = v.stop+1:i.stop
            x = i.start:v.start-1
            insert!(is.intervals, k, x)
            return
        # check for included range, remove it
        elseif v.start <= i.start && v.stop >= i.stop
            popat!(is.intervals, k)
            k -= 1  # need another pass
        # check upper boundary
        elseif v.start < i.start && v.stop < i.stop
            is[k] = v.stop+1:i.stop
        # check for lower boundary
        elseif v.start < i.stop
            is[k] = i.start:v.start-1
        end
        k += 1
    end
end

function complement(is::IntervalSet, start::Int, stop::Int)
    result = IntervalSet()
    addRange!(result, start:stop)
    for i in is
        removeRange!(result, i)
    end
    return result
end

function elementName(is::IntervalSet, literalNames::Vector{String}, symbolicNames::Vector{String}, a::Int)
    if a == TOKEN_EOF
        return "<EOF>"
    elseif a==TOKEN_EPSILON
        return "<EPSILON>"
    end
    if a < length(literalNames) && literalNames[a] != "<INVALID>"
        return literalNames[a]
    end
    if a < length(symbolicNames)
        return symbolicNames[a]
    end
    return "<UNKNOWN>"
end

function toString(is::IntervalSet, literalNames::Vector{String}, symbolicNames::Vector{String})
    if isempty(is.intervals)
        return "{}"
    end
    buf = IOBuffer()
    if length(is) > 1
        buf.write("{")
    end
    first = true
    for i in is
        for j in i
            if !first
                buf.write(", ")
            end
            buf.write(elementName(is, literalNames, symbolicNames, j))
            first = false
        end
    end
    if length(is) > 1
        buf.write("}")
    end
    return buf.data()
end