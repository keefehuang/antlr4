#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/


# Represents {@code $} in local context prediction, which means wildcard.
# {@code#+x =#}.
#/
EMPTY_CONTEXT = nothing

# Represents {@code $} in an array in full context mode, when {@code $}
# doesn't mean wildcard: {@code $ + x = [$,x]}. Here,
# {@code $} = {@link #EMPTY_RETURN_STATE}.
#/
EMPTY_RETURN_STATE = 0x7FFFFFFF

abstract type PredictionContext end
const nPredictionContext = Union{PredictionContext,Nothing}

    # Stores the computed hash code of this {@link PredictionContext}. The hash
    # code is computed in parts to match the following reference algorithm.
    #
    # <pre>
    #  private int referenceHashCode() {
    #      int hash = {@link MurmurHash#initialize MurmurHash.initialize}({@link #INITIAL_HASH});
    #
    #      for (int i = 0; i &lt; {@link #size()}; i++) {
    #          hash = {@link MurmurHash#update MurmurHash.update}(hash, {@link #getParent getParent}(i));
    #      }
    #
    #      for (int i = 0; i &lt; {@link #size()}; i++) {
    #          hash = {@link MurmurHash#update MurmurHash.update}(hash, {@link #getReturnState getReturnState}(i));
    #      }
    #
    #      hash = {@link MurmurHash#finish MurmurHash.finish}(hash, 2# {@link #size()});
    #      return hash;
    #  }
    # </pre>
    #/

Base.length(::PredictionContext) = 0

Base.isempty(::PredictionContext) = false

function getReturnState(::PredictionContext, ::Int)
    throw(IllegalStateException("illegal!"))
end

function hasEmptyPath(p::PredictionContext)
    getReturnState(p, length(p)-1) == EMPTY_RETURN_STATE
end

function calculateHashCode(parent::PredictionContext, returnState::Int)
    if isnothing(parent)
        return hash("")
    end
    hash((hash(parent), returnState))
end

function calculateListsHashCode(parents::Vector{PredictionContext}, returnStates::Vector{Int})
    h = 0
    for (parent, returnState) in zip(parents, returnStates)
        h = hash((h, calculateHashCode(parent, returnState)))
    end
    return h
end

#  Used to cache {@link PredictionContext} objects. Its used for the shared
#  context cash associated with contexts in DFA states. This cache
#  can be used for both lexers and parsers.

struct PredictionContextCache
    cache::Dict
    function PredictionContextCache()
        new(Dict())
    end
end


#  Add a context to the cache and return it. If the context already exists,
#  return that one instead and do not add a new context to the cache.
#  Protect shared cache from unsafe thread access.
function add!(pc::PredictionContextCache, ctx::PredictionContext)
    if ctx == EMPTY_CONTEXT
        return EMPTY_CONTEXT
    end
    existing = get(pc.cache, ctx, nothing)
    if !isnothing(existing)
        return existing
    end
    pc.cache[ctx] = ctx
    return ctx
end

function Base.get(pc::PredictionContextCache, ctx::PredictionContext)
    get(pc.cache, ctx, nothing)
end

function Base.length(pc::PredictionContextCache)
    return length(pc.cache)
end

struct EmptyPredictionContext <: PredictionContext end
EMPTY_HASH = 0xbd32f78d463d7cfb
PREDICTION_CONTEXT_EMPTY = EmptyPredictionContext()

Base.isempty(::EmptyPredictionContext) = true
getReturnState(::EmptyPredictionContext, ::Int) = EMPTY_RETURN_STATE

Base.hash(::EmptyPredictionContext) = EMPTY_HASH
Base.string(::EmptyPredictionContext) = "\$"

struct SingletonPredictionContext <: PredictionContext
    cachedHashCode::UInt64
    parentCtx::PredictionContext
    returnState::Int

    function SingletonPredictionContext(parent::PredictionContext, returnState::Int)
        hashCode = calculateHashCode(parent, returnState)
        new(hashCode, parent, returnState)
    end
end

Base.length(ctx::SingletonPredictionContext) = 1

getParent(ctx::SingletonPredictionContext, ::Int) = ctx.parentCtx
getReturnState(ctx::SingletonPredictionContext, ::Int) = ctx.returnState

function Base.:(==)(self::SingletonPredictionContext, other::SingletonPredictionContext)
    return self.returnState == other.returnState && self.parentCtx == other.parentCtx
end

Base.hash(ctx::T) where {T <: PredictionContext}= ctx.cachedHashCode

function create(parent::nPredictionContext, returnState::Int )
    if returnState == EMPTY_RETURN_STATE && isnothing(parent)
        # someone can pass in the bits of an array ctx that mean $
        return PREDICTION_CONTEXT_EMPTY
    end
    return SingletonPredictionContext(parent, returnState)
end



function Base.string(ctx::SingletonPredictionContext)
    if isnothing(ctx.parentCtx)
        up = ""
    else
        up = string(ctx.parentCtx)
    end

    if length(up) == 0
        if ctx.returnState == EMPTY_RETURN_STATE
            return "\$"
        else
            return string(ctx.returnState)
        end
    end
    return string(ctx.returnState) * " " * up
end

struct ArrayPredictionContext <: PredictionContext
    cachedHashCode::UInt64
    parents::Vector{nPredictionContext}
    returnStates::Vector{nInt}

    # Parent can be null only if full ctx mode and we make an array
    #  from {@link #EMPTY} and non-empty. We merge {@link #EMPTY} by using null parent and
    #  returnState == {@link #EMPTY_RETURN_STATE}.
    function ArrayPredictionContext(parents::Vector{nPredictionContext}, returnStates::Vector{nInt})
        new(calculateListsHashCode(parents, returnStates), parents, returnStates)
    end
end

Base.isempty(ctx::ArrayPredictionContext) = ctx.returnState[1] == EMPTY_RETURN_STATE

Base.length(ctx::ArrayPredictionContext) = length(ctx.returnStates)

getParent(ctx::ArrayPredictionContext, index::Int) = ctx.parents[index]

getReturnState(ctx::ArrayPredictionContext, index::Int) = ctx.returnStates[index]

function Base.:(==)(self::ArrayPredictionContext, other::ArrayPredictionContext)
    if self === other
        return true
    end
    if hash(self) != hash(other)
        return false
    end
    return self.returnStates == other.returnStates && self.parents == other.parents
end

function Base.string(ctx::ArrayPredictionContext)
    if isempty(ctx)
        return "[]"
    end
    buf = IOBuffer()
    write(buf, "[")
    for i in 1:length(ctx.returnStates)
        if i > 1
            write(buf, ", ")
        end
        if ctx.returnStates[i] == EMPTY_RETURN_STATE
            write(buf, "\$")
            continue
        end
        write(buf, string(ctx.returnStates[i]))
        if !isnothing(ctx.parents[i])
            write(buf, ' ')
            write(buf, string(ctx.parents[i]))
        else
            write(buf, "null")
        end
        write(buf, "]")
    end
    return buf.data()
end

#  Convert a {@link RuleContext} tree to a {@link PredictionContext} graph.
#  Return {@link #EMPTY} if {@code outerContext} is empty or null.
#/
function PredictionContextFromRuleContext(atn::AbstractATN, outerContext::nRuleContext=nothing)
    if isnothing(outerContext)
        outerContext = RULE_CONTEXT_EMPTY
    end

    # if we are in RuleContext of start rule, s, then PredictionContext
    # is EMPTY. Nobody called us. (if we are empty, return empty)
    if isnothing(outerContext.parentCtx) || outerContext == EMPTY_CONTEXT
        return PREDICTION_CONTEXT_EMPTY
    end

    # If we have a parent, convert it to a PredictionContext graph
    parent = PredictionContextFromRuleContext(atn, outerContext.parentCtx)
    state = atn.states[outerContext.invokingState+1]
    transition = state.transitions[1]
    return create(parent, transition.followState.stateNumber)
end

function merge(a::PredictionContext, b::PredictionContext, rootIsWildcard::Bool, mergeCache::Dict)
    # share same graph if both same
    if a == b
        return a 
    end

    if a isa SingletonPredictionContext && b isa SingletonPredictionContext
        return mergeSingletons(a, b, rootIsWildcard, mergeCache)
    end

    if rootIsWildcard
        if a isa EmptyPredictionContext
            return a 
        end
        if b isa EmptyPredictionContext
            return b
        end
    end

    if a isa SingletonPredictionContext
        a = ArrayPredictionContext([a.parentCtx], [a.returnState])
    end
    if b isa SingletonPredictionContext
        b = ArrayPredictionContext([b.parentCtx], [b.returnState])
    end
    mergeArrays(a, b, rootIsWildcard, mergeCache)
end

#
# Merge two {@link SingletonPredictionContext} instances.
#
# <p>Stack tops equal, parents merge is same; return left graph.<br>
# <embed src="images/SingletonMerge_SameRootSamePar.svg" type="image/svg+xml"/></p>
#
# <p>Same stack top, parents differ; merge parents giving array node, then
# remainders of those graphs. A new root node is created to point to the
# merged parents.<br>
# <embed src="images/SingletonMerge_SameRootDiffPar.svg" type="image/svg+xml"/></p>
#
# <p>Different stack tops pointing to same parent. Make array node for the
# root where both element in the root point to the same (original)
# parent.<br>
# <embed src="images/SingletonMerge_DiffRootSamePar.svg" type="image/svg+xml"/></p>
#
# <p>Different stack tops pointing to different parents. Make array node for
# the root where each element points to the corresponding original
# parent.<br>
# <embed src="images/SingletonMerge_DiffRootDiffPar.svg" type="image/svg+xml"/></p>
#
# @param a the first {@link SingletonPredictionContext}
# @param b the second {@link SingletonPredictionContext}
# @param rootIsWildcard {@code true} if this is a local-context merge,
# otherwise false to indicate a full-context merge
# @param mergeCache
#/

function mergeSingletons(a::SingletonPredictionContext, b::SingletonPredictionContext, rootIsWildcard::Bool, mergeCache::Dict)
    if !isempty(mergeCache)
        previous = get(mergeCache, (a,b), nothing)
        if !isnothing(previous)
            return previous
        end
        previous = get(mergeCache, (b,a), nothing)
        if !isnothing(previous)
            return previous
        end
    end
    merged = mergeRoot(a, b, rootIsWildcard)
    if !nothing(merged)
        if !isempty(mergeCache)
            mergeCache[(a, b)] = merged
        end
        return merged
    end

    if a.returnState == b.returnState
        parent = merge(a.parentCtx, b.parentCtx, rootIsWildcard, mergeCache)
        # if parent is same as existing a or b parent or reduced to a parent, return it
        if parent == a.parentCtx
            return a # ax + bx = ax, if a=b
        end
        if parent == b.parentCtx
            return b # ax + bx = bx, if a=b
        end
        # else: ax + ay = a'[x,y]
        # merge parents x and y, giving array node with x,y then remainders
        # of those graphs.  dup a, a' points at merged array
        # new joined parent so create new singleton pointing to it, a'
        merged = create(parent, a.returnState)
        if !isempty(mergeCache)
            mergeCache[(a, b)] = merged
        end
        return merged
    end 
    # a != b payloads differ
    # see if we can collapse parents due to $+x parents if local ctx
    singleParent = nothing
    if a == b || (!isnothing(a.parentCtx) && a.parentCtx==b.parentCtx) # ax + bx = [a,b]x
        singleParent = a.parentCtx
    end
    if !isnothing(singleParent)# parents are same
        # sort payloads and use same parent
        payloads = [ a.returnState, b.returnState ]
        if a.returnState > b.returnState
            payloads = [ b.returnState, a.returnState ]
        end
        parents = [singleParent, singleParent]
        merged = ArrayPredictionContext(parents, payloads)
        if !isempty(mergeCache)
            mergeCache[(a, b)] = merged
        end
        return merged
    end
    # parents differ and can't merge them. Just pack together
    # into array; can't merge.
    # ax + by = [ax,by]
    payloads = [ a.returnState, b.returnState ]
    parents = [ a.parentCtx, b.parentCtx ]
    if a.returnState > b.returnState # sort by payload
        payloads = [ b.returnState, a.returnState ]
        parents = [ b.parentCtx, a.parentCtx ]
    end
    merged = ArrayPredictionContext(parents, payloads)
    if !isempty(mergeCache)
        mergeCache[(a, b)] = merged
    end
    return merged
end


#
# Handle case where at least one of {@code a} or {@code b} is
# {@link #EMPTY}. In the following diagrams, the symbol {@code $} is used
# to represent {@link #EMPTY}.
#
# <h2>Local-Context Merges</h2>
#
# <p>These local-context merge operations are used when {@code rootIsWildcard}
# is true.</p>
#
# <p>{@link #EMPTY} is superset of any graph; return {@link #EMPTY}.<br>
# <embed src="images/LocalMerge_EmptyRoot.svg" type="image/svg+xml"/></p>
#
# <p>{@link #EMPTY} and anything is {@code #EMPTY}, so merged parent is
# {@code #EMPTY}; return left graph.<br>
# <embed src="images/LocalMerge_EmptyParent.svg" type="image/svg+xml"/></p>
#
# <p>Special case of last merge if local context.<br>
# <embed src="images/LocalMerge_DiffRoots.svg" type="image/svg+xml"/></p>
#
# <h2>Full-Context Merges</h2>
#
# <p>These full-context merge operations are used when {@code rootIsWildcard}
# is false.</p>
#
# <p><embed src="images/FullMerge_EmptyRoots.svg" type="image/svg+xml"/></p>
#
# <p>Must keep all contexts; {@link #EMPTY} in array is a special value (and
# null parent).<br>
# <embed src="images/FullMerge_EmptyRoot.svg" type="image/svg+xml"/></p>
#
# <p><embed src="images/FullMerge_SameRoot.svg" type="image/svg+xml"/></p>
#
# @param a the first {@link SingletonPredictionContext}
# @param b the second {@link SingletonPredictionContext}
# @param rootIsWildcard {@code true} if this is a local-context merge,
# otherwise false to indicate a full-context merge
#/
function mergeRoot(a::SingletonPredictionContext, b::SingletonPredictionContext, rootIsWildcard::Bool)
    if rootIsWildcard
        if a == PREDICTION_CONTEXT_EMPTY
            return PREDICTION_CONTEXT_EMPTY  ## + b =#
        end
        if b == PREDICTION_CONTEXT_EMPTY
            return PREDICTION_CONTEXT_EMPTY  # a +# =#
        end
    end

    if a == PREDICTION_CONTEXT_EMPTY && b == PREDICTION_CONTEXT_EMPTY
        return PREDICTION_CONTEXT_EMPTY # $ + $ = $
    elseif a == PREDICTION_CONTEXT_EMPTY # $ + x = [$,x]
        payloads = [ b.returnState, PREDICTION_CONTEXT_EMPTY ]
        parents = [ b.parentCtx, nothing ]
        return ArrayPredictionContext(parents, payloads)
    elseif b == PREDICTION_CONTEXT_EMPTY # x + $ = [$,x] ($ is always first if present)
        payloads = [ a.returnState, PREDICTION_CONTEXT_EMPTY ]
        parents = [ a.parentCtx, nothing ]
        return ArrayPredictionContext(parents, payloads)
    end
    return nothing
end


#
# Merge two {@link ArrayPredictionContext} instances.
#
# <p>Different tops, different parents.<br>
# <embed src="images/ArrayMerge_DiffTopDiffPar.svg" type="image/svg+xml"/></p>
#
# <p>Shared top, same parents.<br>
# <embed src="images/ArrayMerge_ShareTopSamePar.svg" type="image/svg+xml"/></p>
#
# <p>Shared top, different parents.<br>
# <embed src="images/ArrayMerge_ShareTopDiffPar.svg" type="image/svg+xml"/></p>
#
# <p>Shared top, all shared parents.<br>
# <embed src="images/ArrayMerge_ShareTopSharePar.svg" type="image/svg+xml"/></p>
#
# <p>Equal tops, merge parents and reduce top to
# {@link SingletonPredictionContext}.<br>
# <embed src="images/ArrayMerge_EqualTop.svg" type="image/svg+xml"/></p>
#/
function mergeArrays(a::ArrayPredictionContext, b::ArrayPredictionContext, rootIsWildcard::Bool, mergeCache::Dict)
    if !isempty(mergeCache)
        previous = mergeCache.get((a,b), nothing)
        if !isnothing(previous)
            return previous
        end
        previous = mergeCache.get((b,a), nothing)
        if !isnothing(previous)
            return previous
        end
    end
    # merge sorted payloads a + b => M
    i = 0 # walks a
    j = 0 # walks b
    k = 0 # walks target M array

    numStates = length(a.returnStates) + length( b.returnStates)
    mergedReturnStates = Vector{nInt}(nothing, numStates)
    mergedParents = Vector{nPredictionContext}(nothing, numStates)
    
    # walk and merge to yield mergedParents, mergedReturnStates
    while i < length(a.returnStates) && j < length(b.returnStates)
        a_parent = a.parents[i]
        b_parent = b.parents[j]
        if a.returnStates[i] == b.returnStates[j]
            # same payload (stack tops are equal), must yield merged singleton
            payload = a.returnStates[i]
            # $+$ = $
            bothDollars = payload == EMPTY_RETURN_STATE && isnothing(a_parent) && isnothing(b_parent)
            ax_ax = !isnothing(a_parent) && !isnothing(b_parent) && a_parent==b_parent # ax+ax -> ax
            if bothDollars || ax_ax
                mergedParents[k] = a_parent # choose left
                mergedReturnStates[k] = payload
            else # ax+ay -> a'[x,y]
                mergedParent = merge(a_parent, b_parent, rootIsWildcard, mergeCache)
                mergedParents[k] = mergedParent
                mergedReturnStates[k] = payload
            end
            i += 1 # hop over left one as usual
            j += 1 # but also skip one in right side since we merge
        elseif a.returnStates[i] < b.returnStates[j] # copy a[i] to M
            mergedParents[k] = a_parent
            mergedReturnStates[k] = a.returnStates[i]
            i += 1
        else # b > a, copy b[j] to M
            mergedParents[k] = b_parent
            mergedReturnStates[k] = b.returnStates[j]
            j += 1
        end
        k += 1
    end

    # copy over any payloads remaining in either array
    if i < length(a.returnStates)
        for p in i:length(a.returnStates)
            mergedParents[k] = a.parents[p]
            mergedReturnStates[k] = a.returnStates[p]
            k += 1
        end
    else
        for p in j:length(b.returnStates)
            mergedParents[k] = b.parents[p]
            mergedReturnStates[k] = b.returnStates[p]
            k += 1
        end
    end

    # trim merged if we combined a few that had same stack tops
    if k < length(mergedParents) # write index < last position; trim
        if k == 1 # for just one merged element, return singleton top
            merged = create(mergedParents[1], mergedReturnStates[1])
            if !isempty(mergeCache)
                mergeCache[(a,b)] = merged
            end
            return merged
        end
        mergedParents = mergedParents[1:k]
        mergedReturnStates = mergedReturnStates[1:k]
    end

    merged = ArrayPredictionContext(mergedParents, mergedReturnStates)

    # if we created same array as a or b, return that instead
    # TODO: track whether this is possible above during merge sort for speed
    if merged == a
        if !isempty(mergeCache)
            mergeCache[(a,b)] = a
        end
        return a
    end
    if merged == b
        if !isempty(mergeCache)
            mergeCache[(a,b)] = b
        end
        return b
    end
    combineCommonParents(mergedParents)

    if !isempty(mergeCache)
        mergeCache[(a,b)] = merged
    end
    return merged
end


#
# Make pass over all <em>M</em> {@code parents}; merge any {@code equals()}
# ones.
#/
function combineCommonParents(parents::Vector{nPredictionContext})
    uniqueParents = Dict()

    for p in 1:length(parents)
        parent = parents[p]
        if haskey(uniqueParents, parent)
            uniqueParents[parent] = parent
        end
    end

    for p in 1:length(parents)
        parents[p] = uniqueParents[parents[p]]
    end
end

function getCachedPredictionContext(context::PredictionContext, contextCache::PredictionContextCache, visited::Dict)
    if isempty(context)
        return context
    end
    existing = get(visited, context, nothing)
    if haskey(visited, context)
        return visited[context]
    end
    if haskey(contextCache, context)
        visited[context] = contextCache[context]
        return contextCache[context]
    end
    changed = false
    parents = [nothing] * length(context)
    for i in 1:length(parents)
        parent = getCachedPredictionContext(getParent(context, i), contextCache, visited)
        if changed || parent != getParent(context, i)
            if not changed
                parents = [context.getParent(j) for j in range(length(context))]
                changed = true
            end
            parents[i] = parent
        end
    end
    if not changed
        add!(contextCache, context)
        visited[context] = context
        return context
    end

    updated = nothing
    if length(parents) == 0
        updated = PREDICTION_CONTEXT_EMPTY
    elseif length(parents) == 1
        updated = create(parents[1], getReturnState(context, 1))
    else
        updated = ArrayPredictionContext(parents, context.returnStates)
    end

    add!(contextCache, updated)
    visited[updated] = updated
    visited[context] = updated

    return updated
end


#	# extra structures, but cut/paste/morphed works, so leave it.
#	# seems to do a breadth-first walk
#	public static List<PredictionContext> getAllNodes(PredictionContext context) {
#		Map<PredictionContext, PredictionContext> visited =
#			new IdentityHashMap<PredictionContext, PredictionContext>();
#		Deque<PredictionContext> workList = new ArrayDeque<PredictionContext>();
#		workList.add(context);
#		visited.put(context, context);
#		List<PredictionContext> nodes = new ArrayList<PredictionContext>();
#		while (!workList.isEmpty()) {
#			PredictionContext current = workList.pop();
#			nodes.add(current);
#			for (int i = 0; i < current.size(); i++) {
#				PredictionContext parent = current.getParent(i);
#				if ( parent!=null && visited.put(parent, parent) == null) {
#					workList.push(parent);
#				}
#			}
#		}
#		return nodes;
#	}

# ter's recursive version of Sam's getAllNodes()
function getAllContextNodes(context::PredictionContext, nodes::Vector{PredictionContext}=PredictionContext[], visited::Dict=Dict())
    if isnothing(context) || haskey(visited, context)
        return nodes
    end
    visited[context] = context
    push!(nodes, context)
    for i in 1:length(context)
        getAllContextNodes(context.getParent(i), nodes, visited)
    end
    return nodes
end

