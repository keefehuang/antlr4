#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.

#
# Specialized {@link Set}{@code <}{@link ATNConfig}{@code >} that can track
# info about the set, with support for combining similar configurations using a
# graph-structured stack.
#/
# from io import StringIO
# from functools import reduce
# from antlr4.PredictionContext import PredictionContext, merge
# from antlr4.Utils import str_list
# from antlr4.atn.ATN import ATN
# from antlr4.atn.ATNConfig import ATNConfig
# from antlr4.atn.SemanticContext import SemanticContext
# from antlr4.error.Errors import UnsupportedOperationException, IllegalStateException

#
# The reason that we need this is because we don't want the hash map to use
# the standard hash code and equals. We need all configurations with the same
# {@code (s,i,_,semctx)} to be equal. Unfortunately, this key effectively doubles
# the number of objects associated with ATNConfigs. The other solution is to
# use a hash table that lets us specify the equals/hashcode operation.

mutable struct ATNConfigSet
    # All configs but hashed by (s, i, _, pi) not including context. Wiped out
    # when we go readonly as this set becomes a DFA state.
    configLookup::Dict{UInt64,Vector{<:AbstractATNConfig}}
    # Indicates that this configuration set is part of a full context
    #  LL prediction. It will be used to determine how to merge $. With SLL
    #  it's a wildcard whereas it is not for LL context merge.
    fullCtx::Bool
    # Indicates that the set of configurations is read-only. Do not
    #  allow any code to manipulate the set; DFA states will point at
    #  the sets and they must not change. This does not protect the other
    #  fields; in particular, conflictingAlts is set after
    #  we've made this readonly.
    readOnly::Bool
    # Track the elements as they are added to the set; supports get(i)#/
    configs::Vector{AbstractATNConfig}
    # TODO: these fields make me pretty uncomfortable but nice to pack up info together, saves recomputation
    # TODO: can we track conflicts as they are added to save scanning configs later?
    uniqueAlt::Int
    conflictingAlts::Nothing
    # Used in parser and lexer. In lexer, it indicates we hit a pred
    # while computing a closure operation.  Don't make a DFA state from this.
    hasSemanticContext::Bool
    dipsIntoOuterContext::Bool
    cachedHashCode::Union{UInt64,Int64}

    function ATNConfigSet(fullCtx::Bool=true)
        new(Dict(), fullCtx, false, [], 0, nothing, false, false, -1)
    end
end

const nATNConfigSet = Union{ATNConfigSet,Nothing}

Base.iterate(atncs::ATNConfigSet, i::Int) = iterate(atncs.configs, i::Int)
Base.iterate(atncs::ATNConfigSet) = iterate(atncs.configs)

# Adding a new config means merging contexts with existing configs for
# {@code (s, i, pi, _)}, where {@code s} is the
# {@link ATNConfig#state}, {@code i} is the {@link ATNConfig#alt}, and
# {@code pi} is the {@link ATNConfig#semanticContext}. We use
# {@code (s,i,pi)} as key.
#
# <p>This method updates {@link #dipsIntoOuterContext} and
# {@link #hasSemanticContext} when necessary.</p>#/
function add!(atncs::ATNConfigSet, config::AbstractATNConfig, mergeCache::Dict{Int,ATNConfig}=Dict{Int,ATNConfig}())
    if atncs.readOnly
        error("This set is readonly")
    end
    if config.semanticContext != SEMANTIC_CONTEXT_NONE
        atncs.hasSemanticContext = true
    end
    if config.reachesIntoOuterContext > 0
        atncs.dipsIntoOuterContext = true
    end
    existing = getOrAdd!(atncs, config)
    if existing === config
        atncs.cachedHashCode = -1
        push!(atncs.configs, config)  # track order here
        return true
    end
    # a previous (s,i,pi,_), merge with it and save result
    rootIsWildcard = !atncs.fullCtx
    merged = merge(existing.context, config.context, rootIsWildcard, mergeCache)
    # no need to check for existing.context, config.context in cache
    # since only way to create new graphs is "call rule" and here.
    # We cache at both places.
    existing.reachesIntoOuterContext = max(existing.reachesIntoOuterContext, config.reachesIntoOuterContext)
    # make sure to preserve the precedence filter suppression during the merge
    if config.precedenceFilterSuppressed
        existing.precedenceFilterSuppressed = true
    end
    existing.context = merged # replace context; no need to alt mapping
    return true
end

function getOrAdd!(atncs::ATNConfigSet, config::AbstractATNConfig)
    h = hashCodeForConfigSet(config)
    l = get(atncs.configLookup, h, nothing)
    if !isnothing(l)
#         r = next((cfg for cfg in l if equalsForConfigSet(config, cfg)), nothing)
        index = findfirst(x->(equalsForConfigSet(config, x)), l)
        if !isnothing(index)
            r = get(l, index, nothing)
        else
            r = nothing
        end

        if !isnothing(r)
            return r
        end
    end
    if isnothing(l)
        l = [config]
        atncs.configLookup[h] = l
    else
        append!(l, config)
    end
    return config
end

function getStates(atncs::ATNConfigSet)
    return set(c.state for c in atncs.configs)
end

function getPredicates(atncs::ATNConfigSet)
    return list(cfg.semanticContext for cfg in atncs.configs if cfg.semanticContext!=SemanticContext.NONE)
end

function Base.get(atncs::ATNConfigSet, i::Int; default::Any=nothing)
    get(atncs.configs, i, default)
end
        
function optimizeConfigs!(atncs::ATNConfigSet, interpreter::AbstractATNSimulator)
    if atncs.readOnly
        IllegalStateException("This set is readonly")
    end
    if isempty(atncs.configs)
        return
    end
    for config in atncs.configs
        config.context = getCachedContext(interpreter, config.context)
    end
end

function addAll!(atncs::ATNConfigSet, coll::Vector{ATNConfig})
    for c in coll
        add!(atncs, c)
    end
    return false
end

function Base.:(==)(atncs::ATNConfigSet, other::ATNConfigSet)
    if atncs === other
        return true
    end
    if isnothing(atncs.configs)
        return false
    end
    same = atncs.configs==other.configs &&
    atncs.fullCtx == other.fullCtx &&
    atncs.uniqueAlt == other.uniqueAlt &&
    atncs.conflictingAlts == other.conflictingAlts &&
    atncs.hasSemanticContext == other.hasSemanticContext &&
    atncs.dipsIntoOuterContext == other.dipsIntoOuterContext

    return same
end

function Base.hash(atncs::ATNConfigSet)
    if atncs.readOnly
        if atncs.cachedHashCode == -1
            atncs.cachedHashCode = hashConfigs(atncs)
        end
        return atncs.cachedHashCode
    end
    return hashConfigs(atncs)
end

function hashConfigs(atncs::ATNConfigSet)
    return hash(hash.(atncs.configs))
    # return reduce((h, cfg) -> hash((h, cfg)), atncs.configs; init=0)
end

Base.length(atncs::ATNConfigSet) = length(atncs.configs)

Base.isempty(atncs::ATNConfigSet) = isempty(atncs.configs)

function Base.in(atncs::ATNConfigSet, config::ATNConfig)
    if atncs.readOnly
        UnsupportedOperationException("This method is not implemented for readonly sets.")
    end
    h = hashCodeForConfigSet(config)
    l = get(atncs.configLookup, h)
    if !isnothing(l)
        for c in l
            if equalsForConfigSet(config, c)
                return true
            end
        end
    end
    return false
end

function clear!(atncs::ATNConfigSet)
    if atncs.readOnly
        IllegalStateException("This set is readonly")
    end
    atncs.config = ATNConfig[]
    atncs.cachedHashCode = -1
    atncs.configLookup = Dict{Int,ATNConfig}()
end
    
function setReadonly!(atncs::ATNConfigSet, readOnly::Bool)
    atncs.readOnly = readOnly
    atncs.configLookup = Dict{UInt,ATNConfig}() # can't mod, no need for lookup cache
end

function Base.string(atncs::ATNConfigSet)
    buf = IOBuffer()
    for config in atncs.configs
        write(buf, string(config))
        write(buf, ", ")
    end
    if atncs.hasSemanticContext
        write(buf, ",hasSemanticContext=")
        write(buf, string(atncs.hasSemanticContext))
    end
    if atncs.uniqueAlt != Int(ATNENUM.INVALID_ALT_NUMBER)
        write(buf, ",uniqueAlt=")
        write(buf, string(atncs.uniqueAlt))
    end
    if !isnothing(atncs.conflictingAlts)
        write(buf, ",conflictingAlts=")
        write(buf, string(atncs.conflictingAlts))
    end
    if atncs.dipsIntoOuterContext
        write(buf, ",dipsIntoOuterContext")
    end
    return String(take!(buf))
end

function Base.show(io::IO, ::MIME"text/plain", atncs::ATNConfigSet)
    println(io, string(atncs))
end