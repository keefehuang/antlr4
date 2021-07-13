#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/

# A tuple: (ATN state, predicted alt, syntactic, semantic context).
#  The syntactic context is a graph-structured stack node whose
#  path(s) to the root is the rule invocation(s)
#  chain used to arrive at the state.  The semantic context is
#  the tree of semantic predicates encountered before reaching
#  an ATN state.
#/
abstract type AbstractATNConfig end

mutable struct ATNConfig <: AbstractATNConfig
    state::ATNState
    alt::Int
    context::nPredictionContext
    semanticContext::nSemanticContext
    reachesIntoOuterContext::Int
    precedenceFilterSuppressed::Bool

    function ATNConfig(;state::nATNState=nothing, alt::nInt=nothing, context::nPredictionContext=nothing, semantic::nSemanticContext=nothing, config::Union{AbstractATNConfig,Nothing}=nothing)
        if !isnothing(config)
            if isnothing(state)
                state = config.state
            end
            if isnothing(alt)
                alt = config.alt
            end
            if isnothing(context)
                context = config.context
            end
            if isnothing(semantic)
                semantic = context.semanticContext
            end
        end
        if isnothing(semantic)
            semantic = SEMANTIC_CONTEXT_NONE
        end
        if isnothing(config)
            reachesIntoOuterContext = 0
        else
            reachesIntoOuterContext = config.reachesIntoOuterContext
        end
        if isnothing(config)
            precedenceFilterSuppressed = false
        else
            precedenceFilterSuppressed = config.precedenceFilterSuppressed
        end
        new(state, alt, context, semantic, reachesIntoOuterContext, precedenceFilterSuppressed)
    end
end

function Base.:(==)(self::ATNConfig, other::ATNConfig)
    if self === other
        return true
    end

    return self.state.stateNumber == other.state.stateNumber && 
    self.alt == other.alt && ((self.context === other.context) 
    || (self.context == other.context)) && self.semanticContext==other.semanticContext && 
    self.precedenceFilterSuppressed == other.precedenceFilterSuppressed
end


Base.hash(atnc::ATNConfig) = hash((atnc.state.stateNumber, atnc.alt, atnc.context, atnc.semanticContext))

hashCodeForConfigSet(atnc::ATNConfig) = hash((atnc.state.stateNumber, atnc.alt, hash(atnc.semanticContext)))

function equalsForConfigSet(self::ATNConfig, other::ATNConfig)
    if self === other
        return true
    end
    return self.state.stateNumber == other.state.stateNumber && 
    self.alt==other.alt && 
    self.semanticContext==other.semanticContext
end

function Base.string(atnc::ATNConfig)
    buf = IOBuffer()
    write(buf, "(")
    write(buf, "State: " * string(atnc.state))
    write(buf, ",")
    write(buf, "Alt: " * string(Int(atnc.alt)))
    if !isnothing(atnc.context )
        write(buf, ",[")
        write(buf, "Context: " * string(atnc.context))
        write(buf, "]")
    end
    if !isnothing(atnc.semanticContext) && atnc.semanticContext != SEMANTIC_CONTEXT_NONE
        write(buf, ",")
        write(buf, string(atnc.semanticContext))
    end
    if atnc.reachesIntoOuterContext > 0
        write(buf, ",up=")
        write(buf, string(atnc.reachesIntoOuterContext))
    end
    write(buf, ")")
    return String(take!(buf))
end

function Base.show(io::IO, ::MIME"text/plain", atnConfig::ATNConfig)
    println(io, string(atnConfig))
end


mutable struct LexerATNConfig <: AbstractATNConfig
    atnconfig::ATNConfig
    lexerActionExecutor::nLexerActionExecutor
    passedThroughNonGreedyDecision::Bool

    function LexerATNConfig(state::ATNState; alt::nInt=nothing, context::nPredictionContext=nothing, semantic::nSemanticContext=SEMANTIC_CONTEXT_NONE,
        lexerActionExecutor::nLexerActionExecutor=nothing, config::Union{AbstractATNConfig,Nothing}=nothing)
        if !isnothing(config) 
            if isnothing(lexerActionExecutor)
                lexerActionExecutor = config.lexerActionExecutor
            end
        end
        passedThroughNonGreedyDecision = false
        if !isnothing(config)
            passedThroughNonGreedyDecision = checkNonGreedyDecision(config, state)
        end
        new(ATNConfig(state=state, alt=alt, context=context, semantic=semantic, config=config), lexerActionExecutor, passedThroughNonGreedyDecision)
    end
end

function Base.string(lexerATNConfig::LexerATNConfig)
    buf = IOBuffer()
    write(buf, string(lexerATNConfig.atnconfig))
    return String(take!(buf))
end

function Base.show(io::IO, ::MIME"text/plain", atnConfig::T) where {T<:AbstractATNConfig}
    println(io, string(atnConfig))
end

function Base.getproperty(lexerAtnConfig::LexerATNConfig, field::Symbol)
    if hasfield(LexerATNConfig, field)
        return getfield(lexerAtnConfig, field)
    else
        return getfield(getfield(lexerAtnConfig, :atnconfig), field)
    end
end

function Base.setproperty!(lexerAtnConfig::LexerATNConfig, field::Symbol, value::S) where {S}
    if hasfield(LexerATNConfig, field)
        return setfield!(lexerAtnConfig, field, value)
    elseif hasfield(ATNConfig, field)
        return setfield!(getfield(lexerAtnConfig, :atnconfig), field, value)
    end
    error("type LexerATNState has no field $field")
end

const nLexerATNConfig = Union{LexerATNConfig,Nothing}

Base.hash(lexerAtnConfig::LexerATNConfig) = hash((lexerAtnConfig.state.stateNumber, lexerAtnConfig.alt, lexerAtnConfig.context,
                                                    lexerAtnConfig.semanticContext, lexerAtnConfig.passedThroughNonGreedyDecision,
                                                    lexerAtnConfig.lexerActionExecutor))

function Base.:(==)(self::LexerATNConfig, other::LexerATNConfig)
    if self === other
        return true
    end
    if !(self.lexerActionExecutor == other.lexerActionExecutor)
        return false
    end
    if self.passedThroughNonGreedyDecision != other.passedThroughNonGreedyDecision
        return false
    end
    return self.atnconfig == other.atnconfig
end

hashCodeForConfigSet(l::LexerATNConfig) = hash(l)

equalsForConfigSet(self::LexerATNConfig, other::LexerATNConfig) = self == other

function checkNonGreedyDecision(source::LexerATNConfig, target::ATNState)
    return source.passedThroughNonGreedyDecision ||
     target isa DecisionState && target.nonGreedy
end