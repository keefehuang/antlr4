#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

#
# The embodiment of the adaptive LL(*), ALL(*), parsing strategy.
#
# <p>
# The basic complexity of the adaptive strategy makes it harder to understand.
# We begin with ATN simulation to build paths in a DFA. Subsequent prediction
# requests go through the DFA first. If they reach a state without an edge for
# the current symbol, the algorithm fails over to the ATN simulation to
# complete the DFA path for the current input (until it finds a conflict state
# || uniquely predicting state).</p>
#
# <p>
# All of that is done without using the outer context because we want to create
# a DFA that is not dependent upon the rule invocation stack when we do a
# prediction. One DFA works in all contexts. We avoid using context not
# necessarily because it's slower, although it can be, but because of the DFA
# caching problem. The closure routine only considers the rule invocation stack
# created during prediction beginning in the decision rule. For example, if
# prediction occurs without invoking another rule's ATN, there are no context
# stacks in the configurations. When lack of context leads to a conflict, we
# don't know if it's an ambiguity || a weakness in the strong LL(*) parsing
# strategy (versus full LL(*)).</p>
#
# <p>
# When SLL yields a configuration set with conflict, we rewind the input and
# retry the ATN simulation, this time using full outer context without adding
# to the DFA. Configuration context stacks will be the full invocation stacks
# from the start rule. If we get a conflict using full context, then we can
# definitively say we have a true ambiguity for that input sequence. If we
# don't get a conflict, it implies that the decision is sensitive to the outer
# context. (It is not context-sensitive in the sense of context-sensitive
# grammars.)</p>
#
# <p>
# The next time we reach this DFA state with an SLL conflict, through DFA
# simulation, we will again retry the ATN simulation using full context mode.
# This is slow because we can't save the results and have to "interpret" the
# ATN each time we get that input.</p>
#
# <p>
# <strong>CACHING FULL CONTEXT PREDICTIONS</strong></p>
#
# <p>
# We could cache results from full context to predicted alternative easily and
# that saves a lot of time but doesn't work in presence of predicates. The set
# of visible predicates from the ATN start state changes depending on the
# context, because closure can fall off the end of a rule. I tried to cache
# tuples (stack context, semantic context, predicted alt) but it was slower
# than interpreting and much more complicated. Also required a huge amount of
# memory. The goal is not to create the world's fastest parser anyway. I'd like
# to keep this algorithm simple. By launching multiple threads, we can improve
# the speed of parsing across a large number of files.</p>
#
# <p>
# There is no strict ordering between the amount of input used by SLL vs LL,
# which makes it really hard to build a cache for full context. Let's say that
# we have input A B C that leads to an SLL conflict with full context X. That
# implies that using X we might only use A B but we could also use A B C D to
# resolve conflict. Input A B C D could predict alternative 1 in one position
# in the input and A B C E could predict alternative 2 in another position in
# input. The conflicting SLL configurations could still be non-unique in the
# full context prediction, which would lead us to requiring more input than the
# original A B C.	To make a	prediction cache work, we have to track	the exact
# input	used during the previous prediction. That amounts to a cache that maps
# X to a specific DFA for that context.</p>
#
# <p>
# Something should be done for left-recursive expression predictions. They are
# likely LL(1) + pred eval. Easier to do the whole SLL unless error and retry
# with full LL thing Sam does.</p>
#
# <p>
# <strong>AVOIDING FULL CONTEXT PREDICTION</strong></p>
#
# <p>
# We avoid doing full context retry when the outer context is empty, we did not
# dip into the outer context by falling off the end of the decision state rule,
# || when we force SLL mode.</p>
#
# <p>
# As an example of the not dip into outer context case, consider as super
# constructor calls versus function calls. One grammar might look like
# this:</p>
#
# <pre>
# ctorBody
#   : '{' superCall? stat* '}'
#   ;
# </pre>
#
# <p>
# Or, you might see something like</p>
#
# <pre>
# stat
#   : superCall ';'
#   | expression ';'
#   | ...
#   ;
# </pre>
#
# <p>
# In both cases I believe that no closure operations will dip into the outer
# context. In the first case ctorBody in the worst case will stop at the '}'.
# In the 2nd case it should stop at the ';'. Both cases should stay within the
# entry rule and not dip into the outer context.</p>
#
# <p>
# <strong>PREDICATES</strong></p>
#
# <p>
# Predicates are always evaluated if present in either SLL || LL both. SLL and
# LL simulation deals with predicates differently. SLL collects predicates as
# it performs closure operations like ANTLR v3 did. It delays predicate
# evaluation until it reaches and accept state. This allows us to cache the SLL
# ATN simulation whereas, if we had evaluated predicates on-the-fly during
# closure, the DFA state configuration sets would be different and we couldn't
# build up a suitable DFA.</p>
#
# <p>
# When building a DFA accept state during ATN simulation, we evaluate any
# predicates and return the sole semantically valid alternative. If there is
# more than 1 alternative, we report an ambiguity. If there are 0 alternatives,
# we throw an exception. Alternatives without predicates act like they have
# true predicates. The simple way to think about it is to strip away all
# alternatives with false predicates and choose the minimum alternative that
# remains.</p>
#
# <p>
# When we start in the DFA and reach an accept state that's predicated, we test
# those and return the minimum semantically viable alternative. If no
# alternatives are viable, we throw an exception.</p>
#
# <p>
# During full LL ATN simulation, closure always evaluates predicates and
# on-the-fly. This is crucial to reducing the configuration set size during
# closure. It hits a landmine when parsing with the Java grammar, for example,
# without this on-the-fly evaluation.</p>
#
# <p>
# <strong>SHARING DFA</strong></p>
#
# <p>
# All instances of the same parser share the same decision DFAs through a
# static field. Each instance gets its own ATN simulator but they share the
# same {@link #decisionToDFA} field. They also share a
# {@link PredictionContextCache} object that makes sure that all
# {@link PredictionContext} objects are shared among the DFA states. This makes
# a big size difference.</p>
#
# <p>
# <strong>THREAD SAFETY</strong></p>
#
# <p>
# The {@link ParserATNSimulator} locks on the {@link #decisionToDFA} field when
# it adds a new DFA object to that array. {@link #addDFAEdge}
# locks on the DFA for the current decision when setting the
# {@link DFAState#edges} field. {@link #addDFAState} locks on
# the DFA for the current decision when looking up a DFA state to see if it
# already exists. We must make sure that all requests to add DFA states that
# are equivalent result in the same shared DFA object. This is because lots of
# threads will be trying to update the DFA at once. The
# {@link #addDFAState} method also locks inside the DFA lock
# but this time on the shared context cache when it rebuilds the
# configurations' {@link PredictionContext} objects using cached
# subgraphs/nodes. No other locking occurs, even during DFA simulation. This is
# safe as long as we can guarantee that all threads referencing
# {@code s.edge[t]} get the same physical target {@link DFAState}, or
# {@code null}. Once into the DFA, the DFA simulation does not reference the
# {@link DFA#states} map. It follows the {@link DFAState#edges} field to new
# targets. The DFA simulator will either find {@link DFAState#edges} to be
# {@code null}, to be non-{@code null} and {@code dfa.edges[t]} null, or
# {@code dfa.edges[t]} to be non-null. The
# {@link #addDFAEdge} method could be racing to set the field
# but in either case the DFA simulator works; if {@code null}, and requests ATN
# simulation. It could also race trying to get {@code dfa.edges[t]}, but either
# way it will work because it's not doing a test and set operation.</p>
#
# <p>
# <strong>Starting with SLL then failing to combined SLL/LL (Two-Stage
# Parsing)</strong></p>
#
# <p>
# Sam pointed out that if SLL does not give a syntax error, then there is no
# point in doing full LL, which is slower. We only have to try LL if we get a
# syntax error. For maximum speed, Sam starts the parser set to pure SLL
# mode with the {@link BailErrorStrategy}:</p>
#
# <pre>
# parser.{@link Parser#getInterpreter() getInterpreter()}.{@link #setPredictionMode setPredictionMode}{@code (}{@link PredictionMode#SLL}{@code )};
# parser.{@link Parser#setErrorHandler setErrorHandler}(new {@link BailErrorStrategy}());
# </pre>
#
# <p>
# If it does not get a syntax error, then we're done. If it does get a syntax
# error, we need to retry with the combined SLL/LL strategy.</p>
#
# <p>
# The reason this works is as follows. If there are no SLL conflicts, then the
# grammar is SLL (at least for that input set). If there is an SLL conflict,
# the full LL analysis must yield a set of viable alternatives which is a
# subset of the alternatives reported by SLL. If the LL set is a singleton,
# then the grammar is LL but not SLL. If the LL set is the same size as the SLL
# set, the decision is SLL. If the LL set has size &gt; 1, then that decision
# is truly ambiguous on the current input. If the LL set is smaller, then the
# SLL conflict resolution might choose an alternative that the full LL would
# rule out as a possibility based upon better context information. If that's
# the case, then the SLL parse will definitely get an error because the full LL
# analysis says it's not viable. If SLL conflict resolution chooses an
# alternative within the LL set, them both SLL and LL would choose the same
# alternative because they both choose the minimum of multiple conflicting
# alternatives.</p>
#
# <p>
# Let's say we have a set of SLL conflicting alternatives {@code {1, 2, 3}} and
# a smaller LL set called <em>s</em>. If <em>s</em> is {@code {2, 3}}, then SLL
# parsing will get an error because SLL will pursue alternative 1. If
# <em>s</em> is {@code {1, 2}} || {@code {1, 3}} then both SLL and LL will
# choose the same alternative because alternative one is the minimum of either
# set. If <em>s</em> is {@code {2}} || {@code {3}} then SLL will get a syntax
# error. If <em>s</em> is {@code {1}} then SLL will succeed.</p>
#
# <p>
# Of course, if the input is invalid, then we will get an error for sure in
# both SLL and LL parsing. Erroneous input will therefore require 2 passes over
# the input.</p>
#
# import sys
# from antlr4 import DFA
# from antlr4.PredictionContext import PredictionContextCache, PredictionContext, SingletonPredictionContext, \
#     PredictionContextFromRuleContext
# from antlr4.BufferedTokenStream import TokenStream
# from antlr4.Parser import Parser
# from antlr4.ParserRuleContext import ParserRuleContext
# from antlr4.RuleContext import RuleContext
# from antlr4.Token import Token
# from antlr4.Utils import str_list
# from antlr4.atn.ATN import ATN
# from antlr4.atn.ATNConfig import ATNConfig
# from antlr4.atn.ATNConfigSet import ATNConfigSet
# from antlr4.atn.ATNSimulator import ATNSimulator
# from antlr4.atn.ATNState import StarLoopEntryState, DecisionState, RuleStopState, ATNState
# from antlr4.atn.PredictionMode import PredictionMode
# from antlr4.atn.SemanticContext import SemanticContext, AND, andContext, orContext
# from antlr4.atn.Transition import Transition, RuleTransition, ActionTransition, PrecedencePredicateTransition, \
#     PredicateTransition, AtomTransition, SetTransition, NotSetTransition
# from antlr4.dfa.DFAState import DFAState, PredPrediction
# from antlr4.error.Errors import NoViableAltException

mutable struct ParserATNSimulator <: ATNSimulator
    atn::ATN
    sharedContextCache::PredictionContextCache
    parser::AbstractParser
    decisionToDFA::Vector{DFA}
    # # SLL, LL, || LL + exact ambig detection?#
    predictionMode::PredictionMode.PredictionModeEnum
    # # LAME globals to avoid parameters!!!!! I need these down deep in predTransition
    _input::nTokenStream
    _startIndex::Int
    _outerContext::nParserRuleContext
    _dfa::nDFA
    # Each prediction operation uses a cache for merge of prediction contexts.
    #  Don't keep around as it wastes huge amounts of memory. DoubleKeyMap
    #  isn't synchronized but we're ok since two threads shouldn't reuse same
    #  parser/atnsim object because it can only handle one input at a time.
    #  This maps graphs a and b to merged result c. (a,b)&rarr;c. We can avoid
    #  the merge if we ever see a and b again.  Note that (b,a)&rarr;c should
    #  also be examined during cache lookup.
    mergeCache::Dict

    function ParserATNSimulator(parser::AbstractParser, atn::ATN, decisionToDFA::Vector{DFA}, sharedContextCache::PredictionContextCache)
        new(atn, sharedContextCache, parser, decisionToDFA, PredictionMode.LL, nothing, 0, nothing, nothing, Dict())
    end
end

function reset!(::ParserATNSimulator) end


function adaptivePredict(parserATNSimulator::ParserATNSimulator, input::TokenStream, decision::Int, outerContext::ParserRuleContext)
    if debug || debug_list_atn_decisions
        println("adaptivePredict decision " * string(decision) *
                                " exec LA(1)==" * getLookaheadName(parseATNSimulator, input) *
                                " line " * string(input.LT(1).line) * ":" *
                                string(input.LT(1).column))
    end
    parserATNSimulator._input = input
    parserATNSimulator._startIndex = input.index
    parserATNSimulator._outerContext = outerContext

    dfa = parserATNSimulator.decisionToDFA[decision]
    parserATNSimulator._dfa = dfa
    m = input.mark()
    index = input.index

    # Now we are certain to have a specific decision's DFA
    # But, do we still need an initial state?
    try
        if dfa.precedenceDfa
            # the start state for a precedence DFA depends on the current
            # parser precedence, and is provided by a DFA method.
            s0 = getPrecedenceStartState(dfa, getPrecedence(parserATNSimulator.parser))
        else
            # the start state for a "regular" DFA is just s0
            s0 = dfa.s0
        end

        if isnothing(s0)
            if isnothing(outerContext)
                outerContext = ParserRuleContext.EMPTY
            end
            if debug || debug_list_atn_decisions
                println("predictATN decision " * string(dfa.decision) *
                                    " exec LA(1)==" * getLookaheadName(parseATNSimulator, input) *
                                    ", outerContext=" * toString(outerContext, parserATNSimulator.parser.literalNames, nothing))
            end

            fullCtx = false
            s0_closure = computeStartState(parserATNSimulator, dfa.atnStartState, ParserRuleContext.EMPTY, fullCtx)

            if dfa.precedenceDfa
                # If this is a precedence DFA, we use applyPrecedenceFilter
                # to convert the computed start state to a precedence start
                # state. We then use DFA.setPrecedenceStartState to set the
                # appropriate start state for the precedence level rather
                # than simply setting DFA.s0.
                #
                dfa.s0.configs = s0_closure # not used for prediction but useful to know start configs anyway
                s0_closure = applyPrecedenceFilter(parserATNSimulator, s0_closure)
                s0 = addDFAState(parserATNSimulator, dfa, DFAState(configs=s0_closure))
                setPrecedenceStartState(dfa, parserATNSimulator.parser.getPrecedence(), s0)
            else
                s0 = addDFAState(parseATNSimulator, dfa, DFAState(configs=s0_closure))
                dfa.s0 = s0
            end
        end
        alt = execATN(parseATNSimulator, dfa, s0, input, index, outerContext)
        if debug
            println("DFA after predictATN: " * toString(dfa, parserATNSimulator.parser.literalNames))
        end
        return alt
    finally
        parserATNSimulator._dfa = nothing
        parserATNSimulator.mergeCache = nothing # wack cache after each prediction
        seek(input, index)
        input.release(m)
    end
end

# Performs ATN simulation to compute a predicted alternative based
#  upon the remaining input, but also updates the DFA cache to avoid
#  having to traverse the ATN again for the same input sequence.

# There are some key conditions we're looking for after computing a new
# set of ATN configs (proposed DFA state)
# if the set is empty, there is no viable alternative for current symbol
# does the state uniquely predict an alternative?
# does the state have a conflict that would prevent us from
#   putting it on the work list?

# We also have some key operations to do:
# add an edge from previous DFA state to potentially new DFA state, D,
#   upon current symbol but only if adding to work list, which means in all
#   cases except no viable alternative (and possibly non-greedy decisions?)
# collecting predicates and adding semantic context to DFA accept states
# adding rule context to context-sensitive DFA accept states
# consuming an input symbol
# reporting a conflict
# reporting an ambiguity
# reporting a context sensitivity
# reporting insufficient predicates

# cover these cases:
#    dead end
#    single alt
#    single alt + preds
#    conflict
#    conflict + preds
#
function execATN(parserATNSimulator::ParserATNSimulator, dfa::DFA, s0::DFAState, input::TokenStream, startIndex::Int, outerContext::ParserRuleContext)
    if debug || debug_list_atn_decisions
        println("execATN decision " * string(dfa.decision) +
                " exec LA(1)==" * getLookaheadName(parseATNSimulator, input) +
                " line " * string(input.LT(1).line) * ":" * string(input.LT(1).column))
    end

    previousD = s0

    if debug
        println("s0 = " * string(s0))
    end

    t = input.LA(1)

    while true # while more work
        D = getExistingTargetState(parseATNSimulator, previousD, t)
        if isnothing(D)
            D = computeTargetState(parseATNSimulator, dfa, previousD, t)
        end
        if D === parserATNSimulator.ERROR
            # if any configs in previous dipped into outer context, that
            # means that input up to t actually finished entry rule
            # at least for SLL decision. Full LL doesn't dip into outer
            # so don't need special case.
            # We will get an error no matter what so delay until after
            # decision; better error message. Also, no reachable target
            # ATN states in SLL implies LL will also get nowhere.
            # If conflict in states that dip out, choose min since we
            # will get error no matter what.
            seek(input, startIndex)
            alt = getSynValidOrSemInvalidAltThatFinishedDecisionEntryRule(parserATNSimulator, previousD.configs, outerContext)
            if alt!=ATN.INVALID_ALT_NUMBER
                return alt
            end
            noViableAlt(parserATNSimulator, input, outerContext, previousD.configs, startIndex)
        end
        if D.requiresFullContext && parserATNSimulator.predictionMode != PredictionMode.SLL
            # IF PREDS, MIGHT RESOLVE TO SINGLE ALT => SLL (or syntax error)
            conflictingAlts = D.configs.conflictingAlts
            if !isnothing(D.predicates)
                if debug
                    println("DFA state has preds in DFA sim LL failover")
                end
                conflictIndex = input.index
                if conflictIndex != startIndex
                    seek(input, startIndex)
                end

                conflictingAlts = evalSemanticContext(parseATNSimulator, D.predicates, outerContext, true)
                if length(conflictingAlts)==1
                    if debug
                        println("Full LL avoided")
                    end
                    return minimum(conflictingAlts)
                end

                if conflictIndex != startIndex
                    # restore the index so reporting the fallback to full
                    # context occurs with the index at the correct spot
                    seek(input, conflictIndex)
                end
            end

            if ParserATNSimulator.dfa_debug
                println("ctx sensitive state " * string(outerContext) +" in " * string(D))
            end
            fullCtx = true
            s0_closure = computeStartState(parserATNSimulator, dfa.atnStartState, outerContext, fullCtx)
            reportAttemptingFullContext(parserATNSimulator, dfa, conflictingAlts, D.configs, startIndex, input.index)
            alt = execATNWithFullContext(parserATNSimulator, dfa, D, s0_closure, input, startIndex, outerContext)
            return alt
        end

        if D.isAcceptState
            if !isnothing(D.predicates)
                return D.prediction
            end

            stopIndex = input.index
            seek(input, startIndex)
            alts = evalSemanticContext(parserATNSimulator, D.predicates, outerContext, true)
            if length(alts)==0
                noViableAlt(parseATNSimulator, input, outerContext, D.configs, startIndex)
            elseif length(alts)==1
                return minimum(alts)
            else
                # report ambiguity after predicate evaluation to make sure the correct
                # set of ambig alts is reported.
                reportAmbiguity(parseATNSimulator, dfa, D, startIndex, stopIndex, false, alts, D.configs)
                return minimum(alts)
            end
        end
        previousD = D

        if t != Token.EOF
            consume(input)
            t = input.LA(1)
        end
    end
end


#
# Get an existing target state for an edge in the DFA. If the target state
# for the edge has not yet been computed || is otherwise not available,
# this method returns {@code null}.
#
# @param previousD The current DFA state
# @param t The next input symbol
# @return The existing target DFA state for the given input symbol
# {@code t}, || {@code null} if the target state for this edge is not
# already cached
#
function getExistingTargetState(::ParserATNSimulator, previousD::DFAState, t::Int)
    edges = previousD.edges
    if isnothing(edges) || t + 1 < 0 || t + 1 >= length(edges)
        return nothing
    else
        return edges[t + 1]
    end
end

#
# Compute a target state for an edge in the DFA, and attempt to add the
# computed state and corresponding edge to the DFA.
#
# @param dfa The DFA
# @param previousD The current DFA state
# @param t The next input symbol
#
# @return The computed target DFA state for the given input symbol
# {@code t}. If {@code t} does not lead to a valid DFA state, this method
# returns {@link #ERROR}.
#
function computeTargetState(parserATNSimulator::ParserATNSimulator, dfa::DFA, previousD::DFAState, t::Int)
    reach = computeReachSet(parseATNSimulator, previousD.configs, t, false)
    if isnothing(reach)
        addDFAEdge(parseATNSimulator, dfa, previousD, t, parserATNSimulator.ERROR)
        return parserATNSimulator.ERROR
    end

    # create new target state; we'll add to DFA after it's complete
    D = DFAState(configs=reach)

    predictedAlt = getUniqueAlt(parseATNSimulator, reach)

    if debug
        altSubSets = PredictionMode.getConflictingAltSubsets(reach)
        println("SLL altSubSets=" * string(altSubSets) * ", configs=" * string(reach) *
                    ", predict=" * string(predictedAlt) * ", allSubsetsConflict=" *
                    string(PredictionMode.allSubsetsConflict(altSubSets)) * ", conflictingAlts=" *
                    string(getConflictingAlts(parseATNSimulator, reach)))
    end

    if predictedAlt!=ATN.INVALID_ALT_NUMBER
        # NO CONFLICT, UNIQUELY PREDICTED ALT
        D.isAcceptState = true
        D.configs.uniqueAlt = predictedAlt
        D.prediction = predictedAlt
    elseif hasSLLConflictTerminatingPrediction(parserATNSimulator.predictionMode, reach)
        # MORE THAN ONE VIABLE ALTERNATIVE
        D.configs.conflictingAlts = getConflictingAlts(parseATNSimulator, reach)
        D.requiresFullContext = true
        # in SLL-only mode, we will stop at this state and return the minimum alt
        D.isAcceptState = true
        D.prediction = minimum(D.configs.conflictingAlts)
    end
    if D.isAcceptState && D.configs.hasSemanticContext
        predicateDFAState(parseATNSimulator, D, parserATNSimulator.atn.getDecisionState(dfa.decision))
        if !isnothing(D.predicates)
            D.prediction = ATN.INVALID_ALT_NUMBER
        end
    end

    # all adds to dfa are done after we've created full D state
    D = addDFAEdge(parseATNSimulator, dfa, previousD, t, D)
    return D
end

function predicateDFAState(parserATNSimulator::ParserATNSimulator, dfaState::DFAState, decisionState::DecisionState)
    # We need to test all predicates, even in DFA states that
    # uniquely predict alternative.
    nalts = length(decisionState.transitions)
    # Update DFA so reach becomes accept state with (predicate,alt)
    # pairs if preds found for conflicting alts
    altsToCollectPredsFrom = getConflictingAltsOrUniqueAlt(parseATNSimulator, dfaState.configs)
    altToPred = getPredsForAmbigAlts(parseATNSimulator, altsToCollectPredsFrom, dfaState.configs, nalts)
    if !isnothing(altToPred)
        dfaState.predicates = getPredicatePredictions(parseATNSimulator, altsToCollectPredsFrom, altToPred)
        dfaState.prediction = ATNEnum.INVALID_ALT_NUMBER # make sure we use preds
    else
        # There are preds in configs but they might go away
        # when OR'd together like {p}? || nothing == nothing. If neither
        # alt has preds, resolve to min alt
        dfaState.prediction = minimum(altsToCollectPredsFrom)
    end
end

# comes back with reach.uniqueAlt set to a valid alt
function execATNWithFullContext(parserATNSimulator::ParserATNSimulator, dfa::DFA, D::DFAState, # how far we got before failing over
                                        s0::ATNConfigSet, input::TokenStream, startIndex::Int, outerContext::ParserRuleContext)
    if debug || debug_list_atn_decisions
        println("execATNWithFullContext", string(s0))
    end
    fullCtx = true
    foundExactAmbig = false
    reach = nothing
    previous = s0
    seek(input, startIndex)
    t = input.LA(1)
    predictedAlt = -1
    while true # while more work
        reach = computeReachSet(parseATNSimulator, previous, t, fullCtx)
        if isnothing(reach)
            # if any configs in previous dipped into outer context, that
            # means that input up to t actually finished entry rule
            # at least for LL decision. Full LL doesn't dip into outer
            # so don't need special case.
            # We will get an error no matter what so delay until after
            # decision; better error message. Also, no reachable target
            # ATN states in SLL implies LL will also get nowhere.
            # If conflict in states that dip out, choose min since we
            # will get error no matter what.
            
            seek(input, startIndex)
            alt = getSynValidOrSemInvalidAltThatFinishedDecisionEntryRule(parseATNSimulator, previous, outerContext)
            if alt!=ATN.INVALID_ALT_NUMBER
                return alt
            else
                noViableAlt(parseATNSimulator, input, outerContext, previous, startIndex)
            end
        end

        altSubSets = PredictionMode.getConflictingAltSubsets(reach)
        if debug
            println("LL altSubSets=" * string(altSubSets) * ", predict=" *
                    string(PredictionMode.getUniqueAlt(altSubSets)) * ", resolvesToJustOneViableAlt=" *
                    string(PredictionMode.resolvesToJustOneViableAlt(altSubSets)))
        end

        reach.uniqueAlt = getUniqueAlt(parseATNSimulator, reach)
        # unique prediction?
        if reach.uniqueAlt!=ATN.INVALID_ALT_NUMBER
            predictedAlt = reach.uniqueAlt
            break
        elseif parserATNSimulator.predictionMode != PredictionMode.LL_EXACT_AMBIG_DETECTION
            predictedAlt = PredictionMode.resolvesToJustOneViableAlt(altSubSets)
            if predictedAlt != ATN.INVALID_ALT_NUMBER
                break
            end
        else
            # In exact ambiguity mode, we never try to terminate early.
            # Just keeps scarfing until we know what the conflict is
            if allSubsetsConflict(altSubSets) && allSubsetsEqual(altSubSets)
                foundExactAmbig = true
                predictedAlt = PredictionMode.getSingleViableAlt(altSubSets)
                break
            end
            # else there are multiple non-conflicting subsets or
            # we're not sure what the ambiguity is yet.
            # So, keep going.
        end

        previous = reach
        if t != Token.EOF
            consume(input)
            t = input.LA(1)
        end
    end
    # If the configuration set uniquely predicts an alternative,
    # without conflict, then we know that it's a full LL decision
    # not SLL.
    if reach.uniqueAlt != ATN.INVALID_ALT_NUMBER 
        reportContextSensitivity(parseATNSimulator, dfa, predictedAlt, reach, startIndex, input.index)
        return predictedAlt
    end

    # We do not check predicates here because we have checked them
    # on-the-fly when doing full context prediction.

    #
    # In non-exact ambiguity detection mode, we might	actually be able to
    # detect an exact ambiguity, but I'm not going to spend the cycles
    # needed to check. We only emit ambiguity warnings in exact ambiguity
    # mode.
    #
    # For example, we might know that we have conflicting configurations.
    # But, that does not mean that there is no way forward without a
    # conflict. It's possible to have nonconflicting alt subsets as in:

    # altSubSets=[{1, 2}, {1, 2}, {1}, {1, 2}]

    # from
    #
    #    [(17,1,[5 $]), (13,1,[5 10 $]), (21,1,[5 10 $]), (11,1,[$]),
    #     (13,2,[5 10 $]), (21,2,[5 10 $]), (11,2,[$])]
    #
    # In this case, (17,1,[5 $]) indicates there is some next sequence that
    # would resolve this without conflict to alternative 1. Any other viable
    # next sequence, however, is associated with a conflict.  We stop
    # looking for input because no amount of further lookahead will alter
    # the fact that we should predict alternative 1.  We just can't say for
    # sure that there is an ambiguity without looking further.

    reportAmbiguity(parseATNSimulator, dfa, D, startIndex, input.index, foundExactAmbig, nothing, reach)

    return predictedAlt
end

function computeReachSet(parserATNSimulator::ParserATNSimulator, closure::ATNConfigSet, t::Int, fullCtx::Bool)
    if debug
        println("in computeReachSet, starting closure: " * string(closure))
    end

    if !isnothing(parserATNSimulator.mergeCache)
        parserATNSimulator.mergeCache = Dict()
    end

    intermediate = ATNConfigSet(fullCtx)

    # Configurations already in a rule stop state indicate reaching the end
    # of the decision rule (local context) || end of the start rule (full
    # context). Once reached, these configurations are never updated by a
    # closure operation, so they are handled separately for the performance
    # advantage of having a smaller intermediate set when calling closure.
    #
    # For full-context reach operations, separate handling is required to
    # ensure that the alternative matching the longest overall sequence is
    # chosen when multiple such configurations can match the input.

    skippedStopStates = nothing

    # First figure out where we can reach on input t
    for c in closure
        if debug
            println("testing " * getTokenName(parseATNSimulator, t) * " at " * string(c))
        end
        if isinstance(c.state, RuleStopState)
            if fullCtx || t == Token.EOF
                if isnothing(skippedStopStates)
                    skippedStopStates = list()
                end
                push!(skippedStopStates, c)
            end
            continue
        end

        for trans in c.state.transitions
            target = getReachableTarget(parseATNSimulator, trans, t)
            if !isnothing(target)
                intermediate.add(ATNConfig(state=target, config=c), parserATNSimulator.mergeCache)
            end
        end
    end
    # Now figure out where the reach operation can take us...

    reach = nothing

    # This block optimizes the reach operation for intermediate sets which
    # trivially indicate a termination state for the overall
    # adaptivePredict operation.
    #
    # The conditions assume that intermediate
    # contains all configurations relevant to the reach set, but this
    # condition is not true when one || more configurations have been
    # withheld in skippedStopStates, || when the current symbol is EOF.
    #
    if isnothing(skippedStopStates) && t!=Token.EOF
        if length(intermediate)==1
            # Don't pursue the closure if there is just one state.
            # It can only have one alternative; just add to result
            # Also don't pursue the closure if there is unique alternative
            # among the configurations.
            reach = intermediate
        elseif getUniqueAlt(parseATNSimulator, intermediate)!=ATN.INVALID_ALT_NUMBER
            # Also don't pursue the closure if there is unique alternative
            # among the configurations.
            reach = intermediate
        end
    end

    # If the reach set could not be trivially determined, perform a closure
    # operation on the intermediate set to compute its initial value.
    #
    if isnothing(reach)
        reach = ATNConfigSet(fullCtx)
        closureBusy = set()
        treatEofAsEpsilon = t == Token.EOF
        for c in intermediate
            closure(parseATNSimulator, c, reach, closureBusy, false, fullCtx, treatEofAsEpsilon)
        end
    end

    if t == Token.EOF
        # After consuming EOF no additional input is possible, so we are
        # only interested in configurations which reached the end of the
        # decision rule (local context) || end of the start rule (full
        # context). Update reach to contain only these configurations. This
        # handles both explicit EOF transitions in the grammar and implicit
        # EOF transitions following the end of the decision || start rule.
        #
        # When reach==intermediate, no closure operation was performed. In
        # this case, removeAllConfigsNotInRuleStopState needs to check for
        # reachable rule stop states as well as configurations already in
        # a rule stop state.
        #
        # This is handled before the configurations in skippedStopStates,
        # because any configurations potentially added from that list are
        # already guaranteed to meet this condition whether || not it's
        # required.
        #
        reach = removeAllConfigsNotInRuleStopState(parseATNSimulator, reach, reach == intermediate)
    end

    # If skippedStopStates is not null, then it contains at least one
    # configuration. For full-context reach operations, these
    # configurations reached the end of the start rule, in which case we
    # only add them back to reach if no configuration during the current
    # closure operation reached such a state. This ensures adaptivePredict
    # chooses an alternative matching the longest overall sequence when
    # multiple alternatives are viable.
    #
    if !isnothing(skippedStopStates) &&  !fullCtx || (!hasConfigInRuleStopState(reach))
        for c in skippedStopStates
            add(reach, c, parserATNSimulator.mergeCache)
        end
    end

    if length(reach)==0
        return nothing
    else
        return reach
    end
end

#
# Return a configuration set containing only the configurations from
# {@code configs} which are in a {@link RuleStopState}. If all
# configurations in {@code configs} are already in a rule stop state, this
# method simply returns {@code configs}.
#
# <p>When {@code lookToEndOfRule} is true, this method uses
# {@link ATN#nextTokens} for each configuration in {@code configs} which is
# not already in a rule stop state to see if a rule stop state is reachable
# from the configuration via epsilon-only transitions.</p>
#
# @param configs the configuration set to update
# @param lookToEndOfRule when true, this method checks for rule stop states
# reachable by epsilon-only transitions from each configuration in
# {@code configs}.
#
# @return {@code configs} if all configurations in {@code configs} are in a
# rule stop state, otherwise return a new configuration set containing only
# the configurations from {@code configs} which are in a rule stop state
#
function removeAllConfigsNotInRuleStopState(parserATNSimulator::ParserATNSimulator, configs::ATNConfigSet, lookToEndOfRule::Bool)
    if allConfigsInRuleStopStates(configs)
        return configs
    end
    result = ATNConfigSet(configs.fullCtx)
    for config in configs
        if config.stat isa RuleStopState
            add(result, config, parserATNSimulator.mergeCache)
            continue
        end
        if lookToEndOfRule && config.state.epsilonOnlyTransitions
            nextTokens = nextTokens(parserATNSimulator.atn, config.state)
            if Token.EPSILON in nextTokens
                endOfRuleState = parserATNSimulator.atn.ruleToStopState[config.state.ruleIndex]
                result.add(ATNConfig(state=endOfRuleState, config=config), parserATNSimulator.mergeCache)
            end
        end
    end
    return result
end

function computeStartState(parserATNSimulator::ParserATNSimulator, p::ATNState, ctx::RuleContext, fullCtx::Bool)
    # always at least the implicit call to start rule
    initialContext = PredictionContextFromRuleContext(parserATNSimulator.atn, ctx)
    configs = ATNConfigSet(fullCtx)

    for i in 1:length(p.transitions)
        target = p.transitions[i].target
        c = ATNConfig(target, i+1, initialContext)
        closureBusy = set()
        closure(parseATNSimulator, c, configs, closureBusy, true, fullCtx, false)
    end
    return configs
end

#
# This method transforms the start state computed by
# {@link #computeStartState} to the special start state used by a
# precedence DFA for a particular precedence value. The transformation
# process applies the following changes to the start state's configuration
# set.
#
# <ol>
# <li>Evaluate the precedence predicates for each configuration using
# {@link SemanticContext#evalPrecedence}.</li>
# <li>Remove all configurations which predict an alternative greater than
# 1, for which another configuration that predicts alternative 1 is in the
# same ATN state with the same prediction context. This transformation is
# valid for the following reasons:
# <ul>
# <li>The closure block cannot contain any epsilon transitions which bypass
# the body of the closure, so all states reachable via alternative 1 are
# part of the precedence alternatives of the transformed left-recursive
# rule.</li>
# <li>The "primary" portion of a left recursive rule cannot contain an
# epsilon transition, so the only way an alternative other than 1 can exist
# in a state that is also reachable via alternative 1 is by nesting calls
# to the left-recursive rule, with the outer calls not being at the
# preferred precedence level.</li>
# </ul>
# </li>
# </ol>
#
# <p>
# The prediction context must be considered by this filter to address
# situations like the following.
# </p>
# <code>
# <pre>
# grammar TA;
# prog: statement* EOF;
# statement: letterA | statement letterA 'b' ;
# letterA: 'a';
# </pre>
# </code>
# <p>
# If the above grammar, the ATN state immediately before the token
# reference {@code 'a'} in {@code letterA} is reachable from the left edge
# of both the primary and closure blocks of the left-recursive rule
# {@code statement}. The prediction context associated with each of these
# configurations distinguishes between them, and prevents the alternative
# which stepped out to {@code prog} (and then back in to {@code statement}
# from being eliminated by the filter.
# </p>
#
# @param configs The configuration set computed by
# {@link #computeStartState} as the start state for the DFA.
# @return The transformed configuration set representing the start state
# for a precedence DFA at a particular precedence level (determined by
# calling {@link Parser#getPrecedence}).
#
function applyPrecedenceFilter(parserATNSimulator::ParserATNSimulator, configs::ATNConfigSet)
    statesFromAlt1 = dict()
    configSet = ATNConfigSet(configs.fullCtx)
    for config in configs
        # handle alt 1 first
        if config.alt != 1
            continue
        end
        updatedContext = config.semanticContext.evalPrecedence(parserATNSimulator.parser, parserATNSimulator._outerContext)
        if isnothing(updatedContext)
            # the configuration was eliminated
            continue
        end

        statesFromAlt1[config.state.stateNumber] = config.context
        if !(updatedContext == config.semanticContext)
            add(configSet, ATNConfig(config=config, semantic=updatedContext), parserATNSimulator.mergeCache)
        else
            add(configSet, config, parserATNSimulator.mergeCache)
        end
    end
    for config in configs
        if config.alt == 1
            # already handled
            continue
        end

        # In the future, this elimination step could be updated to also
        # filter the prediction context for alternatives predicting alt>1
        # (basically a graph subtraction algorithm).
        #
        if not config.precedenceFilterSuppressed
            context = get(statesFromAlt1, config.state.stateNumber, nothing)
            if context==config.context
                # eliminated
                continue
            end
        end

        add(configSet, config, parserATNSimulator.mergeCache)
    end
    return configSet
end

function getReachableTarget(parserATNSimulator::ParserATNSimulator, trans::Transition, ttype::Int)
    if trans.matches(ttype, 0, parserATNSimulator.atn.maxTokenType)
        return trans.target
    else
        return nothing
    end
end

function getPredsForAmbigAlts(parserATNSimulator::ParserATNSimulator, ambigAlts::Set, configs::ATNConfigSet, nalts::Int)
    # REACH=[1|1|[]|0:0, 1|2|[]|0:1]
    # altToPred starts as an array of all null contexts. The entry at index i
    # corresponds to alternative i. altToPred[i] may have one of three values:
    #   1. null: no ATNConfig c is found such that c.alt==i
    #   2. SemanticContext.nothing: At least one ATNConfig c exists such that
    #      c.alt==i and c.semanticContext==SemanticContext.nothing. In other words,
    #      alt i has at least one unpredicated config.
    #   3. Non-nothing Semantic Context: There exists at least one, and for all
    #      ATNConfig c such that c.alt==i, c.semanticContext!=SemanticContext.nothing.
    #
    # From this, it is clear that nothing||anything==nothing.
    #
    altToPred = [nothing] * (nalts + 1)
    for c in configs
        if c.alt in ambigAlts
            altToPred[c.alt] = orContext(altToPred[c.alt], c.semanticContext)
        end
    end

    nPredAlts = 0
    for i in 1:nalts+1
        if isnothing(altToPred[i])
            altToPred[i] = SEMANTIC_CONTEXT_NONE
        elseif altToPred[i] != SEMANTIC_CONTEXT_NONE
            nPredAlts += 1
        end
    end

    # nonambig alts are null in altToPred
    if nPredAlts==0
        altToPred = nothing
    end
    if debug
        println("getPredsForAmbigAlts result " * str_list(altToPred))
    end
    return altToPred
end

function getPredicatePredictions(::ParserATNSimulator, ambigAlts::Set, altToPred::Vector{T}) where {T}
    pairs = []
    containsPredicate = false
    for i in 1:length(altToPred)
        pred = altToPred[i]
        # unpredicated is indicated by SemanticContext.nothing
        if !isnothing(ambigAlts) && i in ambigAlts
            pairs.append(PredPrediction(pred, i))
        end
        if pred != SEMANTIC_CONTENT_NONE
            containsPredicate = true
        end
    end

    if !containsPredicate
        return nothing
    end

    return pairs
end

#
# This method is used to improve the localization of error messages by
# choosing an alternative rather than throwing a
# {@link NoViableAltException} in particular prediction scenarios where the
# {@link #ERROR} state was reached during ATN simulation.
#
# <p>
# The default implementation of this method uses the following
# algorithm to identify an ATN configuration which successfully parsed the
# decision entry rule. Choosing such an alternative ensures that the
# {@link ParserRuleContext} returned by the calling rule will be complete
# and valid, and the syntax error will be reported later at a more
# localized location.</p>
#
# <ul>
# <li>If a syntactically valid path || paths reach the end of the decision rule and
# they are semantically valid if predicated, return the min associated alt.</li>
# <li>Else, if a semantically invalid but syntactically valid path exist
# || paths exist, return the minimum associated alt.
# </li>
# <li>Otherwise, return {@link ATN#INVALID_ALT_NUMBER}.</li>
# </ul>
#
# <p>
# In some scenarios, the algorithm described above could predict an
# alternative which will result in a {@link FailedPredicateException} in
# the parser. Specifically, this could occur if the <em>only</em> configuration
# capable of successfully parsing to the end of the decision rule is
# blocked by a semantic predicate. By choosing this alternative within
# {@link #adaptivePredict} instead of throwing a
# {@link NoViableAltException}, the resulting
# {@link FailedPredicateException} in the parser will identify the specific
# predicate which is preventing the parser from successfully parsing the
# decision rule, which helps developers identify and correct logic errors
# in semantic predicates.
# </p>
#
# @param configs The ATN configurations which were valid immediately before
# the {@link #ERROR} state was reached
# @param outerContext The is the \gamma_0 initial parser context from the paper
# || the parser stack at the instant before prediction commences.
#
# @return The value to return from {@link #adaptivePredict}, or
# {@link ATN#INVALID_ALT_NUMBER} if a suitable alternative was not
# identified and {@link #adaptivePredict} should report an error instead.
#
function getSynValidOrSemInvalidAltThatFinishedDecisionEntryRule(::ParserATNSimulator, configs::ATNConfigSet, outerContext::ParserRuleContext)
    semValidConfigs, semInvalidConfigs = splitAccordingToSemanticValidity(parseATNSimulator, configs, outerContext)
    alt = getAltThatFinishedDecisionEntryRule(parseATNSimulator, semValidConfigs)
    if alt!=ATN.INVALID_ALT_NUMBER # semantically/syntactically viable path exists
        return alt
    end
    # Is there a syntactically valid path with a failed pred?
    if length(semInvalidConfigs)>0
        alt = getAltThatFinishedDecisionEntryRule(parseATNSimulator, semInvalidConfigs)
        if alt!=ATN.INVALID_ALT_NUMBER # syntactically viable path exists
            return alt
        end
    end
    return ATN.INVALID_ALT_NUMBER
end

function getAltThatFinishedDecisionEntryRule(::ParserATNSimulator, configs::ATNConfigSet)
    alts = Set()
    for c in configs
        if c.reachesIntoOuterContext>0 || (c.state isa RuleStopState && c.context.hasEmptyPath())
            add(alts, c.alt)
        end
    end

    if length(alts)==0
        return ATN.INVALID_ALT_NUMBER
    else
        return minimum(alts)
    end
end

# Walk the list of configurations and split them according to
#  those that have preds evaluating to true/false.  If no pred, assume
#  true pred and include in succeeded set.  Returns Pair of sets.
#
#  Create a new set so as not to alter the incoming parameter.
#
#  Assumption: the input stream has been restored to the starting point
#  prediction, which is where predicates need to evaluate.
#
function splitAccordingToSemanticValidity(parserATNSimulator::ParserATNSimulator, configs::ATNConfigSet, outerContext::ParserRuleContext)
    succeeded = ATNConfigSet(configs.fullCtx)
    failed = ATNConfigSet(configs.fullCtx)
    for c in configs
        if c.semanticContext != SEMANTIC_CONTEXT_NONE
            predicateEvaluationResult = c.semanticContext.eval(parserATNSimulator.parser, outerContext)
            if predicateEvaluationResult
                add(succeeded, c)
            else
                add(failed, c)
            end
        else
            add(succeeded, c)
        end
    end
    return (succeeded,failed)
end

# Look through a list of predicate/alt pairs, returning alts for the
#  pairs that win. A {@code nothing} predicate indicates an alt containing an
#  unpredicated config which behaves as "always true." If !complete
#  then we stop at the first predicate that evaluates to true. This
#  includes pairs with null predicates.
#
function evalSemanticContext(parserATNSimulator::ParserATNSimulator, predPredictions::Vector, outerContext::ParserRuleContext, complete::Bool)
    predictions = Set()
    for pair in predPredictions
        if pair.pred == SEMANTIC_CONTEXT_NONE
            predictions.add(pair.alt)
            if not complete
                break
            end
            continue
        end
        predicateEvaluationResult = pair.pred.eval(parserATNSimulator.parser, outerContext)
        if debug || dfa_debug
            println("eval pred " * string(pair) * "=" * string(predicateEvaluationResult))
        end

        if predicateEvaluationResult
            if debug || dfa_debug
                println("PREDICT " * string(pair.alt))
            end
            predictions.add(pair.alt)
            if !complete
                break
            end
        end
    end
    return predictions
end


# TODO: If we are doing predicates, there is no point in pursuing
#     closure operations if we reach a DFA state that uniquely predicts
#     alternative. We will not be caching that DFA state and it is a
#     waste to pursue the closure. Might have to advance when we do
#     ambig detection thought :(
#

function closure(parserATNSimulator::ParserATNSimulator, config::ATNConfig, configs::ATNConfigSet, closureBusy::Set, collectPredicates::Bool, fullCtx::Bool, treatEofAsEpsilon::Bool)
    initialDepth = 0
    closureCheckingStopState(parseATNSimulator, config, configs, closureBusy, collectPredicates,
                                fullCtx, initialDepth, treatEofAsEpsilon)
end


function closureCheckingStopState(parserATNSimulator::ParserATNSimulator, config::ATNConfig, configs::ATNConfigSet, closureBusy::Set, collectPredicates::Bool, fullCtx::Bool, depth::Int, treatEofAsEpsilon::Bool)
    if debug
        println("closure(" * string(config) * ")")
    end

    if config.state isa RuleStopState
        # We hit rule end. If we have context info, use it
        # run thru all possible stack tops in ctx
        if !isEmpty(config.context.isEmpty)
            for i in 1:length(config.context)
                state = getReturnState(config.context, i)
                if state == PredictionContext.EMPTY_RETURN_STATE
                    if fullCtx
                        configs.add(ATNConfig(state=config.state, context=PredictionContext.EMPTY, config=config), parserATNSimulator.mergeCache)
                        continue
                    else
                        # we have no context info, just chase follow links (if greedy)
                        if debug
                            println("FALLING off rule " * getRuleName(parseATNSimulator, config.state.ruleIndex))
                        end
                        closure_(parserATNSimulator, config, configs, closureBusy, collectPredicates,
                                    fullCtx, depth, treatEofAsEpsilon)
                    end
                    continue
                end
                returnState = parserATNSimulator.atn.states[state]
                newContext = parserATNSimulator(config.context, i) # "pop" return state
                c = ATNConfig(state=returnState, alt=config.alt, context=newContext, semantic=config.semanticContext)
                # While we have context to pop back from, we may have
                # gotten that context AFTER having falling off a rule.
                # Make sure we track that we are now out of context.
                c.reachesIntoOuterContext = config.reachesIntoOuterContext
                closureCheckingStopState(parseATNSimulator, c, configs, closureBusy, collectPredicates, fullCtx, depth - 1, treatEofAsEpsilon)
            end
            return
        elseif fullCtx
            # reached end of start rule
            configs.add(config, parserATNSimulator.mergeCache)
            return
        else
            # else if we have no context info, just chase follow links (if greedy)
            if debug
                println("FALLING off rule " * getRuleName(parseATNSimulator, config.state.ruleIndex))
            end
        end
    end

    parserATNSimulator.closure_(config, configs, closureBusy, collectPredicates, fullCtx, depth, treatEofAsEpsilon)
end

# Do the actual work of walking epsilon edges#
function closure_(parserATNSimulator::ParserATNSimulator, config::ATNConfig, configs::ATNConfigSet, closureBusy::Set, collectPredicates::Bool, fullCtx::Bool, depth::Int, treatEofAsEpsilon::Bool)
    p = config.state
    # optimization
    if !p.epsilonOnlyTransitions
        ad(configs, config, parserATNSimulator.mergeCache)
        # make sure to not return here, because EOF transitions can act as
        # both epsilon transitions and non-epsilon transitions.
    end
    first = true
    for t in p.transitions
        if first
            first = false
            if canDropLoopEntryEdgeInLeftRecursiveRule(parseATNSimulator, config)
                continue
            end
        end

        continueCollecting = collectPredicates && !(t isa ActionTransition)
        c = getEpsilonTarget(parseATNSimulator, config, t, continueCollecting, depth == 0, fullCtx, treatEofAsEpsilon)
        if !isnothing(c)
            newDepth = depth
            if ( config.state isa RuleStopState)
                # target fell off end of rule; mark resulting c as having dipped into outer context
                # We can't get here if incoming config was rule stop and we had context
                # track how far we dip into outer context.  Might
                # come in handy and we avoid evaluating context dependent
                # preds if this is > 0.
                if !isnothing(parserATNSimulator._dfa) && parserATNSimulator._dfa.precedenceDfa
                    if t.outermostPrecedenceReturn == parserATNSimulator._dfa.atnStartState.ruleIndex
                        c.precedenceFilterSuppressed = true
                    end
                end
                c.reachesIntoOuterContext += 1
                if c in closureBusy
                    # avoid infinite recursion for right-recursive rules
                    continue
                end
                add(closureBusy, c)
                configs.dipsIntoOuterContext = true # TODO: can remove? only care when we add to set per middle of this method
                newDepth -= 1
                if debug
                    println("dips into outer ctx: " * string(c))
                end
            else
                if not t.isEpsilon
                    if c in closureBusy
                        # avoid infinite recursion for EOF* and EOF+
                        continue
                    end
                    add(closureBusy, c)
                end
                if t isa RuleTransition
                    # latch when newDepth goes negative - once we step out of the entry context we can't return
                    if newDepth >= 0
                        newDepth += 1
                    end
                end
            end
            closureCheckingStopState(parseATNSimulator, c, configs, closureBusy, continueCollecting, fullCtx, newDepth, treatEofAsEpsilon)
        end
    end
end



# Implements first-edge (loop entry) elimination as an optimization
#  during closure operations.  See antlr/antlr4#1398.
#
# The optimization is to avoid adding the loop entry config when
# the exit path can only lead back to the same
# StarLoopEntryState after popping context at the rule end state
# (traversing only epsilon edges, so we're still in closure, in
# this same rule).
#
# We need to detect any state that can reach loop entry on
# epsilon w/o exiting rule. We don't have to look at FOLLOW
# links, just ensure that all stack tops for config refer to key
# states in LR rule.
#
# To verify we are in the right situation we must first check
# closure is at a StarLoopEntryState generated during LR removal.
# Then we check that each stack top of context is a return state
# from one of these cases:
#
#   1. 'not' expr, '(' type ')' expr. The return state points at loop entry state
#   2. expr op expr. The return state is the block end of internal block of (...)*
#   3. 'between' expr 'and' expr. The return state of 2nd expr reference.
#      That state points at block end of internal block of (...)*.
#   4. expr '?' expr ':' expr. The return state points at block end,
#      which points at loop entry state.
#
# If any is true for each stack top, then closure does not add a
# config to the current config set for edge[1], the loop entry branch.
#
#  Conditions fail if any context for the current config is
#
#   a. empty (we'd fall out of expr to do a global FOLLOW which could
#      even be to some weird spot in expr) or,
#   b. lies outside of expr or,
#   c. lies within expr but at a state not the BlockEndState
#   generated during LR removal
#
# Do we need to evaluate predicates ever in closure for this case?
#
# No. Predicates, including precedence predicates, are only
# evaluated when computing a DFA start state. I.e., only before
# the lookahead (but not parser) consumes a token.
#
# There are no epsilon edges allowed in LR rule alt blocks || in
# the "primary" part (ID here). If closure is in
# StarLoopEntryState any lookahead operation will have consumed a
# token as there are no epsilon-paths that lead to
# StarLoopEntryState. We do not have to evaluate predicates
# therefore if we are in the generated StarLoopEntryState of a LR
# rule. Note that when making a prediction starting at that
# decision point, decision d=2, compute-start-state performs
# closure starting at edges[1], edges[1] emanating from
# StarLoopEntryState. That means it is not performing closure on
# StarLoopEntryState during compute-start-state.
#
# How do we know this always gives same prediction answer?
#
# Without predicates, loop entry and exit paths are ambiguous
# upon remaining input +b (in, say, a+b). Either paths lead to
# valid parses. Closure can lead to consuming + immediately || by
# falling out of this call to expr back into expr and loop back
# again to StarLoopEntryState to match +b. In this special case,
# we choose the more efficient path, which is to take the bypass
# path.
#
# The lookahead language has not changed because closure chooses
# one path over the other. Both paths lead to consuming the same
# remaining input during a lookahead operation. If the next token
# is an operator, lookahead will enter the choice block with
# operators. If it is not, lookahead will exit expr. Same as if
# closure had chosen to enter the choice block immediately.
#
# Closure is examining one config (some loopentrystate, some alt,
# context) which means it is considering exactly one alt. Closure
# always copies the same alt to any derived configs.
#
# How do we know this optimization doesn't mess up precedence in
# our parse trees?
#
# Looking through expr from left edge of stat only has to confirm
# that an input, say, a+b+c; begins with any valid interpretation
# of an expression. The precedence actually doesn't matter when
# making a decision in stat seeing through expr. It is only when
# parsing rule expr that we must use the precedence to get the
# right interpretation and, hence, parse tree.
#
# @since 4.6
#
function canDropLoopEntryEdgeInLeftRecursiveRule(parserATNSimulator::ParserATNSimulator, config)
    # return false
    p = config.state
    # First check to see if we are in StarLoopEntryState generated during
    # left-recursion elimination. For efficiency, also check if
    # the context has an empty stack case. If so, it would mean
    # global FOLLOW so we can't perform optimization
    # Are we the special loop entry/exit state? || SLL wildcard
    if p.stateType != ATNState.STAR_LOOP_ENTRY  || !p.isPrecedenceDecision || 
            isEmpty(config.context) || hasEmptyPath(config.context)
        return false
    end

    # Require all return states to return back to the same rule
    # that p is in.
    numCtxs = length(config.context)
    for i in 1:numCtxs  # for each stack context
        returnState = parserATNSimulator.atn.states[config.context.getReturnState(i)]
        if returnState.ruleIndex != p.ruleIndex
            return false
        end
    end
    decisionStartState = p.transitions[1].target
    blockEndStateNum = decisionStartState.endState.stateNumber
    blockEndState = parserATNSimulator.atn.states[blockEndStateNum]

    # Verify that the top of each stack context leads to loop entry/exit
    # state through epsilon edges and w/o leaving rule.
    for i in 1:numCtxs  # for each stack context
        returnStateNumber = getReturnState(config.context, i)
        returnState = parserATNSimulator.atn.states[returnStateNumber]
        # all states must have single outgoing epsilon edge
        if length(returnState.transitions) != 1 || !returnState.transitions[1].isEpsilon
            return false
        end

        # Look for prefix op case like 'not expr', (' type ')' expr
        returnStateTarget = returnState.transitions[1].target
        if returnState.stateType == ATNState.BLOCK_END && returnStateTarget == p
            continue
        end

        # Look for 'expr op expr' || case where expr's return state is block end
        # of (...)* internal block; the block end points to loop back
        # which points to p but we don't need to check that
        if returnState == blockEndState
            continue
        end

        # Look for ternary expr ? expr : expr. The return state points at block end,
        # which points at loop entry state
        if returnStateTarget == blockEndState
            continue
        end

        # Look for complex prefix 'between expr and expr' case where 2nd expr's
        # return state points at block end state of (...)* internal block
        if returnStateTarget.stateType == ATNState.BLOCK_END && length(returnStateTarget.transitions) == 1  &&
            returnStateTarget.transitions[1].isEpsilon && returnStateTarget.transitions[1].target == p 
                continue
        end

        # anything else ain't conforming
        return false
    end

    return true
end


function getRuleName(parserATNSimulator::ParserATNSimulator, index::Int)
    if !isnothing(parserATNSimulator.parser) && index>=0
        return parserATNSimulator.parser.ruleNames[index]
    else
        return "<rule " * string(index) * ">"
    end
end

epsilonTargetMethods = Dict()
epsilonTargetMethods[TransitionConstant.RULE] = (sim, config, t, collectPredicates, inContext, fullCtx, treatEofAsEpsilon)->
    ruleTransition(sim, config, t)
epsilonTargetMethods[TransitionConstant.PRECEDENCE] = (sim, config, t, collectPredicates, inContext, fullCtx, treatEofAsEpsilon)->
    precedenceTransition(sim, config, t, collectPredicates, inContext, fullCtx)
epsilonTargetMethods[TransitionConstant.PREDICATE] = (sim, config, t, collectPredicates, inContext, fullCtx, treatEofAsEpsilon)->
    predTransition(sim, config, t, collectPredicates, inContext, fullCtx)
epsilonTargetMethods[TransitionConstant.ACTION] = (sim, config, t, collectPredicates, inContext, fullCtx, treatEofAsEpsilon)->
    actionTransition(sim, config, t)
epsilonTargetMethods[TransitionConstant.EPSILON] = (sim, config, t, collectPredicates, inContext, fullCtx, treatEofAsEpsilon)->
    ATNConfig(state=t.target, config=config)
epsilonTargetMethods[TransitionConstant.ATOM] = (sim, config, t, collectPredicates, inContext, fullCtx, treatEofAsEpsilon)->
    (treatEofAsEpsilon && matches(t, Token.EOF, 0, 1)) ? ATNConfig(state=t.target, config=config) : nothing
epsilonTargetMethods[TransitionConstant.RANGE] = (sim, config, t, collectPredicates, inContext, fullCtx, treatEofAsEpsilon)->
    treatEofAsEpsilon && matches(t, Token.EOF, 0, 1) ? ATNConfig(state=t.target, config=config) : nothing
epsilonTargetMethods[TransitionConstant.SET] = (sim, config, t, collectPredicates, inContext, fullCtx, treatEofAsEpsilon)->
    treatEofAsEpsilon && matches(t, Token.EOF, 0, 1) ? ATNConfig(state=t.target, config=config) : nothing

function getEpsilonTarget(parserATNSimulator::ParserATNSimulator, config::ATNConfig, t::Transition, collectPredicates::Bool, inContext::Bool, fullCtx::Bool, treatEofAsEpsilon::Bool)
    m = parserATNSimulator.epsilonTargetMethods.get(t.serializationType, nothing)
    if isnothing(m)
        return nothing
    else
        return m(parserATNSimulator::ParserATNSimulator, config, t, collectPredicates, inContext, fullCtx, treatEofAsEpsilon)
    end
end

function actionTransition(parserATNSimulator::ParserATNSimulator, config::ATNConfig, t::ActionTransition)
    if debug
        println("ACTION edge " * string(t.ruleIndex) * ":" * string(t.actionIndex))
    end
    return ATNConfig(state=t.target, config=config)
end

function precedenceTransition(parserATNSimulator::ParserATNSimulator, config::ATNConfig, pt::PrecedencePredicateTransition,  collectPredicates::Bool, inContext::Bool, fullCtx::Bool)
    if debug
        println("PRED (collectPredicates=" * string(collectPredicates) * ") " *
                string(pt.precedence) * ">=_p, ctx dependent=true")
        if !isnothing(parserATNSimulator.parser)
            println("context surrounding pred is " * string(parserATNSimulator.parser.getRuleInvocationStack()))
        end
    end

    c = nothing
    if collectPredicates && inContext
        if fullCtx
            # In full context mode, we can evaluate predicates on-the-fly
            # during closure, which dramatically reduces the size of
            # the config sets. It also obviates the need to test predicates
            # later during conflict resolution.
            currentPosition = parserATNSimulator._input.index
            parserATNSimulator._input.seek(parserATNSimulator._startIndex)
            predSucceeds = pt.getPredicate().eval(parserATNSimulator.parser, parserATNSimulator._outerContext)
            parserATNSimulator._seek(input, currentPosition)
           if predSucceeds
                c = ATNConfig(state=pt.target, config=config) # no pred context
           end
        else
            newSemCtx = andContext(config.semanticContext, pt.getPredicate())
            c = ATNConfig(state=pt.target, semantic=newSemCtx, config=config)
        end
    else
        c = ATNConfig(state=pt.target, config=config)
    end

    if debug
        println("config from pred transition=" * string(c))
    end
    return c
end

function predTransition(parserATNSimulator::ParserATNSimulator, config::ATNConfig, pt::PredicateTransition, collectPredicates::Bool, inContext::Bool, fullCtx::Bool)
    if debug
        println("PRED (collectPredicates=" * string(collectPredicates) * ") " * string(pt.ruleIndex) *
                ":" * string(pt.predIndex) * ", ctx dependent=" * string(pt.isCtxDependent))
        if !isnothing(parserATNSimulator.parser)
            println("context surrounding pred is " * string(parserATNSimulator.parser.getRuleInvocationStack()))
        end
    end

    c = nothing
    if collectPredicates && (!pt.isCtxDependent || (pt.isCtxDependent && inContext))
        if fullCtx
            # In full context mode, we can evaluate predicates on-the-fly
            # during closure, which dramatically reduces the size of
            # the config sets. It also obviates the need to test predicates
            # later during conflict resolution.
            currentPosition = parserATNSimulator._input.index
            parserATNSimulator._input.seek(parserATNSimulator._startIndex)
            predSucceeds = eval(getPredicate(pt), parserATNSimulator.parser, parserATNSimulator._outerContext)
            parserATNSimulator._seek(input, currentPosition)
            if predSucceeds
                c = ATNConfig(state=pt.target, config=config) # no pred context
            end
        else
            newSemCtx = andContext(config.semanticContext, getPredicate(pt))
            c = ATNConfig(state=pt.target, semantic=newSemCtx, config=config)
        end
    else
        c = ATNConfig(state=pt.target, config=config)
    end

    if debug
        println("config from pred transition=" * string(c))
    end
    return c
end

function ruleTransition(parserATNSimulator::ParserATNSimulator, config::ATNConfig, t::RuleTransition)
    if debug
        println("CALL rule " * getRuleName(parseATNSimulator, t.target.ruleIndex) * ", ctx=" * string(config.context))
    end
    returnState = t.followState
    newContext = SingletonPredictionContext.create(config.context, returnState.stateNumber)
    return ATNConfig(state=t.target, context=newContext, config=config )
end

function getConflictingAlts(parserATNSimulator::ParserATNSimulator, configs::ATNConfigSet)
    altsets = getConflictingAltSubsets(configs)
    return getAlts(altsets)
end

     # Sam pointed out a problem with the previous definition, v3, of
     # ambiguous states. If we have another state associated with conflicting
     # alternatives, we should keep going. For example, the following grammar
     #
     # s : (ID | ID ID?) ';' ;
     #
     # When the ATN simulation reaches the state before ';', it has a DFA
     # state that looks like: [12|1|[], 6|2|[], 12|2|[]]. Naturally
     # 12|1|[] and 12|2|[] conflict, but we cannot stop processing this node
     # because alternative to has another way to continue, via [6|2|[]].
     # The key is that we have a single state that has config's only associated
     # with a single alternative, 2, and crucially the state transitions
     # among the configurations are all non-epsilon transitions. That means
     # we don't consider any conflicts that include alternative 2. So, we
     # ignore the conflict between alts 1 and 2. We ignore a set of
     # conflicting alts when there is an intersection with an alternative
     # associated with a single alt state in the state&rarr;config-list map.
     #
     # It's also the case that we might have two conflicting configurations but
     # also a 3rd nonconflicting configuration for a different alternative:
     # [1|1|[], 1|2|[], 8|3|[]]. This can come about from grammar:
     #
     # a : A | A | A B ;
     #
     # After matching input A, we reach the stop state for rule A, state 1.
     # State 8 is the state right before B. Clearly alternatives 1 and 2
     # conflict and no amount of further lookahead will separate the two.
     # However, alternative 3 will be able to continue and so we do not
     # stop working on this state. In the previous example, we're concerned
     # with states associated with the conflicting alternatives. Here alt
     # 3 is not associated with the conflicting configs, but since we can continue
     # looking for input reasonably, I don't declare the state done. We
     # ignore a set of conflicting alts when we have an alternative
     # that we still need to pursue.
    #

function getConflictingAltsOrUniqueAlt(parserATNSimulator::ParserATNSimulator, configs::ATNConfigSet)
    conflictingAlts = nothing
    if configs.uniqueAlt!= ATN.INVALID_ALT_NUMBER
        conflictingAlts = set()
        add!(conflictingAlts, configs.uniqueAlt)
    else
        conflictingAlts = configs.conflictingAlts
    end
    return conflictingAlts
end

function getTokenName(parserATNSimulator::ParserATNSimulator, t::Int)
    if t==Token.EOF
        return "EOF"
    end
    if !isnothing(parserATNSimulator.parser) && !isnothing(parserATNSimulator.parser.literalNames) &&
        t < length(parserATNSimulator.parser.literalNames)
        return parserATNSimulator.parser.literalNames[t] * "<" * string(t) * ">"
    else
        return string(t)
    end
end
        

function getLookaheadName(parserATNSimulator::ParserATNSimulator, input::TokenStream)
    return getTokenName(parserATNSimulator, input.LA(1))
end

# Used for debugging in adaptivePredict around execATN but I cut
#  it out for clarity now that alg. works well. We can leave this
#  "dead" code for a bit.
#
function dumpDeadEndConfigs(parserATNSimulator::ParserATNSimulator, nvae::NoViableAltException)
    println("dead end configs: ")
    for c in nvae.getDeadEndConfigs()
        trans = "no edges"
        if length(c.state.transitions)>0
            t = c.state.transitions[1]
            if isinstance(t, AtomTransition)
                trans = "Atom "+ getTokenName(parseATNSimulator, t.label)
            elseif isinstance(t, SetTransition)
                neg = isinstance(t, NotSetTransition)
                trans = (neg ? "~" : "")+"Set "+ string(t.set)
            end
        end
        println(c.toString(parserATNSimulator.parser, true) * ":" * trans, file=sys.stderr)
    end
end

function noViableAlt(parserATNSimulator::ParserATNSimulator, input::TokenStream, outerContext::ParserRuleContext, configs::ATNConfigSet, startIndex::Int)
    return NoViableAltException(parserATNSimulator.parser, input, input.get(startIndex), input.LT(1), configs, outerContext)
end

function getUniqueAlt(parserATNSimulator::ParserATNSimulator, configs::ATNConfigSet)
    alt = ATN.INVALID_ALT_NUMBER
    for c in configs
        if alt == ATN.INVALID_ALT_NUMBER
            alt = c.alt # found first alt
        elseif c.alt!=alt
            return ATN.INVALID_ALT_NUMBER
        end
    end
    return alt
end

#
# Add an edge to the DFA, if possible. This method calls
# {@link #addDFAState} to ensure the {@code to} state is present in the
# DFA. If {@code from} is {@code null}, || if {@code t} is outside the
# range of edges that can be represented in the DFA tables, this method
# returns without adding the edge to the DFA.
#
# <p>If {@code to} is {@code null}, this method returns {@code null}.
# Otherwise, this method returns the {@link DFAState} returned by calling
# {@link #addDFAState} for the {@code to} state.</p>
#
# @param dfa The DFA
# @param from The source state for the edge
# @param t The input symbol
# @param to The target state for the edge
#
# @return If {@code to} is {@code null}, this method returns {@code null};
# otherwise this method returns the result of calling {@link #addDFAState}
# on {@code to}
#
function addDFAEdge(parserATNSimulator::ParserATNSimulator, dfa::DFA, from_::DFAState, t::Int, to::DFAState)
    if debug
        println("EDGE " * string(from_) * " -> " * string(to) * " upon " * getTokenName(parseATNSimulator, t))
    end

    if isnothing(to)
        return nothing
    end

    to = addDFAState(parseATNSimulator, dfa, to) # used existing if possible not incoming
    if isnothing(from_) || t < -1 || t > parserATNSimulator.atn.maxTokenType
        return to
    end

    if isnothing(from_.edges)
        from_.edges = [nothing] * (parserATNSimulator.atn.maxTokenType + 2)
    end
    from_.edges[t+1] = to # connect

    if debug
        names = isnothing(parserATNSimulator.parser) ? nothing : parserATNSimulator.parser.literalNames
        println("DFA=\n" * dfa.toString(names))
    end

    return to
end

#
# Add state {@code D} to the DFA if it is not already present, and return
# the actual instance stored in the DFA. If a state equivalent to {@code D}
# is already in the DFA, the existing state is returned. Otherwise this
# method returns {@code D} after adding it to the DFA.
#
# <p>If {@code D} is {@link #ERROR}, this method returns {@link #ERROR} and
# does not change the DFA.</p>
#
# @param dfa The dfa
# @param D The DFA state to add
# @return The state stored in the DFA. This will be either the existing
# state if {@code D} is already in the DFA, || {@code D} itparserATNSimulator if the
# state was not already present.
#
function addDFAState(parserATNSimulator::ParserATNSimulator, dfa::DFA, D::DFAState)
    if D == ERROR
        return D
    end


    existing = dfa.states.get(D, nothing)
    if !isnothing(existing)
        return existing
    end
    D.stateNumber = length(dfa.states)
    if not D.configs.readonly
        D.configs.optimizeConfigs(parserATNSimulator)
        D.configs.setReadonly(true)
    end
    dfa.states[D] = D
    if debug
        println("adding new DFA state: " * string(D))
    return D
    end
end

function reportAttemptingFullContext(parserATNSimulator::ParserATNSimulator, dfa::DFA, conflictingAlts::Set, configs::ATNConfigSet, startIndex::Int, stopIndex::Int)
    if debug || ParserATNSimulator.retry_debug
        println("reportAttemptingFullContext decision=" * string(dfa.decision) * ":" * string(configs) *
                            ", input=" * getText(getTokenStream(parserATNSimulator.parser),startIndex, stopIndex))
    end
    if !isnothing(parserATNSimulator.parser)
        parserATNSimulator.parser.getErrorListenerDispatch().reportAttemptingFullContext(parserATNSimulator.parser, dfa, startIndex, stopIndex, conflictingAlts, configs)
    end
end

function reportContextSensitivity(parserATNSimulator::ParserATNSimulator, dfa::DFA, prediction::Int, configs::ATNConfigSet, startIndex::Int, stopIndex::Int)
    if debug || retry_debug
        println("reportContextSensitivity decision=" * string(dfa.decision) * ":" * string(configs) *
                            ", input=" * parserATNSimulator.parser.getTokenStream().getText(startIndex, stopIndex))
    end
    if !isnothing(parserATNSimulator.parser)
        parserATNSimulator.parser.getErrorListenerDispatch().reportContextSensitivity(parserATNSimulator.parser, dfa, startIndex, stopIndex, prediction, configs)
    end
end

# If context sensitive parsing, we know it's ambiguity not conflict#
function reportAmbiguity(parserATNSimulator::ParserATNSimulator, dfa::DFA, D::DFAState, startIndex::Int, stopIndex::Int,
                                exact::Bool, ambigAlts::Set, configs::ATNConfigSet )
    if debug || ParserATNSimulator.retry_debug
#			ParserATNPathFinder finder = new ParserATNPathFinder(parser, atn);
#			int i = 1;
#			for (Transition t : dfa.atnStartState.transitions) {
#				println("ALT "+i+"=");
#				println(startIndex+".."+stopIndex+", length(input)="+parser.getInputStream().size());
#				TraceTree path = finder.trace(t.target, parser.getContext(), (TokenStream)parser.getInputStream(),
#											  startIndex, stopIndex);
#				if ( path!=null ) {
#					println("path = "+path.toStringTree());
#					for (TraceTree leaf : path.leaves) {
#						List<ATNState> states = path.getPathToNode(leaf);
#						println("states="+states);
#					}
#				}
#				i++;
#			}
        println("reportAmbiguity " * string(ambigAlts) * ":" * string(configs) +
                            ", input=" * parserATNSimulator.parser.getTokenStream().getText(startIndex, stopIndex))
    end
    if !isnothing(parserATNSimulator.parser)
        parserATNSimulator.parser.getErrorListenerDispatch().reportAmbiguity(parserATNSimulator.parser, dfa, startIndex, stopIndex, exact, ambigAlts, configs)
    end
end
