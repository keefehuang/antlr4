#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#


#
# This implementation of {@link ANTLRErrorListener} can be used to identify
# certain potential correctness and performance problems in grammars. "Reports"
# are made by calling {@link AbstractParser#notifyErrorListeners} with the appropriate
# message.
#
# <ul>
# <li><b>Ambiguities</b>: These are cases where more than one path through the
# grammar can match the input.</li>
# <li><b>Weak context sensitivity</b>: These are cases where full-context
# prediction resolved an SLL conflict to a unique alternative which equaled the
# minimum alternative of the SLL conflict.</li>
# <li><b>Strong (forced) context sensitivity</b>: These are cases where the
# full-context prediction resolved an SLL conflict to a unique alternative,
# <em>and</em> the minimum alternative of the SLL conflict was found to not be
# a truly viable alternative. Two-stage parsing cannot be used for inputs where
# this situation occurs.</li>
# </ul>

# from io import StringIO
# from antlr4 import AbstractParser, DFA
# from antlr4.atn.ATNConfigSet import ATNConfigSet
# from antlr4.error.ErrorListener import ErrorListener
struct DiagnosticErrorListener <: ErrorListener 
    # whether all ambiguities or only exact ambiguities are reported.
    exactOnly::Bool
    function DiagnosticErrorListener(exactOnly::Bool=true)
        new(exactOnly)
    end
end

function reportAmbiguity(del::DiagnosticErrorListener, recognizer::AbstractParser, dfa::DFA, startIndex::Int,
                    stopIndex::Int, exact::Bool, ambigAlts::Set, configs::ATNConfigSet)
    if del.exactOnly && !exact
        return
    end
    buf = IOBuffer()
    write(buf, "reportAmbiguity d=")
    write(buf, getDecisionDescription(del, recognizer, dfa))
    write(buf, ": ambigAlts=")
    write(buf, string(getConflictingAlts(del, ambigAlts, configs)))
    write(buf, ", input='")
    write(buf, getText(getTokenStream(recognizer), startIndex, stopIndex))
    write(buf, "'")
    notifyErrorListeners(recognizer, string(take!(buf)))
end


function reportAttemptingFullContext(del::DiagnosticErrorListener, recognizer::AbstractParser, dfa::DFA, startIndex::Int,
                    stopIndex::Int, conflictingAlts::Set, configs::ATNConfigSet)
    buf = IOBuffer()
    write(buf, "reportAttemptingFullContext d=")
    write(buf, getDecisionDescription(del, recognizer, dfa))
    write(buf, ", input='")
    write(buf, getText(getTokenStream(recognizer), startIndex, stopIndex))
    write(buf, "'")
    notifyErrorListeners(recognizer, string(take!(buf)))
end

function reportContextSensitivity(del::DiagnosticErrorListener, recognizer::AbstractParser, dfa::DFA, startIndex::Int,
                    stopIndex::Int, prediction::Int, configs::ATNConfigSet)
    buf = IOBuffer()
    write(buf, "reportContextSensitivity d=")
    write(buf, getDecisionDescription(del, recognizer, dfa))
    write(buf, ", input='")
    write(buf, getText(getTokenStream(recognizer), startIndex, stopIndex))
    write(buf, "'")
    notifyErrorListeners(recognizer, string(take!(buf)))
end

function getDecisionDescription(del::DiagnosticErrorListener, recognizer::AbstractParser, dfa::DFA)
    decision = dfa.decision
    ruleIndex = dfa.atnStartState.ruleIndex

    ruleNames = recognizer.ruleNames
    if ruleIndex < 0 || ruleIndex >= length(ruleNames)
        return string(decision)
    end

    ruleName = ruleNames[ruleIndex]
    if isnothing(ruleName) || length(ruleName)==0
        return string(decision)
    end

    return string(decision) * " (" * ruleName * ")"
end

#
# Computes the set of conflicting or ambiguous alternatives from a
# configuration set, if that information was not already provided by the
# parser.
#
# @param reportedAlts The set of conflicting or ambiguous alternatives, as
# reported by the parser.
# @param configs The conflicting or ambiguous configuration set.
# @return Returns {@code reportedAlts} if it is not {@code null}, otherwise
# returns the set of alternatives represented in {@code configs}.
#
function getConflictingAlts(del::DiagnosticErrorListener, reportedAlts::Set, configs::ATNConfigSet)
    if !isnothing(reportedAlts)
        return reportedAlts
    end

    result = Set()
    for config in configs
        add(result, config.alt)
    end

    return result
end
