#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.

# Provides an empty default implementation of {@link ANTLRErrorListener}. The
# default implementation of each method does nothing, but can be overridden as
# necessary.

abstract type ErrorListener end

struct ConsoleErrorListener <: ErrorListener
end

function syntaxError(::ConsoleErrorListener, recognizer, offendingSymbol, line, column, msg, e)
    println("line " + str(line) + ":" + str(column) + " " + msg)
end

instance(::ConsoleErrorListener) = ConsoleErrorListener()

struct ProxyErrorListener <: ErrorListener 
    delegates::Vector
    function ProxyErrorListener(delegates)
        if isnothing(delegates)
            ReferenceError("delegates")
        end
        new(delegates)
    end
end

function syntaxError(pel::ProxyErrorListener, recognizer::AbstractRecognizer, offendingSymbol, line::Int, column::Int, msg::String, e::Exception)
    for delegate in pel.delegates
        syntaxError(delegate, recognizer, offendingSymbol, line, column, msg, e)
    end
end

function reportAmbiguity(pel::ProxyErrorListener, recognizer::AbstractRecognizer, dfa::DFA, startIndex, stopIndex::Int, exact, ambigAlts, configs)
    for delegate in pel.delegates
        reportAmbiguity(delegate, recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs)
    end
end

function reportAttemptingFullContext(pel::ProxyErrorListener, recognizer::AbstractRecognizer, dfa::DFA, startIndex::Int, stopIndex::Int, conflictingAlts, configs)
    for delegate in pel.delegates
        reportAttemptingFullContext(delegate, recognizer, dfa, startIndex, stopIndex, conflictingAlts, configs)
    end
end

function reportContextSensitivity(pel::ProxyErrorListener, recognizer::AbstractRecognizer, dfa::DFA, startIndex::Int, stopIndex::Int, prediction, configs)
    for delegate in pel.delegates
        reportContextSensitivity(delegate, recognizer, dfa, startIndex, stopIndex, prediction, configs)
    end
end