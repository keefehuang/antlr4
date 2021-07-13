#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#/

# Represents an executor for a sequence of lexer actions which traversed during
# the matching operation of a lexer rule (token).
#
# <p>The executor tracks position information for position-dependent lexer actions
# efficiently, ensuring that actions appearing only at the end of the rule do
# not cause bloating of the {@link DFA} created for the lexer.</p>


mutable struct LexerActionExecutor
    lexerActions::Vector{LexerAction}
    hashCode::UInt64

    # Caches the result of {@link #hashCode} since the hash code is an element
        # of the performance-critical {@link LexerATNConfig#hashCode} operation.
    function LexerActionExecutor(lexerActions::Vector{LexerAction})
        new(lexerActions, hash(join([string(la) for la in lexerActions])))
    end
end
const nLexerActionExecutor = Union{LexerActionExecutor,Nothing}

# Creates a {@link LexerActionExecutor} which executes the actions for
# the input {@code lexerActionExecutor} followed by a specified
# {@code lexerAction}.
#
# @param lexerActionExecutor The executor for actions already traversed by
# the lexer while matching a token within a particular
# {@link LexerATNConfig}. If this is {@code null}, the method behaves as
# though it were an empty executor.
# @param lexerAction The lexer action to execute after the actions
# specified in {@code lexerActionExecutor}.
#
# @return A {@link LexerActionExecutor} for executing the combine actions
# of {@code lexerActionExecutor} and {@code lexerAction}.
Base.push!(lae::LexerActionExecutor, lexerAction::LexerAction) = push!(lae.lexerActions, lexerAction)
    
# Creates a {@link LexerActionExecutor} which encodes the current offset
# for position-dependent lexer actions.
#
# <p>Normally, when the executor encounters lexer actions where
# {@link LexerAction#isPositionDependent} returns {@code true}, it calls
# {@link IntStream#seek} on the input {@link CharStream} to set the input
# position to the <em>end</em> of the current token. This behavior provides
# for efficient DFA representation of lexer actions which appear at the end
# of a lexer rule, even when the lexer rule matches a variable number of
# characters.</p>
#
# <p>Prior to traversing a match transition in the ATN, the current offset
# from the token start index is assigned to all position-dependent lexer
# actions which have not already been assigned a fixed offset. By storing
# the offsets relative to the token start index, the DFA representation of
# lexer actions which appear in the middle of tokens remains efficient due
# to sharing among tokens of the same length, regardless of their absolute
# position in the input stream.</p>
#
# <p>If the current executor already has offsets assigned to all
# position-dependent lexer actions, the method returns {@code this}.</p>
#
# @param offset The current offset to assign to all position-dependent
# lexer actions which do not already have offsets assigned.
#
# @return A {@link LexerActionExecutor} which stores input stream offsets
# for all position-dependent lexer actions.
#/
function fixOffsetBeforeMatch(lae::LexerActionExecutor, offset::Int)
    updatedLexerActions = LexerActions[]
    for i in 1:length(lae.lexerActions)
        if lae.lexerActions[i].isPositionDependent && !(la.lexerActions[i] isa LexerIndexedCustomAction)
            if isempty(updatedLexerActions)
                updatedLexerActions = append!(updatedLexerActions, lae.lexerActions)
            end
            updatedLexerActions[i] = LexerIndexedCustomAction(offset, self.lexerActions[i])
        end
    end

    if isempty(updatedLexerActions)
        return lae
    else
        return LexerActionExecutor(updatedLexerActions)
    end
end


# Execute the actions encapsulated by this executor within the context of a
# particular {@link Lexer}.
#
# <p>This method calls {@link IntStream#seek} to set the position of the
# {@code input} {@link CharStream} prior to calling
# {@link LexerAction#execute} on a position-dependent action. Before the
# method returns, the input position will be restored to the same position
# it was in when the method was invoked.</p>
#
# @param lexer The lexer instance.
# @param input The input stream which is the source for the current token.
# When this method is called, the current {@link IntStream#index} for
# {@code input} should be the start of the following token, i.e. 1
# character past the end of the current token.
# @param startIndex The token start index. This value may be passed to
# {@link IntStream#seek} to set the {@code input} position to the beginning
# of the token.
#/
function execute(lae::LexerActionExecutor, lexer::AbstractLexer, input::InputStream, startIndex::Int)
    requiresSeek = false
    stopIndex = input._index
    try
        for lexerAction in lae.lexerActions
            if lexerAction isa LexerIndexedCustomAction
                offset = lexerAction.offset
                seek(input, startIndex + offset)
                lexerAction = lexerAction.action
                requiresSeek = (startIndex + offset) != stopIndex
            elseif lexerAction.isPositionDependent
                seek(input, stopIndex)
                requiresSeek = false
            end
            execute(lexerAction, lexer)
        end
    finally
        if requiresSeek
            seek(input, stopIndex)
        end
    end
end

Base.hash(lae::LexerActionExecutor) = lae.hashCode

function Base.:(==)(self::LexerActionExecutor, other::LexerActionExecutor)
    if self === other
        return true
    end
    for field in fieldnames(LexerActionExecutor)
        if !(getfield(self, field) == getfield(other, field))
            return false
        end
    end
    true
end