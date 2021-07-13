#
# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
 #

using SuperEnum

"""
    CHANNEL = 0     #The type of a {@link LexerChannelAction} action.
    CUSTOM = 1      #The type of a {@link LexerCustomAction} action.
    MODE = 2        #The type of a {@link LexerModeAction} action.
    MORE = 3        #The type of a {@link LexerMoreAction} action.
    POP_MODE = 4    #The type of a {@link LexerPopModeAction} action.
    PUSH_MODE = 5   #The type of a {@link LexerPushModeAction} action.
    SKIP = 6        #The type of a {@link LexerSkipAction} action.
    TYPE = 7        #The type of a {@link LexerTypeAction} action.
"""

@se LexerActionType CHANNEL CUSTOM MODE MORE POP_MODE PUSH_MODE SKIP TYPE
const LexerActionTypeEnum = LexerActionType.LexerActionTypeEnum

abstract type LexerAction end
#     actionType::LexerActionTypeEnum
#     isPositionDependent::Bool

#     function LexerAction(action::LexerActionTypeEnum)
#         new(action, false)
#     end
# end

Base.hash(la::LexerAction) = hash(la.actionType)

function Base.:(==)(self::T, other::T) where {T<:LexerAction}
    if self === other
        return true
    end

    for field in fieldnames(T)
        if getfield(self, field) != getfield(other, field)
            return false
        end
    end
    true
end

Base.:(==)(self::T, other::S) where {T,S <: LexerAction} = false

#
# Implements the {@code skip} lexer action by calling {@link Lexer#skip}.
#
# <p>The {@code skip} command does not have any parameters, so this action is
# implemented as a singleton instance exposed by {@link #INSTANCE}.</p>
struct LexerSkipAction <: LexerAction 
    actionType::LexerActionTypeEnum
    isPositionDependent::Bool

    function LexerSkipAction()
        new(LexerActionType.SKIP, false)
    end
end

execute(::LexerSkipAction, lexer::AbstractLexer) = skip(lexer)
Base.string(::LexerSkipAction) = "skip"

LEXER_SKIP_ACTION = LexerSkipAction()

#  Implements the {@code type} lexer action by calling {@link Lexer#setType}
# with the assigned type.
struct LexerTypeAction <: LexerAction
    actionType::LexerActionTypeEnum
    isPositionDependent::Bool
    type::Int

    function LexerTypeAction(type::Int)
        new(LexerActionType.TYPE, false, type)
    end
end

function Base.hash(la::LexerTypeAction)
    hash((la.actionType, la.type))
end

function exectue(la::LexerTypeAction, lexer::AbstractLexer)
    lexer.type = la.type
end

function Base.string(la::LexerTypeAction)
    return "type(" * string(la.type) * ")"
end

# Implements the {@code pushMode} lexer action by calling
# {@link Lexer#pushMode} with the assigned mode.
struct LexerPushModeAction <: LexerAction
    actionType::LexerActionTypeEnum
    isPositionDependent::Bool
    mode::Int

    function LexerPushModeAction(mode::Int)
        new(LexerActionType.PUSH_MODE, false, mode)
    end
end

# <p>This action is implemented by calling {@link Lexer#pushMode} with the
# value provided by {@link #getMode}.</p>
function execute(la::LexerPushModeAction, lexer::AbstractLexer)
    pushMode(lexer, la.mode)
end

function Base.hash(la::LexerPushModeAction)
    return hash((la.actionType, la.mode))
end

Base.string(la::LexerPushModeAction) = "pushMode(" * string(la.mode) * ")"


# Implements the {@code popMode} lexer action by calling {@link Lexer#popMode}.
#
# <p>The {@code popMode} command does not have any parameters, so this action is
# implemented as a singleton instance exposed by {@link #INSTANCE}.</p>
struct LexerPopModeAction <: LexerAction 
    actionType::LexerActionTypeEnum
    isPositionDependent::Bool

    function LexerPopModeAction()
        new(LexerActionType.POP_MODE, false)
    end
end

execute(::LexerPopModeAction, lexer::AbstractLexer) = popMode(lexer)

Base.string(la::LexerPopModeAction) = "popMode"

LEXER_POPMODE_ACTION = LexerPopModeAction()

# Implements the {@code more} lexer action by calling {@link Lexer#more}.
#
# <p>The {@code more} command does not have any parameters, so this action is
# implemented as a singleton instance exposed by {@link #INSTANCE}.</p>
struct LexerMoreAction <: LexerAction 
    actionType::LexerActionTypeEnum
    isPositionDependent::Bool

    function LexerMoreAction()
        new(LexerActionType.MORE, false)
    end
end

Base.string(::LexerMoreAction) = "more"

LEXER_MORE_ACTION = LexerMoreAction()

# Implements the {@code mode} lexer action by calling {@link Lexer#mode} with
# the assigned mode.
struct LexerModeAction <: LexerAction
    actionType::LexerActionTypeEnum
    isPositionDependent::Bool
    mode::Int

    function LexerModeAction(mode::Int)
        new(LexerTypeAction.ACTION, false, mode)
    end
end

# <p>This action is implemented by calling {@link Lexer#mode} with the
    # value provided by {@link #getMode}.</p>
execute(la::LexerModeAction, lexer::AbstractLexer) = mode(lexer, la.mode)
    
Base.hash(la::LexerModeAction) = hash((la.actionType, la.mode))

Base.string(la::LexerModeAction) = "mode(" * string(la.mode) * ")"

# Executes a custom lexer action by calling {@link Recognizer#action} with the
# rule and action indexes assigned to the custom action. The implementation of
# a custom action is added to the generated code for the lexer in an override
# of {@link Recognizer#action} when the grammar is compiled.
#
# <p>This class may represent embedded actions created with the <code>{...}</code>
# syntax in ANTLR 4, as well as actions created for lexer commands where the
# command argument could not be evaluated when the grammar was compiled.</p>

struct LexerCustomAction <: LexerAction
    actionType::LexerActionTypeEnum
    isPositionDependent::Bool
    ruleIndex::Int
    actionIndex::Int

    function LexerCustomAction(ruleIndex::Int, actionIndex::Int)
        new(LexerActionType.CUSTOM, false, ruleIndex, actionIndex)
    end
end

execute(la::LexerCustomAction, lexer::AbstractLexer) = action(lexer, la.ruleIndex, la.actionIndex)

Base.hash(la::LexerCustomAction) = hash((la.actionType, la.ruleIndex, la.actionIndex))

# Implements the {@code channel} lexer action by calling
# {@link Lexer#setChannel} with the assigned channel.
struct LexerChannelAction <: LexerAction
    actionType::LexerActionTypeEnum
    isPositionDependent::Bool
    channel::Int

    function LexerChannelAction(channel::Int)
        new(LexerActionType.CHANNEL, false, channel)
    end
end

    # <p>This action is implemented by calling {@link Lexer#setChannel} with the
    # value provided by {@link #getChannel}.</p>
function execute(la::LexerChannelAction, lexer::AbstractLexer)
        lexer._channel = la.channel
end

Base.hash(la::LexerChannelAction) = hash((la.actionType, la.channel))

Base.string(la::LexerChannelAction) = "channel(" * string(la.channel) * ")"

# This implementation of {@link LexerAction} is used for tracking input offsets
# for position-dependent actions within a {@link LexerActionExecutor}.
#
# <p>This action is not serialized as part of the ATN, and is only required for
# position-dependent lexer actions which appear at a location other than the
# end of a rule. For more information about DFA optimizations employed for
# lexer actions, see {@link LexerActionExecutor#append} and
# {@link LexerActionExecutor#fixOffsetBeforeMatch}.</p>
struct LexerIndexedCustomAction <: LexerAction
    actionType::LexerActionTypeEnum
    isPositionDependent::Bool
    offset::Int
    action::LexerAction

    # Constructs a new indexed custom action by associating a character offset
    # with a {@link LexerAction}.
    #
    # <p>Note: This class is only required for lexer actions for which
    # {@link LexerAction#isPositionDependent} returns {@code true}.</p>
    #
    # @param offset The offset into the input {@link CharStream}, relative to
    # the token start index, at which the specified lexer action should be
    # executed.
    # @param action The lexer action to execute at a particular offset in the
    # input {@link CharStream}.
    function LexerIndexedCustomAction(offset::Int, action::LexerAction)
        new(action.actionType, true, offset, action)
    end
end


# <p>This method calls {@link #execute} on the result of {@link #getAction}
# using the provided {@code lexer}.</p>
# assume the input stream position was properly set by the calling code
execute(la::LexerIndexedCustomAction, lexer::AbstractLexer) = execute(la.action, lexer)

Base.hash(la::LexerIndexedCustomAction) = hash((la.actionType, la.offset, la.action))
