"""
Performs a walk on the given parse tree starting at the root and going down recursively
with depth-first search. On each node, {@link ParseTreeWalker#enterRule} is called before
recursively walking down into child nodes, then
{@link ParseTreeWalker#exitRule} is called after the recursive call to wind up.
@param listener The listener used by the walker to process grammar rules
@param t The parse tree to be walked on
"""
function walk(listener::ParseTreeListener, t::ParseTree)
    if t isa ErrorNode
        visitErrorNode(listener, t)
        return
    elseif t isa TerminalNode
        visitTerminal(listener, t)
        return
    end
    println(typeof(t))
    enterRule(listener, t)
    for child in getChildren(t)
        walk(listener, child)
    end
    exitRule(listener, t)
end

#
# The discovery of a rule node, involves sending two events: the generic
# {@link ParseTreeListener#enterEveryRule} and a
# {@link RuleContext}-specific event. First we trigger the generic and then
# the rule specific. We to them in reverse order upon finishing the node.
#
function enterRule(listener::ParseTreeListener, r::RuleNode)
    """
    Enters a grammar rule by first triggering the generic event {@link ParseTreeListener#enterEveryRule}
    then by triggering the event specific to the given parse tree node
    @param listener The listener responding to the trigger events
    @param r The grammar rule containing the rule context
    """
    ctx = getRuleContext(r)
    enterEveryRule(listener, ctx)
    enterRule(ctx, listener)
end

function exitRule(listener::ParseTreeListener, r::RuleNode)
    """
    Exits a grammar rule by first triggering the event specific to the given parse tree node
    then by triggering the generic event {@link ParseTreeListener#exitEveryRule}
    @param listener The listener responding to the trigger events
    @param r The grammar rule containing the rule context
    """
    ctx = getRuleContext(r)
    exitRule(ctx, listener)
    exitEveryRule(listener, ctx)
end