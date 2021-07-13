module antlr4
using SuperEnum
import SuperEnum: @se
include("error/Errors.jl")
include("Utils.jl")
export 
    nInt,
    nBool,
    AbstractLexer,
    AbstractParser,
    AbstractListener
include("Token.jl")

include("tree/Chunk.jl")
include("tree/Tree.jl")
export 
    RuleNode
include("tree/RuleTagToken.jl")
include("tree/TokenTagToken.jl")
include("tree/TreeVisitor.jl")
export 
    ParseTreeVisitor

export 
    Token,
    CommonToken

include("InputStream.jl")
export 
    LA
include("CommonTokenFactory.jl")
include("BufferedTokenStream.jl")
export 
    TokenStream
include("CommonTokenStream.jl")
export
    CommonTokenStream
include("TokenStreamRewriter.jl")


export 
    InputStream
include("RuleContext.jl")
export 
    RuleContext,
    setAltNumber
include("ParserRuleContext.jl")
export
    ParserRuleContext,
    nParserRuleContext,
    ParserRuleContextVars,
    getChild,
    getChildren
include("Recognizer.jl")
include("IntervalSet.jl")

export 
    IntervalSet
    
include("Lexer.jl")
export 
    Lexer,
    checkVersion
include("ListTokenSource.jl")
include("PredictionContext.jl")

include("tree/TreeListener.jl")
export 
    ParseTreeListener
include("tree/TreeWalker.jl")
export 
    walk,
    enterRule,
    exitRule

include("atn/SemanticContext.jl")
include("atn/Transition.jl")
include("atn/ATNState.jl")
include("atn/ATNType.jl")
include("atn/LexerAction.jl")
include("atn/LexerActionExecutor.jl")
include("atn/ATNConfig.jl")
include("atn/ATNConfigSet.jl")
include("atn/PredictionMode.jl")
export 
    PredictionContextCache

include("atn/ATNDeserializationOptions.jl")
include("atn/ATNDeserializer.jl")
export
    ATNDeserializer,
    deserialize
include("error/RecognitionErrors.jl")

include("dfa/DFAState.jl")
include("dfa/DFA.jl")
export
    DFA
include("error/ErrorListener.jl")
include("error/ErrorStrategy.jl")
export 
    reportError,
    sync
include("error/DiagnosticErrorListener.jl")
export 
    defaultErrorStrategy,
    recoverInline
include("dfa/DFASerializer.jl")
include("dfa/DFAString.jl")

include("atn/ATNSimulator.jl")
include("atn/ATN.jl")
include("atn/LexerATNSimulator.jl")
export
    LexerATNSimulator
include("atn/ParserATNSimulator.jl")
export 
    ParserATNSimulator
include("tree/ParseTreePatternMatcher.jl")
include("tree/ParseTreePattern.jl")
include("tree/ParseTreeMatch.jl")
include("Parser.jl")
export
    Parser,
    enterRule,
    exitRule,
    enterOuterAlt
end # module
