using Logging
io = open("log.txt", "w+")
logger = ConsoleLogger(io)
global_logger(logger)

using Base: parse_cache_header
include("./antlr4/src/antlr4.jl")
using .antlr4
include("qasm3Lexer.jl")
include("qasm3Parser.jl")
# include("qasm3Listener.jl")

open("adder.qasm", "r") do io
    r = read(io, String)
    lexer = qasm3Lexer(InputStream(r))
    stream = CommonTokenStream(lexer)
    parser = qasm3Parser(stream)
    program(parser)
#         listener = qasm3Listener()

        # println(length(p.parserrulevars.children))

        # walk(listener, p)

    # stream = CommonTokenStream(lexer)
    # parser = HelloParser(stream)
    # tree = parser.hi()
    # printer = HelloPrintListener()
    # walker = ParseTreeWalker()
    # walker.walk(printer, tree)
end