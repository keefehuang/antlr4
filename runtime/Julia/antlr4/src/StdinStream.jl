struct StdinStream <: AbstractInputStream
    is::InputStream
    function StdinStream(encoding::String="ascii", errors::String="strict")
        bytes = buffer.read()
    end
end


class StdinStream(InputStream):
    def __init__(self, encoding:str='ascii', errors:str='strict') -> None:
        bytes = sys.stdin.buffer.read()
        data = codecs.decode(bytes, encoding, errors)
        super().__init__(data)
