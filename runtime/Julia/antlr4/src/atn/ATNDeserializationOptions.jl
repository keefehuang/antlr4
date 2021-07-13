# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.

struct ATNDeserializationOptions
    readOnly::Bool
    verifyATN::Bool
    generateRuleBypassTransitions::Bool
    function ATNDeserializationOptions(copyFrom::Union{ATNDeserializationOptions,Nothing} = nothing, verifyATN::Bool=true)
        if isnothing(copyFrom)
            generateRuleBypassTransitions = false
        else
            verifyATN = copyFrom.verifyATN
            generateRuleBypassTransitions = copyFrom.generateRuleBypassTransitions
        end
        new(false, verifyATN, generateRuleBypassTransitions)
    end
end

function setproperty!(atnd::ATNDeserializationOptions, s::Symbol, x::S) where {S}
    if atnd.readOnly
        error("The object is read only.")
    end
    setfield!(atnd, s, x)
end

default(::ATNDeserializationOptions) = ATNDeserializationOptions()
# ATNDeserializationOptions.defaultOptions = ATNDeserializationOptions()
