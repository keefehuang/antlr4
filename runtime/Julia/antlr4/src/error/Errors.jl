# Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
# Use of this file is governed by the BSD 3-clause license that
# can be found in the LICENSE.txt file in the project root.
#

struct UnsupportedOperationException <: Exception
    var::String
end

struct IllegalStateException <: Exception
    var::String
end

struct CancellationException <: Exception
    var::String
end

abstract type RecognitionException <: Exception end