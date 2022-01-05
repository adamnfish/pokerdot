module Fixtures exposing (..)

import Expect
import Fuzz exposing (intRange)


startTimeGenerator =
    intRange 0 100000


todo =
    Expect.fail "Test is not implemented"
