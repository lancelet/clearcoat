module Suite exposing (suite)

import Internal.MatTest
import Test exposing (..)


suite : Test
suite =
    describe "All Tests"
        [ Internal.MatTest.suite
        ]
