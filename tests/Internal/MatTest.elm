module Internal.MatTest exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Internal.Mat as Mat exposing (Mat(..))
import Test exposing (..)


{-| -}
suite : Test
suite =
    describe "Internal.Matrix"
        [ invertIdentity
        , invertTranslate
        , invertRotate
        , invertScale
        , rot90
        ]


tol : FloatingPointTolerance
tol =
    AbsoluteOrRelative 1.0e-6 1.0e-6


aeq : Mat -> Mat -> Expectation
aeq =
    mat_aeq tol


genVec : Fuzzer ( Float, Float )
genVec =
    let
        genFloat =
            Fuzz.floatRange -3000 3000
    in
    Fuzz.pair genFloat genFloat


genRotateAngle : Fuzzer Float
genRotateAngle =
    Fuzz.floatRange (-4 * 360) (4 * 360)


genScaleFacs : Fuzzer ( Float, Float )
genScaleFacs =
    let
        genFloat =
            Fuzz.floatRange 0.05 100
    in
    Fuzz.pair genFloat genFloat


invertIdentity : Test
invertIdentity =
    test "inverting identity produces identity" <|
        \() -> aeq Mat.identity (Mat.invert Mat.identity)


rot90 : Test
rot90 =
    fuzz genVec "rotating 90 degrees has the expected effect on coordinates" <|
        \( x, y ) ->
            let
                rot =
                    Mat.rotateDeg 90

                expected =
                    ( -y, x, 1 )

                actual =
                    Mat.vecMul rot ( x, y, 1 )
            in
            pth_aeq tol expected actual


invertTranslate : Test
invertTranslate =
    fuzz
        genVec
        "inverting a translation produces the inverse translation"
    <|
        \( tx, ty ) ->
            let
                expected =
                    Mat.translate ( -tx, -ty )

                actual =
                    Mat.invert (Mat.translate ( tx, ty ))
            in
            aeq expected actual


invertRotate : Test
invertRotate =
    fuzz
        genRotateAngle
        "inverting a rotation produces the inverse rotation"
    <|
        \degrees ->
            let
                expected =
                    Mat.rotateDeg -degrees

                actual =
                    Mat.invert (Mat.rotateDeg degrees)
            in
            aeq expected actual


invertScale : Test
invertScale =
    fuzz
        genScaleFacs
        "inverting a scale produces the inverse scale"
    <|
        \( sx, sy ) ->
            let
                expected =
                    Mat.scaleXY ( 1 / sx, 1 / sy )

                actual =
                    Mat.invert (Mat.scaleXY ( sx, sy ))
            in
            aeq expected actual


{-| Compare two matrices for equality up to a supplied tolerance.
-}
mat_aeq : FloatingPointTolerance -> Mat -> Mat -> Expectation
mat_aeq tolerance expected actual =
    let
        cmp : (Mat -> Float) -> Mat -> Expectation
        cmp extract subject =
            Expect.within tolerance (extract expected) (extract subject)

        unMatrix (Mat m) =
            m
    in
    Expect.all
        [ cmp (unMatrix >> .a)
        , cmp (unMatrix >> .b)
        , cmp (unMatrix >> .c)
        , cmp (unMatrix >> .d)
        , cmp (unMatrix >> .tx)
        , cmp (unMatrix >> .ty)
        ]
        actual


{-| Compare two homogeneous points for equality.
-}
pth_aeq :
    FloatingPointTolerance
    -> ( Float, Float, Float )
    -> ( Float, Float, Float )
    -> Expectation
pth_aeq tolerance expected actual =
    let
        cmp : (( Float, Float, Float ) -> Float) -> ( Float, Float, Float ) -> Expectation
        cmp extract subject =
            Expect.within tolerance (extract expected) (extract subject)
    in
    Expect.all
        [ cmp (\( x, _, _ ) -> x), cmp (\( _, y, _ ) -> y), cmp (\( _, _, z ) -> z) ]
        actual
