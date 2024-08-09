module Internal.Mat exposing
    ( Mat(..)
    , new, identity, translate, rotateDeg, scaleXY
    , invert, mul, vecMul
    )

{-| 3x3 Matrices for homogeneous transformations.


# Types

@docs Mat


# Functions


## Construction

@docs new, identity, translate, rotateDeg, scaleXY


## Manipulation

@docs invert, mul, vecMul

-}


{-| A 3x3 matrix for 2D homogeneous transformations.

The matrix elements appear in a 3x3 matrix as follows:

    [ [ a, c, tx ]
    , [ b, d, ty ]
    , [ 0, 0, 1 ]
    ]

-}
type Mat
    = Mat
        { a : Float
        , b : Float
        , c : Float
        , d : Float
        , tx : Float
        , ty : Float
        }


{-| Construct a `Mat` from its components.
-}
new : Float -> Float -> Float -> Float -> Float -> Float -> Mat
new a b c d tx ty =
    Mat { a = a, b = b, c = c, d = d, tx = tx, ty = ty }


{-| Identity matrix.
-}
identity : Mat
identity =
    new 1 0 0 1 0 0


{-| Invert using a general algorithm.
-}
invert : Mat -> Mat
invert m =
    scalarDiv (adj m) (det m)


{-| Divide by a scalar.
-}
scalarDiv : Mat -> Float -> Mat
scalarDiv (Mat m) z =
    Mat
        { a = m.a / z
        , b = m.b / z
        , c = m.c / z
        , d = m.d / z
        , tx = m.tx / z
        , ty = m.ty / z
        }


{-| Compute the determinant.
-}
det : Mat -> Float
det (Mat m) =
    m.a * m.d - m.c * m.b


{-| Return the adjoint.
-}
adj : Mat -> Mat
adj (Mat m) =
    Mat
        { a = m.d
        , b = -m.b
        , c = -m.c
        , d = m.a
        , tx = m.c * m.ty - m.d * m.tx
        , ty = m.b * m.tx - m.a * m.ty
        }


{-| Multiply two matrices.
-}
mul : Mat -> Mat -> Mat
mul (Mat m1) (Mat m2) =
    Mat
        { a = m1.a * m2.a + m1.c * m2.b
        , b = m1.b * m2.a + m1.d * m2.b
        , c = m1.a * m2.c + m1.c * m2.d
        , d = m1.b * m2.c + m1.d * m2.d
        , tx = m1.a * m2.tx + m1.c * m2.ty + m1.tx
        , ty = m1.b * m2.tx + m1.d * m2.ty + m1.ty
        }


{-| Multiply a matrix by a homogeneous vector.
-}
vecMul : Mat -> ( Float, Float, Float ) -> ( Float, Float, Float )
vecMul (Mat m) ( x, y, w ) =
    ( m.a * x + m.c * y + m.tx * w
    , m.b * x + m.d * y + m.ty * w
    , w
    )


{-| Construct a new translation matrix.
-}
translate : ( Float, Float ) -> Mat
translate ( tx, ty ) =
    new 1 0 0 1 tx ty


{-| Construct a new rotation matrix.

This rotation is in degrees.

-}
rotateDeg : Float -> Mat
rotateDeg angDeg =
    let
        angRad =
            angDeg * pi / 180

        c =
            cos angRad

        s =
            sin angRad
    in
    new c s -s c 0 0


{-| Construct a new scaling matrix.
-}
scaleXY : ( Float, Float ) -> Mat
scaleXY ( sx, sy ) =
    new sx 0 0 sy 0 0
