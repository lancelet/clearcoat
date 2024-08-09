module Internal.XForm exposing (XForm(..))

{-| Transformations between coordinate systems.


# Types

@docs XForm

-}

import Internal.Mat as Mat exposing (Mat(..))


{-| Transformation between coordinate systems.

Both the forward transformation and the inverse transformation
are stored.

-}
type XForm
    = XForm
        { matFwd : Mat
        , matInv : Mat
        }


fromMatrix : Mat -> XForm
fromMatrix m =
    XForm { matFwd = m, matInv = Mat.invert m }


fwdPt : XForm -> ( Float, Float ) -> ( Float, Float )
fwdPt xform ( x, y ) =
    let
        ( xx, yy, w ) =
            fwdH xform ( x, y, 1 )
    in
    ( xx / w, yy / w )


invPt : XForm -> ( Float, Float ) -> ( Float, Float )
invPt xform ( x, y ) =
    let
        ( xx, yy, w ) =
            invH xform ( x, y, 1 )
    in
    ( xx / w, yy / w )


fwdH : XForm -> ( Float, Float, Float ) -> ( Float, Float, Float )
fwdH (XForm xform) pth =
    Mat.vecMul xform.matFwd pth


invH : XForm -> ( Float, Float, Float ) -> ( Float, Float, Float )
invH (XForm xform) pth =
    Mat.vecMul xform.matInv pth


invert : XForm -> XForm
invert (XForm xform) =
    XForm
        { matFwd = xform.matInv
        , matInv = xform.matFwd
        }


andThen : XForm -> XForm -> XForm
andThen (XForm xfa) (XForm xfb) =
    XForm
        { matFwd = Mat.mul xfa.matFwd xfb.matFwd
        , matInv = Mat.mul xfb.matInv xfa.matInv
        }


translate : ( Float, Float ) -> XForm
translate ( tx, ty ) =
    XForm { matFwd = Mat.translate ( tx, ty ), matInv = Mat.translate ( -tx, -ty ) }
