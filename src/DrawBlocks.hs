module DrawBlocks
    ( drawBlocks
    ) where

import           Data.List
import           Diagrams.Backend.SVG
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.Trail
import           Diagrams.TwoD.Layout.Grid
import           HelperFunctions

rend = renderSVG "her.svg" (mkWidth (200 :: Double))

drawBlocks :: IO ()
drawBlocks = mainWith connectedScheme

a = text "start" <> circle 1 # scaleY 0.5

b = text "kakoi-nibud = chemu-nibud" <> rect 2 1

c = square 2 # rotate (45 @@ deg) # scaleY 0.5

d = rect 2 1

comb :: Diagram B
comb =
    (comb'' <> intPoint <> intPoint2) # 
    connectOutside' (with & arrowHead .~ lineHead) "top" "intPoint" #
    connectOutside "intPoint" "bottom" #
    connectOutside' (with & arrowHead .~ lineHead) "last" "intPoint2" #
    connectOutside "intPoint2" "dot" 
  where
    comb'' = cat (r2 (0, -1)) [comb', strut (r2 (-1, -1)), circle 0.01 # named "dot"]
    intPoint2 = position [(getIntersectionPoint "last" "dot" comb'', circle 0.001 # named "intPoint2")]
    intPoint = position [(getIntersectionPoint "bottom" "top" comb', circle 0.001 # named "intPoint")]
    comb' =
        cat
            (r2 (-1, -1))
            [ c # named "top"
            , strut (r2 (-1, -1))
            , innerScheme # named "bottom"
            ]
        --atPoints
            --(trailVertices $ square 5 # rotate (45 @@ deg))
            --[d, c # named "C", b # named "B", circle 0.01]

--diamond x = square x # rotate (45 @@ deg)
scheme :: Diagram B
scheme = vcat (intersperse (strutY 1) [a, b, comb, a])

defaultNames = map (\c -> c : []) ['A' .. 'Z']

pointsNinetyDegreeAngleTrail :: [Point V2 Double]
pointsNinetyDegreeAngleTrail = trailVertices $ fromOffsets . map r2 $ [(1, 0), (0, -3)]

ninetyDegreeAngleTrail :: Trail V2 Double
ninetyDegreeAngleTrail = fromOffsets . map r2 $ [(-1, 0), (0, -1)]

visPoints :: [P2 Double] -> Diagram B
visPoints pts = atPoints pts (repeat (circle 0.05 # lw none # fc blue))

zipName :: (IsName a) => [Diagram B] -> [a] -> [Diagram B]
zipName = zipWith (flip named)

connectOutsideList :: IsName a => Int -> [a] -> Diagram B -> Diagram B
connectOutsideList 0 _ d          = d
connectOutsideList _ [] d         = d
connectOutsideList _ (_:[]) d     = d
connectOutsideList n (n1:n2:ns) d = connectOutside n1 n2 $ connectOutsideList (n - 1) (n2 : ns) d

vcatConnect :: IsName a => [Diagram B] -> a -> Diagram B
vcatConnect ds levelName =
    vcat (intersperse (strutY 1) namedDs) # connectOutsideList (length ds) namesList
  where
    namedDs = zipName ds namesList
    namesList = map (\x -> (levelName, x)) [1 :: Int,2 ..]

getPointX (P (V2 x _)) = x

getPointY (P (V2 _ y)) = y

getIntersectionPoint :: (Semigroup m, Floating a, Ord a, IsName nm) => nm -> nm -> QDiagram b V2 a m -> Point V2 a
getIntersectionPoint nameX nameY d = P $ V2 (getPointX point1) (getPointY point2)
  where
    points = names d
    point1 =
        case lookup (toName nameX) points of
            Just (p:ps) -> p
    point2 =
        case lookup (toName nameY) points of
            Just (p:ps) -> p

innerScheme :: Diagram B
innerScheme = vcatConnect [d, d, b, d # named "last"] (2 :: Int)

connectedScheme :: Diagram B
connectedScheme = vcatConnect [a, b, comb, d] (1 :: Int)
    --visPoints $ interleave circlePoints diamondPoints
--circlePoints = trailVertices $ square 2
--diamondPoints = trailPoints $ circle 1
    --cat (r2 (-1, -1)) [(c # named "C" # showEnvelope), strut (r2 (-1, -1)), (d # named "D")] #
    --connectPerim' (with & arrowShaft .~ ninetyDegreeAngleTrail) "C" "D" (180 @@ deg) (90 @@ deg)
  --where
    --compTrail =
    --getBorderPoint nm v = case lookupName nm connectedScheme of
        --Just d -> envelopeP v d
    --scheme # connectOutside "A" "B" # connectOutside "B" "C" #
    --connectOutside "D" "E"
