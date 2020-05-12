{-# LANGUAGE FlexibleContexts #-}

module DrawBlocks
    ( drawBlocks
    ) where

import           Data.List
import           Diagrams.Backend.SVG
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.Trail
import           Diagrams.TwoD.Layout.Grid
import           Diagrams.TwoD.Text
import           HelperFunctions

rend = renderSVG "her.svg" (mkWidth (200 :: Double))

drawBlocks :: IO ()
drawBlocks = mainWith $ hergavno

hergavno :: Diagram B
hergavno =
    vcatConnect
        (map (\x -> x ss) [terminator, ioScheme, anyAction, branchBlock, callBlock, loopStart, loopEnd])
        "1"

textC :: (TypeableFloat n, Renderable (Text n) b) => String -> QDiagram b V2 n Any
textC s = fontSize (local (3 / fromIntegral (length (s)))) $ text s

terminator :: String -> Diagram B
terminator s = roundedRect 3 1 0.5 <> textC s

ioScheme :: String -> Diagram B
ioScheme s = rect 3 1 # shearX 0.3 <> textC s

anyAction :: String -> Diagram B
anyAction s = rect 3 1 <> textC s

branchBlock :: String -> Diagram B
branchBlock s = square 2 # rotate (45 @@ deg) # scaleY 0.5 <> textC s

callBlock :: String -> Diagram B
callBlock s = rect 3 1 <> rect 3 1 # scaleX 0.9 <> textC s

loopStart :: String -> Diagram B
loopStart s = loopForm <> textC s

loopForm :: Diagram B
loopForm =
    centerXY $
    stroke $ closeLine $ lineFromVertices [0 ^& 0, 0 ^& 0.8, 0.2 ^& 1, 2.8 ^& 1, 3 ^& 0.8, 3 ^& 0]

loopEnd :: String -> Diagram B
loopEnd s = loopForm # reflectY <> textC s

a = text "start" <> circle 1 # scaleY 0.5

ss = "kakoi-nibud = chemu-nibud"

bb :: String -> Diagram B
bb s = textC s <> rect 2 1

b = textC ss <> rect 2 1

c = square 2 # rotate (45 @@ deg) # scaleY 0.5

d = rect 2 1

-- True for connecting above
-- False for connecting belove
connect90deg ::
       (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
    => Bool
    -> n1
    -> n2
    -> QDiagram b V2 n Any
    -> QDiagram b V2 n Any
connect90deg b n1 n2 d =
    (d <> intPoint) # connectOutside' (with & arrowHead .~ lineHead) n1 "intPoint" #
    connectOutside "intPoint" n2
  where
    intPoint =
        position
            [ ( if b
                    then getIntersectionPoint n2 n1 d
                    else getIntersectionPoint n1 n2 d
              , circle 0.001 # named "intPoint")
            ]

connect90deg' ::
       (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
    => ArrowOpts n
    -> Bool
    -> n1
    -> n2
    -> QDiagram b V2 n Any
    -> QDiagram b V2 n Any
connect90deg' opts b n1 n2 d =
    (d <> intPoint) # connectOutside' (opts & arrowHead .~ lineHead) n1 "intPoint" #
    connectOutside' opts "intPoint" n2
  where
    intPoint =
        position
            [ ( if b
                    then getIntersectionPoint n2 n1 d
                    else getIntersectionPoint n1 n2 d
              , circle 0.001 # applyStyle (_shaftStyle opts) # named "intPoint")
            ]

comb :: Diagram B
comb = comb'' # connect90deg True "top" "bottom" # connect90deg False "LAST" "dot"
  where
    comb'' = cat (r2 (0, -1)) [comb', strut (r2 (0, -1)), circle 0.01 # named "dot"]
    comb' = cat (r2 (-1, -1)) [c # named "top", strut (r2 (-1, -1)), innerScheme # named "bottom"]

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
zipName = go
  where
    go [] _          = []
    go (x:[]) _      = named "LAST" x : []
    go _ []          = []
    go (x:xs) (y:ys) = named y x : go xs ys

connectOutsideList :: IsName a => Int -> [a] -> Diagram B -> Diagram B
connectOutsideList 1 _ d          = d
connectOutsideList 2 (n1:_) d     = connectOutside n1 "LAST" $ d
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

getIntersectionPoint ::
       (Semigroup m, Floating a, Ord a, IsName n1, IsName n2)
    => n1
    -> n2
    -> QDiagram b V2 a m
    -> Point V2 a
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
innerScheme = vcatConnect [d, d, b, d] (2 :: Int)

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
