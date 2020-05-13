{-# LANGUAGE FlexibleContexts #-}

module DrawBlocks
    ( drawBlocks
    , blocksToDiagram
    , blockToDiagram
    , Diagram
    , B
    ) where

import           Blocks
import           HelperFunctions

import           Data.List
import           Diagrams.Backend.SVG
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.Trail
import           Diagrams.TwoD.Layout.Grid
import           Diagrams.TwoD.Text

rend = renderSVG "her.svg" (mkWidth (200 :: Double))

drawBlocks :: IO ()
drawBlocks = mainWith $ blocksToDiagram theblock

branchYes b = cat unit_X [b, textLeft]

branchYesElse b = cat unitX [branchYes b, textRight]

branchYesNo b = cat unit_Y [branchYes b, textBottom]

textRight :: Diagram B
textRight = alignBL $ text "No" # fontSize (local 0.5) <> pha
  where
    pha = setTrace emptyTrace $ phantom (rect 1.5 0.7 :: Diagram B)
    emptyTrace = getTrace $ (strutX 1 :: Diagram B)

textBottom :: Diagram B
textBottom = alignTL $ text "No" # fontSize (local 0.5) <> pha
  where
    pha = setTrace emptyTrace $ phantom (rect 1.5 0.7 :: Diagram B)
    emptyTrace = getTrace $ (strutY 1 :: Diagram B)

textLeft :: Diagram B
textLeft = alignBR $ text "yes" # fontSize (local 0.5) <> pha
  where
    pha = setTrace emptyTrace $ phantom (rect 1.5 0.7 :: Diagram B)
    emptyTrace = getTrace $ (strutX 1 :: Diagram B)

polU level left = comb level (branchYesNo c) (left # named (level, "L"))

pol1 = polU 1 sanina2

saninaU level left =
    combWithElse level (branchYesElse c) (left # named (level, "L")) (innerScheme level "R")

sanina1 = saninaU 1 sanina2

sanina2 = saninaU 2 sanina3

sanina3 = saninaU 3 $ innerScheme 3 "R"

hergavno :: Diagram B
hergavno =
    vcatConnect
        (map (\x -> x ss)
             [ comment ss . terminator
             , ioScheme
             , anyAction
             , branchBlock
             , callBlock
             , loopStart
             , loopEnd
             ])
        "1"
        "M"

blocksToDiagram :: [Block] -> Diagram B
blocksToDiagram bs = hcat $ intersperse (strutX 10) ds
  where
    ds = map (\x -> blockToDiagram x 1 "M") bs

blockToDiagram :: Block -> Int -> String -> Diagram B
blockToDiagram b level s = vcatConnect (blockToDiagrams b level) level s

blockToDiagrams :: Block -> Int -> [Diagram B]
blockToDiagrams (Blocks.Start (Just s) next) _ =
    (terminator "Start" # comment s) : blockToDiagrams next 1
blockToDiagrams (Blocks.Start (Nothing) next) _ = (terminator "Start") : blockToDiagrams next 1
blockToDiagrams (IOBlock s next) level = (ioScheme s) : blockToDiagrams next level
blockToDiagrams (AssignBlock s next) level = (anyAction s) : blockToDiagrams next level
blockToDiagrams (Procedure s next) level = (callBlock s) : blockToDiagrams next level
blockToDiagrams (LoopBlock s next) level = (loopStart s) : blockToDiagrams next level
blockToDiagrams (LoopEnd s next) level = (loopEnd s) : blockToDiagrams next level
blockToDiagrams (Next) _ = []
blockToDiagrams (Blocks.End (Just s)) _ = (terminator s) : []
blockToDiagrams (Blocks.End (Nothing)) _ = (terminator "End") : []
blockToDiagrams (Decision s left (Just right) next) level =
    (combWithElse
         level
         (branchYesElse $ branchBlock s)
         (blockToDiagram left (level + 1) "L")
         (blockToDiagram right (level + 1) "R")) :
    blockToDiagrams next level
blockToDiagrams (Decision s left (Nothing) next) level =
    (comb level (branchYesNo $ branchBlock s) (blockToDiagram left (level + 1) "L")) :
    blockToDiagrams next level

combWithElse :: Int -> Diagram B -> Diagram B -> Diagram B -> Diagram B
combWithElse level top left right =
    comb'' # connect90deg True (level, "top") (level, "leftBottom") #
    connect90deg False (level, "leftBottom") (level, "dot") #
    connect90deg True (level, "top") (level, "rightBottom") #
    connect90deg False (level, "rightBottom") (level, "dot")
  where
    comb'' = cat (r2 (0, -1)) [comb', strut (r2 (0, -1)), circle 0.01 # named (level, "dot")]
    leftAndRight = 
        centerX $
        cat
            (unitX)
            [left # named (level, "leftBottom"), strutX 4, right # named (level, "rightBottom")]
    comb' = cat (r2 (0, -1)) [top # named (level, "top"), strutY 1, leftAndRight]

comb :: Int -> Diagram B -> Diagram B -> Diagram B
comb level top left =
    comb'' # connect90deg True (level, "top") (level, "leftBottom") #
    connect90deg False (level, "leftBottom") (level, "dot") #
    connectOutsideL (level, "top") (level, "dot")
  where
    comb'' = cat (r2 (0, -1)) [comb', strut (r2 (0, -1)), circle 0.01 # named (level, "dot")]
    comb' =
        cat
            (r2 (-1, -1))
            [ top # named (level, "top")
            , strut (r2 (-1, -1))
            , alignR $ left # named (level, "leftBottom")
            ]

comment :: String -> Diagram B -> Diagram B
comment s d =
    cat (r2 (1, 0))
        [ d
        , hrule 0.5 # dashingL (replicate 5 0.1) 0
        , commentPart <> alignedTextC s
        , phantom ((rect 2.8 1) :: Diagram B)
        ]
  where
    alignedTextC s = fontSize (local (3 / fromIntegral (length (s)))) $ alignedText 0 0.5 s
    commentPart =
        centerXY $ stroke $ lineFromVertices [0.2 ^& 0, 0 ^& 0, 0 ^& 0.5, 0 ^& 1, 0.2 ^& 1]

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

--a = text "start" <> circle 1 # scaleY 0.5
ss = "kakoi-nibud = chemu-nibud"

--bb :: String -> Diagram B
--bb s = textC s <> rect 2 1
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
    case intPoint of
        Just p ->
            (d <> intPoint' p) # connectOutsideL' (with & arrowHead .~ lineHead) n1 "intPoint" #
            connectOutsideL "intPoint" n2
        Nothing -> d
  where
    intPoint =
        if b
            then getIntersectionPoint n2 n1 d
            else getIntersectionPoint n1 n2 d
    intPoint' x = position [(x, circle 0.001 # named "intPoint")]

connect90deg' ::
       (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
    => ArrowOpts n
    -> Bool
    -> n1
    -> n2
    -> QDiagram b V2 n Any
    -> QDiagram b V2 n Any
connect90deg' opts b n1 n2 d =
    case intPoint of
        Just p ->
            (d <> intPoint' p) # connectOutsideL' (opts & arrowHead .~ lineHead) n1 "intPoint" #
            connectOutsideL' opts "intPoint" n2
        Nothing -> d
  where
    intPoint =
        if b
            then getIntersectionPoint n2 n1 d
            else getIntersectionPoint n1 n2 d
    intPoint' x = position [(x, circle 0.001 # applyStyle (_shaftStyle opts) # named "intPoint")]

--diamond x = square x # rotate (45 @@ deg)
--scheme :: Diagram B
--scheme = vcat (intersperse (strutY 1) [a, b, comb, a])
defaultNames = map (\c -> c : []) ['A' .. 'Z']

pointsNinetyDegreeAngleTrail :: [Point V2 Double]
pointsNinetyDegreeAngleTrail = trailVertices $ fromOffsets . map r2 $ [(1, 0), (0, -3)]

ninetyDegreeAngleTrail :: Trail V2 Double
ninetyDegreeAngleTrail = fromOffsets . map r2 $ [(-1, 0), (0, -1)]

visPoints :: [P2 Double] -> Diagram B
visPoints pts = atPoints pts (repeat (circle 0.05 # lw none # fc blue))

zipName :: (IsName nm1, IsName nm2) => [Diagram B] -> [nm1] -> nm2 -> [Diagram B]
zipName ds nms lastName = go ds nms
  where
    go [] _          = []
    go (x:[]) _      = named lastName x : []
    go _ []          = []
    go (x:xs) (y:ys) = named y x : go xs ys

connectOutsideList :: (IsName nm1, IsName nm2) => Int -> [nm1] -> nm2 -> Diagram B -> Diagram B
connectOutsideList 1 _ _ d = d
connectOutsideList 2 (n1:_) lastName d = connectOutsideL n1 lastName $ d
connectOutsideList _ [] _ d = d
connectOutsideList _ (_:[]) _ d = d
connectOutsideList n (n1:n2:ns) lastName d =
    connectOutsideL n1 n2 $ connectOutsideList (n - 1) (n2 : ns) lastName d

vcatConnect :: (IsName nm1, IsName nm2) => [Diagram B] -> nm1 -> nm2 -> Diagram B
vcatConnect ds levelName lastName =
    vcat (intersperse (strutY 1) namedDs) #
    connectOutsideList (length ds) namesList (levelName, lastName)
  where
    namedDs = zipName ds namesList (levelName, lastName)
    namesList = map (\x -> (levelName, x)) [1 :: Int,2 ..]

connectOutsideL ::
       (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
    => n1
    -> n2
    -> QDiagram b V2 n Any
    -> QDiagram b V2 n Any
connectOutsideL = connectOutside' (with & headLength .~ (local 0.1))

connectOutsideL' ::
       (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
    => ArrowOpts n
    -> n1
    -> n2
    -> QDiagram b V2 n Any
    -> QDiagram b V2 n Any
connectOutsideL' opts = connectOutside' (opts & headLength .~ (local 0.1))

getPointX (P (V2 x _)) = x

getPointY (P (V2 _ y)) = y

getIntersectionPoint ::
       (Semigroup m, Floating a, Ord a, IsName n1, IsName n2)
    => n1
    -> n2
    -> QDiagram b V2 a m
    -> Maybe (Point V2 a)
getIntersectionPoint nameX nameY d =
    (\x y -> P $ V2 x y) <$> (fmap getPointX point1) <*> (fmap getPointY point2)
  where
    points = names d
    point1 =
        case lookup (toName nameX) points of
            Just (p:ps) -> (Just p)
            Nothing     -> Nothing
    point2 =
        case lookup (toName nameY) points of
            Just (p:ps) -> (Just p)
            Nothing     -> Nothing

innerScheme :: IsName a => Int -> a -> Diagram B
innerScheme level nm = vcatConnect [d, d, b, d] level nm
--connectedScheme :: Diagram B
--connectedScheme = vcatConnect [a, b, comb, d] (1 :: Int) "M"
    --visPoints $ interleave circlePoints diamondPoints
--circlePoints = trailVertices $ square 2
--diamondPoints = trailPoints $ circle 1
    --cat (r2 (-1, -1)) [(c # named "C" # showEnvelope), strut (r2 (-1, -1)), (d # named "D")] #
    --connectPerim' (with & arrowShaft .~ ninetyDegreeAngleTrail) "C" "D" (180 @@ deg) (90 @@ deg)
  --where
    --compTrail =
    --getBorderPoint nm v = case lookupName nm connectedScheme of
        --Just d -> envelopeP v d
    --scheme # connectOutsideL"A" "B" # connectOutsideL"B" "C" #
    --connectOutsideL"D" "E"
