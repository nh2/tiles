{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

module Main where

import           Data.List
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import           Graphics.Gloss.Interface.Pure.Game hiding (Vector)


_N :: Int
_N = 20

tileSize :: Int
tileSize = 20

tileDiff :: Int
tileDiff = tileSize + 1

initWinSize :: (Int, Int)
initWinSize = (_N * tileDiff, _N * tileDiff)


data World = World
  { tiles :: Vector (Vector Tile)
  , winSize :: (Int, Int)
  , hover :: (Int, Int)
  , tileColor :: Color
  , tileType :: TileType
  }


initWorld :: World
initWorld = World
  { tiles = V.replicate _N (V.replicate _N (Small, makeColor 0.1 0.1 0.1 1))
  , winSize = initWinSize
  , hover = (0, 0)
  , tileColor = makeColor 1 0 0 1
  , tileType = Small
  }


data TileType = Small | Big (Int, Int)
  deriving (Eq, Ord, Show)

type Tile = (TileType, Color)


toList2 :: Vector (Vector a) -> [[a]]
toList2 = V.toList . V.map V.toList


asciiWorld :: World -> String
asciiWorld world =
  unlines [ unwords [ case t of Small -> "."
                                Big (a, b) -> show (a*3+b + 4)
                    | (t, _) <- row ]
          | row <- reverse . transpose . toList2 $ tiles world ]


render :: World -> Picture
render World{ tiles, hover, tileType } =
  -- Translate from Gloss's "0,0 is center" to "0,0 is bottom left corner"
  Translate (fi (-(_N - 1) * tileDiff) / 2) (fi (-(_N - 1) * tileDiff) / 2)
    . Pictures
    . (++ [renderTile hover (tileType, makeColor 1 1 0 1)])
    . concat
    . toList2
    $ V.imap (\i -> V.imap (\j -> renderTile (i, j))) tiles
  where
    fi = fromIntegral
    tileSize3 = tileDiff * 2 + tileSize

    renderTile :: (Int, Int) -> Tile -> Picture
    renderTile (i, j) tt = case tt of
      (Small, col) -> Translate (fi $ i * tileDiff) (fi $ j * tileDiff)
                        . Color col
                        $ Polygon (rectanglePath (fi tileSize) (fi tileSize))
      (Big (0, 0), col) -> Translate (fi $ i * tileDiff) (fi $ j * tileDiff)
                              . Color col
                              $ Polygon (rectanglePath (fi tileSize3) (fi tileSize3))
      (Big _, _) -> Blank


topleft :: (RealFrac a) => World -> (a, a) -> (Int, Int)
topleft World{ winSize = (wx, wy) } (x, y) = (floor x + (wx `div` 2), floor y + (wy `div` 2))


event :: Event -> World -> World
event ev world = case ev of
  EventMotion (topleft world -> (x, y)) -> world{ hover = (tileOf x, tileOf y) }
  EventResize (x, y) -> world{ winSize = (x, y) }
  EventKey (Char 'r') Down _ _ -> world{ tileColor = makeColor 1 0 0 1 }
  EventKey (Char 'g') Down _ _ -> world{ tileColor = makeColor 0.1 0.1 0.1 1 }
  EventKey (Char 'w') Down _ _ -> world{ tileColor = makeColor 0.9 0.9 0.9 1 }
  EventKey (Char '1') Down _ _ -> world{ tileType = Small }
  EventKey (Char '3') Down _ _ -> world{ tileType = Big (0, 0) }
  EventKey (MouseButton LeftButton) Down _ (topleft world -> (x, y)) ->
    let i = tileOf x
        j = tileOf y
        ts = tiles world
    in case tileType world of
         Small -> world{ tiles = ts // [(i, ts ! i // [(j, (Small, tileColor world))])] }
         Big _
           | 1 <= i          && 1 <= j &&
                  i < _N - 1 &&      j < _N - 1 ->
             -- ^ bounds check: must not be an outermost 1-tile
             let big = [ (i+di, j+dj, (Big (di, dj), tileColor world))
                       | di <- [-1,0,1]
                       , dj <- [-1,0,1]
                       ]
                 split = [ (i', j', (Small, col))
                         | (bi, bj, (Big _, _)) <- big
                         , (Big (bdi, bdj), col) <- [ts ! bi ! bj]
                         , di <- [-1,0,1]
                         , dj <- [-1,0,1]
                         , let i' = bi - bdi + di
                         , let j' = bj - bdj + dj
                         , abs (i - i') > 1 || abs (j - j') > 1 -- don't split ourselves
                         ]
             in world{ tiles = ts /// big /// split }
           | otherwise -> world
  _ -> world
  where
    tileOf pix = pix `quot` tileDiff


(///) :: Vector (Vector a) -> [(Int, Int, a)] -> Vector (Vector a)
(///) = foldl' (\v (i, j, a) -> v // [(i, v ! i // [(j, a)])])


step :: Float -> World -> World
step _time _world = error "shouldn't be called since we set the step time to 0 in `play`"


main :: IO ()
main =
  play
    (InWindow "Tiles" initWinSize (0, 0))
    (makeColor 0 0 0 1)
    0
    initWorld
    render
    event
    step