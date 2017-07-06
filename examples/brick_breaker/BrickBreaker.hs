{- 

Brick Breaker - a very simple FunGEn example.
Copyright (C) 2017  Milorad Vojnovic <milekuglas@gmail.com>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Paths_FunGEn (getDataFileName)

data GameAttribute = GA Int Int Int Int Int Int String String
 
width = 400
height = 400
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

main :: IO ()
main = do
  texbmp <- getDataFileName "examples/brick_breaker/tex.bmp"
  let winConfig = ((100,80),(width,height),"A brief example!")
      bmpList = [(texbmp, Nothing)]
      gameMap = textureMap 0 30 30 w h
      bar     = objectGroup "barGroup"  [createBar]
      ball    = objectGroup "ballGroup" [createBall]
      bricksLvl1  = objectGroup "bricksGroupLvl1" createBricks
      bricksLvl2  = objectGroup "bricksGroupLvl2" createBricksLvl2
      bricksLvl3  = objectGroup "bricksGroupLvl3" createBricksLvl3
      bricksLvl4  = objectGroup "bricksGroupLvl4" createBricksLvl4
      bricksLvl5  = objectGroup "bricksGroupLvl5" createBricksLvl5
      initAttr = GA 3 0 35 1 100 50 "bricksGroupLvl1" "Level 1"
      input = [
        (SpecialKey KeyRight, StillDown, moveBarToRight)
        ,(SpecialKey KeyLeft,  StillDown, moveBarToLeft)
        ,(Char 'q',            Press,     \_ _ -> funExit)
        ]
  funInit winConfig gameMap [bar,ball,bricksLvl1,bricksLvl2,bricksLvl3,bricksLvl4,bricksLvl5] () initAttr input gameCycle (Timer 30) bmpList

createBall :: GameObject ()
createBall =
  let ballPic = Basic (Circle 6.0 0.0 1.0 0.0 Filled)
  in object "ball" ballPic False (20,20) (-6,6) ()

createBar :: GameObject ()
createBar =
  let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)]
      barPic   = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
  in object "bar" barPic False (w/2,30) (0,0) ()

createBrick :: Double -> Double -> Bool -> GameObject ()
createBrick x y sleep=
  let barBound = [(-20,-10),(20,-10),(20,10),(-20,10)]
      barPic   = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
  in object "brick" barPic sleep (x, y) (0,0) ()

createBricks :: [GameObject ()]
createBricks = [createBrick 335 300 False]++[createBrick 290 300 False]++[createBrick 245 300 False]++[createBrick 200 300 False]++[createBrick 155 300 False]++[createBrick 110 300 False]++[createBrick 65 300 False]
              ++[createBrick 335 275 False]++[createBrick 290 275 False]++[createBrick 245 275 False]++[createBrick 200 275 False]++[createBrick 155 275 False]++[createBrick 110 275 False]++[createBrick 65 275 False]
              ++[createBrick 335 250 False]++[createBrick 290 250 False]++[createBrick 245 250 False]++[createBrick 200 250 False]++[createBrick 155 250 False]++[createBrick 110 250 False]++[createBrick 65 250 False]
              ++[createBrick 335 225 False]++[createBrick 290 225 False]++[createBrick 245 225 False]++[createBrick 200 225 False]++[createBrick 155 225 False]++[createBrick 110 225 False]++[createBrick 65 225 False]
              ++[createBrick 335 200 False]++[createBrick 290 200 False]++[createBrick 245 200 False]++[createBrick 200 200 False]++[createBrick 155 200 False]++[createBrick 110 200 False]++[createBrick 65 200 False]

createBricksLvl2 :: [GameObject ()]
createBricksLvl2 = [createBrick 335 350 True]++[createBrick 290 350 True]++[createBrick 245 350 True]++[createBrick 200 350 True]++[createBrick 155 350 True]++[createBrick 110 350 True]++[createBrick 65 350 True]
              ++[createBrick 313 325 True]++[createBrick 268 325 True]++[createBrick 223 325 True]++[createBrick 178 325 True]++[createBrick 133 325 True]++[createBrick 88 325 True]
              ++[createBrick 291 300 True]++[createBrick 246 300 True]++[createBrick 201 300 True]++[createBrick 156 300 True]++[createBrick 111 300 True]
              ++[createBrick 269 275 True]++[createBrick 224 275 True]++[createBrick 180 275 True]++[createBrick 133 275 True]
              ++[createBrick 247 250 True]++[createBrick 203 250 True]++[createBrick 156 250 True]
              ++[createBrick 226 225 True]++[createBrick 179 225 True]
              ++[createBrick 200 200 True]
              ++[createBrick 335 175 True]++[createBrick 290 175 True]++[createBrick 245 175 True]++[createBrick 200 175 True]++[createBrick 155 175 True]++[createBrick 110 175 True]++[createBrick 65 175 True]
              
createBricksLvl3 :: [GameObject ()]
createBricksLvl3 =[createBrick 380 350 True]++[createBrick 335 350 True]++[createBrick 290 350 True]++[createBrick 245 350 True]++[createBrick 200 350 True]++[createBrick 155 350 True]++[createBrick 110 350 True]
              ++[createBrick 290 300 True]++[createBrick 245 300 True]++[createBrick 200 300 True]++[createBrick 155 300 True]++[createBrick 110 300 True]++[createBrick 65 300 True]++[createBrick 20 300 True]
              ++[createBrick 380 250 True]++[createBrick 335 250 True]++[createBrick 290 250 True]++[createBrick 245 250 True]++[createBrick 200 250 True]++[createBrick 155 250 True]++[createBrick 110 250 True]
              ++[createBrick 290 200 True]++[createBrick 245 200 True]++[createBrick 200 200 True]++[createBrick 155 200 True]++[createBrick 110 200 True]++[createBrick 65 200 True]++[createBrick 20 200 True]
              ++[createBrick 380 150 True]++[createBrick 335 150 True]++[createBrick 290 150 True]++[createBrick 245 150 True]++[createBrick 200 150 True]++[createBrick 155 150 True]++[createBrick 110 150 True]
              
createBricksLvl4 :: [GameObject ()]
createBricksLvl4 = [createBrick 335 350 True]++[createBrick 268 350 True]++[createBrick 201 350 True]++[createBrick 134 350 True]++[createBrick 65 350 True]
               ++[createBrick 335 325 True]++[createBrick 268 325 True]++[createBrick 201 325 True]++[createBrick 134 325 True]++[createBrick 65 325 True]
               ++[createBrick 335 300 True]++[createBrick 268 300 True]++[createBrick 201 300 True]++[createBrick 134 300 True]++[createBrick 65 300 True]
               ++[createBrick 335 275 True]++[createBrick 268 275 True]++[createBrick 201 275 True]++[createBrick 134 275 True]++[createBrick 65 275 True]
               ++[createBrick 335 250 True]++[createBrick 268 250 True]++[createBrick 201 250 True]++[createBrick 134 250 True]++[createBrick 65 250 True]
               ++[createBrick 335 225 True]++[createBrick 268 225 True]++[createBrick 201 225 True]++[createBrick 134 225 True]++[createBrick 65 225 True]
               ++[createBrick 335 200 True]++[createBrick 268 200 True]++[createBrick 201 200 True]++[createBrick 134 200 True]++[createBrick 65 200 True]

createBricksLvl5 :: [GameObject ()]
createBricksLvl5 =  [createBrick 335 300 True]++[createBrick 290 300 True]++[createBrick 245 300 True]++[createBrick 200 300 True]++[createBrick 155 300 True]++[createBrick 110 300 True]++[createBrick 65 300 True]
              ++[createBrick 335 275 True]++[createBrick 65 275 True]
              ++[createBrick 335 250 True]++[createBrick 290 250 True]++[createBrick 245 250 True]++[createBrick 200 250 True]++[createBrick 155 250 True]++[createBrick 110 250 True]++[createBrick 65 250 True]
              ++[createBrick 335 225 True]++[createBrick 200 225 True]++[createBrick 65 225 True]
              ++[createBrick 335 200 True]++[createBrick 290 200 True]++[createBrick 245 200 True]++[createBrick 200 200 True]++[createBrick 155 200 True]++[createBrick 110 200 True]++[createBrick 65 200 True]
              ++[createBrick 335 175 True]++[createBrick 65 175 True]
              ++[createBrick 335 150 True]++[createBrick 290 150 True]++[createBrick 245 150 True]++[createBrick 200 150 True]++[createBrick 155 150 True]++[createBrick 110 150 True]++[createBrick 65 150 True]

moveBarToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToRight _ _ = do
  obj     <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 8 <= w)
   then (setObjectPosition ((pX + 8),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveBarToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToLeft _ _ = do
  obj <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 8 >= 0)
    then (setObjectPosition ((pX - 8),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)

isLeftRight :: GameObject s -> GameObject s -> IOGame t s u v Bool
isLeftRight o1 o2 = do
    asleep1 <- getObjectAsleep o1
    asleep2 <- getObjectAsleep o2
    if (asleep1 || asleep2)
                then (return False)
                else  do (_,p1Y) <- getObjectPosition o1
                         (_,p2Y) <- getObjectPosition o2
                         (_,s2Y) <- getObjectSize o2
        
                         let bY1 = p2Y - (s2Y/2)
                             bY2 = p2Y + (s2Y/2)
                              
                         return ((p1Y > bY1) && (p1Y < bY2))
isUpDown :: GameObject s -> GameObject s -> IOGame t s u v Bool
isUpDown o1 o2 = do
    asleep1 <- getObjectAsleep o1
    asleep2 <- getObjectAsleep o2
    if (asleep1 || asleep2)
                then (return False)
                else  do (p1X,_) <- getObjectPosition o1
                         (p2X,_) <- getObjectPosition o2
                         (s2X,_) <- getObjectSize o2
        
                         let bX1 = p2X - (s2X/2)
                             bX2 = p2X + (s2X/2)
                              
                         return ((p1X > bX1) && (p1X < bX2))

getCollidedBrick :: [(GameObject s)] -> GameObject s -> IOGame t s u v (GameObject s)
getCollidedBrick [] b = return b
getCollidedBrick (a:[]) _ = return a
getCollidedBrick (a:as) b = do
        col <- objectsCollision a b
        if col
                then (return a)
                else (getCollidedBrick as b)

nextMap :: String -> String
nextMap map 
  | map == "bricksGroupLvl1" = "bricksGroupLvl2"
  | map == "bricksGroupLvl2" = "bricksGroupLvl3"
  | map == "bricksGroupLvl3" = "bricksGroupLvl4"
  | map == "bricksGroupLvl4" = "bricksGroupLvl5"
  | otherwise = "bricksGroupLvl1"

nextLevelMessage :: String -> String
nextLevelMessage map 
  | map == "bricksGroupLvl1" = "Level 2"
  | map == "bricksGroupLvl2" = "Level 3"
  | map == "bricksGroupLvl3" = "Level 4"
  | map == "bricksGroupLvl4" = "Level 5"
  | otherwise = "Congratulations!"

enableBricks :: [(GameObject s)] -> IOGame t s u v ()
enableBricks [] = return ()
enableBricks (x:[]) = setObjectAsleep False x
enableBricks (x:xs) = do
  setObjectAsleep False x
  enableBricks xs

gameCycle :: IOGame GameAttribute () () () ()
gameCycle = do
  (GA lives score bricksNum level timer msgTimer map message) <- getGameAttribute
  setGameAttribute (GA lives score bricksNum level timer (msgTimer-1) map message)
  printOnScreen (show score) TimesRoman24 (0,0) 1.0 1.0 1.0
  printOnScreen (show lives) TimesRoman24 (380,0) 1.0 1.0 1.0
  printOnScreen (message) TimesRoman24 (150,80) 1.0 1.0 1.0

  ball <- findObject "ball" "ballGroup"
  when (and[bricksNum == 0, timer == 100]) (do setGameAttribute (GA lives score bricksNum level (timer-1) 50 (nextMap map) (nextLevelMessage map))
                                               setObjectAsleep True ball)
  when (and[bricksNum == 0, timer > 0, timer < 100]) (do setGameAttribute (GA lives score bricksNum level (timer-1) msgTimer map message))
  when (and[bricksNum == 0, timer == 0, message /= "Congratulations!"]) (do setObjectPosition (20,20) ball 
                                                                            setObjectSpeed (6,6) ball
                                                                            setObjectAsleep False ball
                                                                            setGameAttribute (GA lives score 35 (level+1) 100 msgTimer map message))
  
  (GA lives score bricksNum level timer msgTimer map message) <- getGameAttribute
  when(and[message == "Congratulations!", msgTimer == 0]) $ do
    funExit

  when (and[lives == 0, message == ""]) $ do
    setObjectAsleep True ball
    setGameAttribute (GA lives score bricksNum level timer 50 map "You lost!") 
    
  when (and[lives == 0, message == "You lost!", msgTimer == 0]) $ do  
    funExit

  when (msgTimer == 0) $ do 
    setGameAttribute (GA lives score bricksNum level timer msgTimer map "")

  bricks <- getObjectsFromGroup map
  when (and[bricksNum == 0, timer == 99, message /= "Congratulations!"]) (do enableBricks bricks)

  col1 <- objectLeftMapCollision ball
  col2 <- objectRightMapCollision ball
  when (col1 || col2) (reverseXSpeed ball)
  col3 <- objectTopMapCollision ball
  when col3 (reverseYSpeed ball)
  col4 <- objectBottomMapCollision ball
  when col4 $ do
    -- funExit
    setGameAttribute (GA (lives - 1) (score - 100) bricksNum level timer msgTimer map message)
    reverseYSpeed ball

  
  bar <- findObject "bar" "barGroup"
  col5 <- objectsCollision ball bar
  let (_,vy) = getGameObjectSpeed ball
  when (and [col5, vy < 0])  (do reverseYSpeed ball
                                 setGameAttribute (GA lives (score + 10) bricksNum level timer msgTimer map message))

  bricks <- getObjectsFromGroup map
  col6 <- objectListObjectCollision bricks ball
  brick <- getCollidedBrick bricks ball

  leftRight <-  isLeftRight ball brick
  upDown <-  isUpDown ball brick

  when (and [col6, upDown])  (do setObjectAsleep True brick
                                 reverseYSpeed ball
                                 setGameAttribute (GA lives (score + 10) (bricksNum - 1) level timer msgTimer map message))
  
  when (and [col6, leftRight, not upDown])  (do setObjectAsleep True brick
                                                reverseXSpeed ball
                                                setGameAttribute (GA lives (score + 10) (bricksNum - 1) level timer msgTimer map message))
