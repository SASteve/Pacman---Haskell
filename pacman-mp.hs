import Data.Set                                   ( Set )
import qualified Data.Set as S                    ( delete
                                                  , empty
                                                  , insert
                                                  , member
                                                  , size
                                                  )

import Data.List                                  ( intercalate )

import Control.Concurrent                         ( threadDelay )

import System.IO                                  ( hFlush
                                                  , stdout
                                                  )

import System.Console.ANSI                        ( clearScreen )

import System.IO.Streams                          ( InputStream )

import qualified System.IO.Streams as SIOS        ( fromList
                                                  , read
                                                  )

import Control.Monad                              ( when )
import System.Exit                                ( exitSuccess )
import Control.Monad.Loops                        ( iterateM_ )
import System.Console.Haskeline                   ( runInputT, defaultSettings, getInputChar )
import Data.List.Split                            ( chunksOf )


import Control.Monad                              ( forever )
import Control.Concurrent                         ( forkIO )
import Network                                    ( PortID(PortNumber)
                                                  , connectTo
                                                  , withSocketsDo
                                                  )
import System.Environment                         ( getArgs )

import System.IO                                  ( BufferMode(LineBuffering)
                                                  , hClose
                                                  , hFlush
                                                  , hGetLine
                                                  , hPutStrLn
                                                  , hSetBuffering
                                                  )





data Block = Empty | Solid | Pacman | Ghost deriving (Show, Read, Eq)
type Entity = (Int, Int, Int)
type Board = [[Block]]

first::Entity->Int
first (a,b,c) = a

second::Entity->Int
second (a,b,c) = b

third::Entity->Int
third (a,b,c) = c
{-/_\-} --Random Itachi appears



getBoardBlock::Board->Entity->Block
getBoardBlock gameBoard selector = gameBoard !! (first selector) !! (second selector)


initialBoard::Board
initialBoard = [ [Solid , Solid  , Solid , Solid , Solid , Solid , Solid]
               , [Solid , Pacman , Empty , Empty , Empty , Empty , Solid]
               , [Solid , Empty  , Empty , Empty , Empty , Empty , Solid]
               , [Solid , Empty  , Empty , Ghost , Empty , Empty , Solid]
               , [Solid , Empty  , Empty , Empty , Empty , Empty , Solid]
               , [Solid , Empty  , Empty , Empty , Empty , Empty , Solid]
               , [Solid , Solid  , Solid , Solid , Solid , Solid , Solid]
               ]

initialPacmanPosition::Entity
initialPacmanPosition = (1,1,1)

initialGhostPosition::Entity
initialGhostPosition = (3,4,0)





moveRightEntityonBoard::Board->Board
moveRightEntityonBoard gameBoard = map (\lst -> map (\index -> if index == Pacman then Empty else index) lst) gameBoard


moveRightEntity::Entity->Entity
moveRightEntity entityPosition = (first entityPosition, second entityPosition +1, third entityPosition)::Entity


                                               --let len = length (gameBoard!!0)
                                               --let aux1 = zip [0..] gameBoard
                                               --let aux2 = map (\(i,l) -> (i,zip [0..] (l::[Block]))) aux1
                                               --let aux3 = map (\(i,l) -> map (\(j,x) -> ((i,j),x)) (l::Block))  aux2
                                               --let aux4 = map (\(tpl,z) -> if tpl == (first entityPosition, second entityPosition + 1) then (tpl,Pacman) else (tpl,(z::Block))) $ concat aux3
                                               --let aux5 = map (snd) aux4
                                               --let fin  = ((map (chunksOf len) aux5)::Board)
                                               --return fin


                                             -- (first entityPosition, second entityPosition +1, third entityPosition)::Entity
--Entity(first entityPosition, second entityPosition + 1, third entityPosition)
                                           --else
                                          -- gameBoard
                                           --entityPosition



entityStringPusher::Int->String
entityStringPusher x = case x of 0 -> "U"--"▲"
                                 1 -> "D"--"▼"
                                 2 -> "L"--"◀"
                                 3 -> "R"--"▶"
{-
0->Empty
1->Pacman Up
2->Pacman Down
3->Pacman Left
4->Pacman Right
5->Globe
6->Ghost
-}
printBlock::Block->Entity->String
printBlock Solid  _          =          " [X] "
printBlock Empty  _          =          " [ ] "
printBlock Ghost  _          =          " [G] "
printBlock Pacman  entityPos =" [" ++ (entityStringPusher $ third entityPos) ++ "] "


printRow::[Block]->Entity->String
printRow boardRow entityPos = ( concat $ map (\x -> printBlock x entityPos) boardRow ) ++ "\n"


printBoard::Board->Entity->IO()
printBoard gameBoard entityPos = putStrLn (concat $ map (\x -> printRow x entityPos) gameBoard)

pureStepperFunction :: Board -> Maybe Char -> Board
pureStepperFunction board (Just 'd') = moveRightEntityonBoard board

dpureStepperFunction :: Entity-> Maybe Char -> Entity
dpureStepperFunction board (Just 'd') = moveRightEntity board


impureStepperFunction :: Board -> IO Board
impureStepperFunction gameState = do
  clearScreen
  printBoard initialBoard initialPacmanPosition
  maybeKeyboardInput <- runInputT defaultSettings $ getInputChar ""
  when ( maybeKeyboardInput == Just 'q' ) exitSuccess
  let incompleteGameState = pureStepperFunction initialBoard maybeKeyboardInput
  return incompleteGameState

dimpureStepperFunction :: Entity -> IO Entity
dimpureStepperFunction gameState = do
  clearScreen
  printBoard initialBoard initialPacmanPosition
  maybeKeyboardInput <- runInputT defaultSettings $ getInputChar ""
  when ( maybeKeyboardInput == Just 'q' ) exitSuccess
  let incompleteGameState = dpureStepperFunction gameState maybeKeyboardInput
  return incompleteGameState






main :: IO ()
main = iterateM_ dimpureStepperFunction initialPacmanPosition
