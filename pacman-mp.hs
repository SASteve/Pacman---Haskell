import Data.Set                                            ( Set )
import qualified Data.Set as S                             ( delete
                                                           , empty
                                                           , insert
                                                           , member
                                                           , size
                                                           )
import Data.List                                           ( intercalate )
import Control.Concurrent                                  ( threadDelay )
import System.IO                                           ( hFlush
                                                           , stdout
                                                           )
import System.Console.ANSI                                 ( clearScreen )
import System.IO.Streams                                   ( InputStream )
import qualified System.IO.Streams as SIOS                 ( fromList
                                                           , read
                                                           )
import Control.Monad                                       ( when )
import System.Exit                                         ( exitSuccess )
import Control.Monad.Loops                                 ( iterateM_ )
import System.Console.Haskeline                            ( runInputT, defaultSettings, getInputChar )
import Data.List.Split                                     ( chunksOf )
import Control.Monad                                       ( forever )
import Control.Concurrent                                  ( forkIO )
import Network                                             ( PortID(PortNumber)
                                                           , connectTo
                                                           , withSocketsDo
                                                           )
import System.Environment                                  ( getArgs )
import System.IO                                           ( BufferMode(LineBuffering)
                                                           , hClose
                                                           , hFlush
                                                           , hGetLine
                                                           , hPutStrLn
                                                           , hSetBuffering
                                                           )


data Block = Empty | Solid | Pacman | Ghost deriving (Show, Read, Eq)

type Entity = (Int, Int, Int)
type Board = [[Block]]

type GameState = (Board, Entity)


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

modifyRightEntity::Entity->Entity
modifyRightEntity ent = (first ent, second ent + 1, third ent)


moveRightEntity::Board-> Entity->Board
moveRightEntity inputList ent =
	let aux1 = zip [0..] inputList
	    aux2 = map (\(i,l) -> (i,zip [0..] l)) aux1
	    aux3 = map (\(i,l) -> map (\(j,x) -> ((i,j),x)) l) aux2
	    aux4 = map (\((a,b),z) -> if (a,b) == (first ent, second ent) then ((a,b), Empty)  else  if (a,b) ==(first ent, second ent + 1) then ((a,b), Pacman) else ((a,b),z)) $ concat aux3
	in  chunksOf 7 $ map snd aux4




entityStringPusher::Int->String
entityStringPusher x = case x of 0 -> "P"--"▲"
                                 1 -> "P"--"▼"
                                 2 -> "P"--"◀"
                                 3 -> "P"--"▶"
{-
0->Empty
1->Pacman Up
2->Pacman Down
3->Pacman Left
4->Pacman Right
5->Globe
6->Ghost
-}
printBlock::Block->String
printBlock Solid  = " [X] "
printBlock Empty  = " [ ] "
printBlock Ghost  = " [G] "
printBlock Pacman = " [P] "

printRow::[Block]->String
printRow boardRow  = ( concat $ map (\x -> printBlock x ) boardRow ) ++ "\n"

printBoard::Board->IO()
printBoard gameBoard = putStrLn (concat $ map (\x -> printRow x ) gameBoard)

pureStepperFunction :: Board -> Entity -> Maybe Char -> Board
pureStepperFunction board ent (Just 'd') = moveRightEntity board ent

entityStepperFunction :: Entity -> Maybe Char -> Entity
entityStepperFunction ent (Just 'd') = modifyRightEntity ent

firstRun::GameState -> IO GameState
firstRun gameState = do
  clearScreen
  printBoard (fst gameState)
  maybeKeyboardInput <- runInputT defaultSettings $ getInputChar ""
  when ( maybeKeyboardInput == Just 'q' ) exitSuccess
  let incompleteGameState = pureStepperFunction (fst gameState) (snd gameState) maybeKeyboardInput
  let new_entity          = entityStepperFunction (snd gameState) maybeKeyboardInput
  return ((incompleteGameState, new_entity) :: GameState)


main :: IO ()
main = iterateM_ firstRun ((initialBoard, initialPacmanPosition)::GameState)
