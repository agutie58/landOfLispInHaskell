import Control.Monad     (unless)
import System.IO
import Data.List  -- Function "walk" `isPrefixOf` "walk east"

----------------------------------------------------------------------------------
-- GAME ENGINE.hs - Adapted Haskell version from "Lanf of Lisp" book of Conrad Barski
----------------------------------------------------------------------------------
type Clau = String
type Descripcio = String
type Valors = [String]

-- data Domain =   Domain (String, World) deriving (Show)
-- data App = App (Domain, presentationIO )

data Lloc = Lloc String String String deriving (Show) 

llocSituacio ::  Lloc -> String  
llocSituacio  (Lloc situacio _ _ ) = situacio  
 
llocDireccio :: Lloc -> String
llocDireccio (Lloc _ direccio _) = direccio

llocPas :: Lloc -> String
llocPas ( Lloc _ _ pas) = pas

-------- Just a graph describing the world and the relations between places ...
nodes :: [(Clau,Descripcio)]
nodes = [("living-room","you are in the living-room. a wizard is snoring loudly on the couch.")
           ,("garden","you are in a beautiful garden. there is a well in front of you.")
           , ("attic", "you are in the attic. there is a giant welding torch in the corner.")]

edges :: [(Clau, [Lloc])]
edges = [ ("living-room", [(Lloc "garden"  "west" "door"), ( Lloc "attic" "upstairs" "ladder") ])
        , ("attic", [(Lloc "living-room"  "east"  "door")])
        , ("garden", [(Lloc "living-room" "east"  "door")])]
--------------------------------------------------

data World = World {loc :: String, descLlocs :: [(Clau,String)], mapaDelsLlocs :: [(Clau, [Lloc])], mutableMessage::String}  deriving (Show)

--------------------------------------------------
-- VERBS  (Actions to perform in world)
presentationIO :: World -> IO ()
presentationIO world = putStr $ showLookWorldMsg world


look :: World -> World
look world = let msg = "look action -" ++ showLookWorldMsg world
             in world {mutableMessage=msg}
      
walk :: String -> World -> World
walk direction world = world {mutableMessage="walking action - changes the state!"} 

-- Helpers 
showLookWorldMsg :: World -> String
showLookWorldMsg world = "(showLookWorldMsg) => " ++ mutableMessage world 

------------------------------------------------------------------------
-- MAIN.hs LOGIC 
-----------------------------------------------------------------------
data Event =
    EventExit            
  | EventLook   
  | EventWalk String
  deriving(Eq,Show)

actualitzarDomini :: World -> Event -> World
actualitzarDomini world (EventLook) =  look world                                                                                 
actualitzarDomini world (EventWalk direction)  =  walk direction world
actualitzarDomini world _  = world 

llegirComanda = do
                input <- read' 
                if input == "look" then
                   return [EventLook]
                else if "walk" `isPrefixOf` input then
                  return [EventWalk ((words input) !! 1)]  
                else if input == ":quit" then
                  return [EventExit]
                else 
                  --putStrLn ("Entra comanda valida!")
                  llegirComanda

read' :: IO String
read' = putStr "WORLD> "
     >> hFlush stdout
     >> getLine

-- The world
theWorld = World {loc = "living-room", descLlocs = nodes, mapaDelsLlocs = edges, mutableMessage="" }

main :: IO ()
main = run theWorld presentationIO

run dom showDom = do
       event <- llegirComanda
       dom' <- actualitzarDomini dom event 
       showDom dom'
       run dom' showDom

       

