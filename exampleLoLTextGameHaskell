
import Control.Monad     (unless)
import System.IO
import Data.List  -- Function "walk" `isPrefixOf` "walk east"

----------------------------------------------------------------------------------
-- GAME ENGINE.hs - Adapted Haskell version from "Lanf of Lisp" book of Conrad Barski
----------------------------------------------------------------------------------
type Clau = String
type Descripcio = String
type Valors = [String]

data Domain =   Domain (String, World) deriving (Show)

data Lloc = Lloc String String String deriving (Show) 

llocSituacio ::  Lloc -> String  
llocSituacio  (Lloc situacio _ _ ) = situacio  
 
llocDireccio :: Lloc -> String
llocDireccio (Lloc _ direccio _) = direccio

llocPas :: Lloc -> String
llocPas ( Lloc _ _ pas) = pas

--------
nodes :: [(Clau,Descripcio)]
nodes = [("living-room","you are in the living-room. a wizard is snoring loudly on the couch.")
           ,("garden","you are in a beautiful garden. there is a well in front of you.")
           , ("attic", "you are in the attic. there is a giant welding torch in the corner.")]
--  lookup "living-room" nodes

edges :: [(Clau, [Lloc])]
edges = [ ("living-room", [(Lloc "garden"  "west" "door"), ( Lloc "attic" "upstairs" "ladder") ])
        , ("attic", [(Lloc "living-room"  "east"  "door")])
        , ("garden", [(Lloc "living-room" "east"  "door")])]
--------------------------------------------------
objects :: [String]
objects =  ["whiskey", "bucket", "frog", "chain"]

objectLocations :: [(String,String)]
objectLocations = [("whiskey", "living-room")
                  ,("bucket", "living-room")
                  ,("chain", "garden")
                  ,("frog", "garden")]

data World = World {loc :: String, descLlocs :: [(Clau,String)], mapaDelsLlocs :: [(Clau, [Lloc])], objectes:: [Clau], llocObjectes::[(Clau,String)]}  deriving (Show)

theWorld = World {loc = "living-room", descLlocs = nodes, mapaDelsLlocs = edges, objectes = objects, llocObjectes=objectLocations }

-- Core  : https://wiki.haskell.org/Real_World_Applications/Event_Driven_Applications
--         https://softwareengineering.stackexchange.com/questions/316411/event-driven-programming-in-haskell
--------------------------------------------------
-- Els VERBS  
showLookMsg :: World -> String
showLookMsg world = (describeLocation location nodes) ++ "\n" 
                                                  ++ (describePaths location edges) 
                                                  ++ (describeObjects location objects objectLocations)
                                                  ++ "\n"                  
                    where location = loc world
                          edges = mapaDelsLlocs world
                          nodes = descLlocs world
                          objects = objectes world
                          objectLocations = llocObjectes world
look :: World -> Domain
look world = let msg = showLookMsg world
             in (Domain (msg,world))
      

walk :: String -> World -> Domain
walk direction world = case esPotAnarDireccio direction of Just [] -> let msg = "No esta permès anar en aquesta direccio!"
                                                                      in (Domain (msg, world))
                                                           Just llistaLoc -> 
                                                                      let newLloc = llocSituacio ( llistaLoc !! 0)
                                                                          frase = "Nou lloc "  ++ newLloc ++ "\n"
                                                                          newWorld = world {loc= newLloc} 
                                                                      -- look newWorld
                                                                      in (Domain (frase, newWorld))
                       where teDireccio direccio lloc = (==) (llocDireccio lloc) (direccio)
                             esPotAnarDireccio direccio = fmap (filter (teDireccio direccio )) (lookup location edges)
                             location = loc world

pickup :: String -> World -> Domain
pickup objecte world = case esPotAgafarObjecte objecte of False -> let msg = "No es pot agafar aquest objecte en aquest lloc!"
                                                                   in (Domain (msg, world))
                                                          True -> 
                                                                  let 
                                                                  -- putStr accio
                                                                  novaLlista = filter (\(a,b) -> a /= objecte || b /= location ) objectLocations
                                                                  novaLlista' = novaLlista ++ [(objecte,"body")] -- Si no es posa el ' es queda penjat! xD
                                                                  msg = "Agafes el/la " ++ objecte ++" \n" ++ (show novaLlista')
                                                                  newWorld = world { llocObjectes = novaLlista' }
                                                                  in (Domain (msg, newWorld))
                        where location = loc world 
                              objectLocations = llocObjectes world
                              esPotAgafarObjecte objecte =  length (  filter (\(a,b) -> a == objecte && b == location ) objectLocations ) > 0

inventory :: World -> [String]
inventory world = objectsAt "body" objects objectLocations 
    where objects = objectes world
          objectLocations = llocObjectes world

--------------------------------------------------
describeLocation ::  Clau -> [(Clau, Descripcio)] -> Descripcio
describeLocation situacio nodes = case lookup situacio nodes of
    Nothing -> ""
    Just description -> description

describePath :: Lloc -> String  
describePath  e = "There is " ++ llocPas e ++ " going " ++ llocDireccio e ++ " from here.\n" 

--------------------------------------------------
describePaths situacio edges =  concat $ case lookup situacio edges of
    Nothing -> [] -- Curios que tan va be aixi com ""
    Just locs ->  map describePath locs

objectsAt loc objs objsLoc = filter isLoc objs
    where isLoc obj = elem loc (lookup obj objsLoc) 

describeObjects loc obj objLoc = ajuntaEnSolTextS "\n" $ fmap (describeObj) (objectsAt loc obj objLoc)
    where describeObj obj =  ("You see a " ++ obj ++ " on the floor.")

-- Patter maching
ajuntaEnSolText llista = ajuntaEnSolTextS (" ") (llista)
ajuntaEnSolTextS sep [] = ""
ajuntaEnSolTextS sep (l:ls) = l ++ sep ++ otherLines  -- podem posar sep="\n"
 where otherLines = ajuntaEnSolText ls

------------------------------------------------------------------------
-- MAIN.hs LOGIC 
-----------------------------------------------------------------------

data Event =
    EventExit            -- User wants to exit
  | EventLook   
  | EventPickup String
  | EventWalk String
  | EventAdd Int         -- Els events pot ser un tipus molt divers! (se li pot passar parametres si conve!)  
  deriving(Eq,Show)

dmUpdate :: Domain -> Event  -> Domain
dmUpdate (Domain v) (EventLook) =  look (snd v)                                                                                 
dmUpdate (Domain v) (EventWalk direction)  =  walk direction (snd v) 
dmUpdate dm _  = dm 

uiUpdate :: Domain -> IO [Event]
uiUpdate (Domain (msg, world)) = do
  input <- read' 
  if input == "look" then
    putStrLn (showLookMsg world) -- Here it is ok, but I would rather prefer execute the Event, and then putStrLn
    >> return [EventLook]
  else if input == "show" then
    putStrLn (msg) -- Just to check
    >> return [EventLook]
  else if "walk" `isPrefixOf` input then
    return [EventWalk ((words input) !! 1)]  -- If EventLook had IO ".., EventLook]"
  else return [EventExit]

run :: Domain -> [Event] -> IO ()
run dm [] = do
            events <- uiUpdate dm
            run dm events
run _ (EventExit:_) =
            return ()
run dm (e:es) =
            run (dmUpdate dm e ) es

read' :: IO String
read' = putStr "WORLD> "
     >> hFlush stdout
     >> getLine

main :: IO ()
main = run ( Domain ("",theWorld)) [EventLook]
