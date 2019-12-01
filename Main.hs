
-- Import du type prelude en cachant les nom des méthode Left et Right pour enlever l'erreur lorsqu'on va lire l'instruction du logoskell
import Prelude hiding (Left, Right)

-- Nouveau type Instruction, de type Forward 50.0 ou Left 41.0, etc... qui herite des type Show et Read
data Instruction = Forward Float | Left Float | Right Float | Repeat Float [Instruction] 
 deriving (Show, Read)

-- Nouveau type Crayon, de type DonneCrayon {110.0, 120.0, 45;0} par exemple
data Crayon = DonneCrayon {coordX::Float, coordY::Float, angle::Float}

-- Nouvelle fonction pour depacker les Instruction repeat, si egale a 1 on repete 1 fois, sinon on repete la commande en mode recursif pour decrémenter le nbr de repeat 
repeatInst :: [Instruction] -> Float -> [Instruction]  
repeatInst liste nbrRepetitions = case nbrRepetitions of
  1 -> (obtenirInstruction liste)
  
  _ -> (obtenirInstruction liste) ++ (repeatInst liste (nbrRepetitions-1))
  
-- Nouvelle fonction pour obtenir les données de type [Instruction] 
obtenirInstruction :: [Instruction] -> [Instruction]
obtenirInstruction liste = case liste of
  []-> liste
  (x:[])-> case x of
      (Repeat i instr) -> repeatInst instr i
      (Forward i) -> [x]
      (Left i) -> [x]
      (Right i) -> [x]
  
  (x:xs)-> (obtenirInstruction [x]) ++ (obtenirInstruction xs)

conv_prog :: [Instruction] -> Crayon -> String -> (String, Crayon) 
conv_prog liste crayon svg = case liste of
    [] -> (svg,crayon)
    (x:[]) -> case x of
        (Forward pas) -> ((svg ++ "<line x1=\""++ (show (coordX crayon)) ++"\" y1=\""++ (show (coordY crayon)) ++"\" x2=\""++ (show (x2)) ++"\" y2=\""++ (show y2) ++"\" stroke=\"red\" /> \n"), newCrayon)
            where x2 = (coordX crayon) + (pas*(cos (angleRad)))
                  y2 = (coordY crayon) + pas*(sin (angleRad))
                  newCrayon = DonneCrayon x2 y2 (angle crayon)
                  angleRad = (angle crayon)*pi/180
                  
  
        (Left pas) -> (svg,newCrayon)
            where newCrayon = DonneCrayon (coordX crayon) (coordY crayon) ((angle crayon)+pas)
      
        (Right pas) -> (svg,newCrayon)
            where newCrayon = DonneCrayon (coordX crayon) (coordY crayon) ((angle crayon)-pas)
      
    (x:xs) -> conv_prog xs crayon2 svg2 --(x::[])
        where (svg2,crayon2) = conv_prog [x] crayon svg -- fst, snd
        

main = do
  enonce <- getLine -- met dans une variable l'entrée du programme
  let listeInstructions = (read enonce :: [Instruction]) -- Lit le contenu de la variable et met le contenu de type [Instruction]
  let monCrayon = DonneCrayon 100.0 100.0 0.0 -- Initialise le tracer
  let monSVG = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"200\" height=\"200\">\n<title>Exemple</title>\n" -- Entete SVG

  putStrLn ((fst (conv_prog (obtenirInstruction listeInstructions) monCrayon monSVG)) ++ "</svg>")
  -- 
