-- runhaskell Main.hs < LogoSkell.txt > prog.svg   
-- Forward 100, Repeat 4 [Forward 50, Left 90], Forward 100]


import System.Environment     
import System.IO     
import System.IO.Error 
import Data.Char 
import Prelude hiding (Left, Right)      

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction] 
 deriving (Read, Show)

obtenirInstruction :: Instruction -> String
obtenirInstruction (instructionLogoSkell) = case instructionLogoSkell of
 Forward x ->  "avant"
 Left x ->  "arriere"
 Right x -> "droit"
 Repeat x tabInstruction -> separeInstruction x tabInstruction


separeInstruction :: [Instruction] -> Instruction
separeInstruction (tabrepeat) = 
 return tabrepeat
 

 


main = do
 contents <- getLine
 let listeInstruction = (read contents :: [Instruction])
 let test = map obtenirInstruction listeInstruction
 print test
 


 

 --lire le fichier 
 --transforme Instruction
 --unpack Instruction
