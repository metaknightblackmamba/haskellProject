-- runhaskell Main.hs < LogoSkell.txt > prog.svg   

import System.Environment     
import System.IO     
import System.IO.Error 
import Data.Char       

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction] 
 deriving (Read, Show)

obtenirInstruction :: Instruction -> Instruction
obtenirInstruction (InstructionLogoSkell) = case InstructionLogoSkell of
	(Forward x) -> doAvant(x)
	(Left x) -> doGauche(x)
	(Right x) -> doDroit(x)
	(Repeat x [Instruction]) -> separeRepeat(x)




main = do
 contents <- getLine
 let listeInstruction = (read contents :: [Instruction])
 


 