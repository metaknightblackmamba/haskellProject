--CMD : runhaskell main.hs < LogoSkell.txt > prog.svg

import Prelude hiding (Left, Right)

data Instruction = Forward Int
                  | Left Int
                  | Right Int
                  | Repeat Int [Instruction]
         deriving (Show, Read, Eq) -- on cree les classe Show Read et Eq avec deriving

--On définit des coordonnées polaires pour pouvoir utiliser les instructions Forward Left Right en effectuant des rotations pour obtenir le cercle.
--Ces positions seront definis avec un angle theta qui est égal à la position à l'instant + un certain angle t qui sera converti en fraction grâce à realToFrac

ligneX :: Int -> Double -> Double -> Double
ligneX l x theta = x+(realToFrac l)*(cos theta)

ligneY :: Int -> Double -> Double -> Double
ligneY l y theta = y+(realToFrac l)*(sin theta)

--turtle nous permettra de définir un repere pour mieux appréhender la postion de x et de y selon leurs coordonnées polaires. Dans chaque direction (Left ou Right) les points x et y seront definis par leurs coordonnées.
--En allant à gauche (sens contraire des aiguilles d'une montre)  les positions seront defini avec un angle de theta +(pi*angleposition)/180
--En allant à droite (sens des aiguilles d'une montre) les positions de x et de y seront définis de theta -(pi*anglepositon)/180
--Puis on appelle Forward. On avance au fur et à mesure en effectuant les opérations prédefinis (pour Left et Right). C'est comme si on faisait une sorte de boucle

turtle :: [Instruction] -> Double -> Double -> Double -> [(Double,Double)] -> [(Double,Double)]
turtle   []      posx posy theta tsvg = tsvg
turtle (i:rprog) posx posy theta tsvg = case i of
    Left    t -> turtle rprog posx posy (theta+(pi*(realToFrac t)/180)) tsvg
    Right   t -> turtle rprog posx posy (theta-(pi*(realToFrac t)/180)) tsvg
    Forward l -> (turtle
                        rprog
                        nposx -- on appelle en donnant notre position au suivant
                        nposy
                        theta
                        (
                            ( --on rajoute notre position à la liste de sortie
                                nposx,nposy
                            ):tsvg
                        )
                  ) where nposx = (ligneX l posx theta)
                          nposy = (ligneY l posy theta)


couleur c = case c of --Mettre des couleurs pour mieux se situer et de bien voir les points
    0->"darkcyan"
    1->"red"
    2->"darkorange"
    3->"lightsteelblue"
    4->"rosybrown"
    5->"khaki"
    6->"indigo"
    7->"olivedrab"
    8->"salmon"
    9->"cadetblue"
cicl x = if x<9 then (x+1) else 0

ligneSvg x1 y1 x2 y2 c = "<line x1=\"" ++(show x1)++ "\" y1=\"" ++(show y1)++ "\" x2=\"" ++(show x2)++ "\" y2=\"" ++(show y2)++ "\" stroke=\""++ (couleur c) ++"\" stroke-width=\"1\" />\n"
svgHead = "<?xml version=\"1.0\" encoding=\"utf-8\"?><svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"500\" height=\"300\">\n<title>Absatou et Florian</title>\n"
svgHeadParam (largeur, hauteur)= "<?xml version=\"1.0\" encoding=\"utf-8\"?><svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\""++(show largeur)++"\" height=\""++(show hauteur)++"\">\n<title>Absatou et Florian</title>\n"
svgTail = "</svg>"

data Crayon = Crayon Double Double Double Int deriving (Show, Read, Eq)
useCrayon (Crayon x y t couleur) = "x="++(show x)++" y="++(show y)++" theta="++show t
initC = Crayon 0.0 0.0 0.0 0

logoskell2svg :: [Instruction] -> Crayon -> [String] -> (Crayon, [String])
logoskell2svg [] c u=(c,u)
logoskell2svg (ins:restPrgm) (Crayon x0 y0 t0 couleur) out= case ins of
    (Forward l) -> (nouveauCrayon,(snd $ logoskell2svg restPrgm nouveauCrayon (lsvg:out)))
        where nouveauCrayon = Crayon x1 y1 t0 (cicl couleur)
              x1=ligneX l x0 t0
              y1=ligneY l y0 t0
              lsvg = ligneSvg x0 y0 x1 y1 couleur

    (Left t) -> (nouveauCrayon, snd $ logoskell2svg restPrgm nouveauCrayon out)
        where nouveauCrayon = Crayon x0 y0 (t0+(pi*(realToFrac t)/180)) couleur

    (Right t) -> (nouveauCrayon, snd $ logoskell2svg restPrgm nouveauCrayon out)
            where nouveauCrayon = Crayon x0 y0 (t0-(pi*(realToFrac t)/180)) couleur
    (Repeat n prog) -> (crayon, snd $ logoskell2svg ((concat $ take n $ repeat prog)++restPrgm) crayon out) --(prog++((LRepeat (n-1) prog):restPrgm)) --gaffe aux boucles infinies
        where crayon = Crayon x0 y0 t0 couleur

turtlePoints :: [Instruction] -> Crayon -> [(Double, Double)] ->(Crayon, [(Double, Double)])
turtlePoints [] c p = (c,p)
turtlePoints (ins:restPrgm) (Crayon x0 y0 t0 couleur) out= case ins of
    (Forward l) -> (nouveauCrayon,(snd $ turtlePoints restPrgm nouveauCrayon ((x1,y1):out)))
        where nouveauCrayon = Crayon x1 y1 t0 couleur
              x1=ligneX l x0 t0
              y1=ligneY l y0 t0
    (Left t) -> (nouveauCrayon, snd $ turtlePoints restPrgm nouveauCrayon out)
        where nouveauCrayon = Crayon x0 y0 (t0+(pi*(realToFrac t)/180)) couleur

    (Right t) -> (nouveauCrayon, snd $ turtlePoints restPrgm nouveauCrayon out)
            where nouveauCrayon = Crayon x0 y0 (t0-(pi*(realToFrac t)/180)) couleur
    (Repeat n prog) -> (crayon, snd $ turtlePoints ((concat $ take n $ repeat prog)++restPrgm) crayon out) --(prog++((LRepeat (n-1) prog):restPrgm)) --gaffe aux boucles infinies
        where crayon = Crayon x0 y0 t0 couleur

tailleCanevas :: [Instruction] -> (Double,Double,Double,Double)
tailleCanevas prgm = ((maxX-minX),(maxY-minY),(pointDeDepart minX),(pointDeDepart minY)) --dimensionne efficacement la taille de la figure SVG
    where points = unzip $ snd $ turtlePoints prgm (Crayon 0 0 0 0) []
          pointsX = fst $ points
          pointsY = snd $ points
          minX = minimum pointsX
          maxX = maximum pointsX
          minY = minimum pointsY
          maxY = maximum pointsY

pointDeDepart minCoord = abs minCoord --sert à éviter que ça trace avec des coordonnées négatives


executerProgramme prgm = tracer prgm (tailleCanevas prgm)
tracer :: [Instruction] -> (Double,Double,Double,Double) -> String
tracer prgm (largeur,hauteur,minX,minY) = (svgHeadParam (largeur, hauteur))++(foldl1 (++) $ snd $ logoskell2svg prgm (Crayon minX minY 0 0) [])++svgTail

main = do
 termStdin<-getLine -- On récupère l'entrée par lignes
 putStrLn $ executerProgramme ( read (termStdin) :: [Instruction])  -- nous lisons les lignes en type Instruction, et on print/lance le programme
--La fontion main permet de recuperer les lignes converties en svg et d'en appliquer
--les instructions c'est à dire d'executer les instructions definies dans data pour
--pouvoir favoriser l'affichage