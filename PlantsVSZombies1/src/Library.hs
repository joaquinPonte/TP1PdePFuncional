module Library where
import PdePreludat

data Planta = UnaPlanta {
    especie:: [Char],
    puntosDeVida:: Number,
    solesQueProduce:: Number,
    poderDeAtaque:: Number
}deriving(Show,Eq)

data Zombie = UnZombie {
    nombre:: [Char],
    accesorios:: [[Char]],
    dañoPorMordida:: Number
}deriving(Show,Eq)

data Linea = LineaDeDefensa {
    plantas:: [Planta],
    zombies:: [Zombie]
}deriving(Show,Eq)

peaShooter = UnaPlanta "PeaShooter" 5 0 2
repeater = UnaPlanta "Repeater" 5 0 4
sunflower = UnaPlanta "SunFlower" 7 1 0
nut = UnaPlanta "Nut" 100 0 0
tripleSunflower = UnaPlanta "TripleSunflower"  7 3 0
cherryCanon = UnaPlanta "CherryCanon" 4 0 8

zombieBase = UnZombie "ZombieBase" [] 1
ballonzombie = UnZombie "BallonZombie" ["Globo"] 1
newspaperzombie = UnZombie "NewspaperZombie" ["Diario"] 2
gargantuar = UnZombie "GargantuarHulkSmashPunyGod" ["PosteElectrico", "ZombieEnano"] 30

linea1 = LineaDeDefensa [sunflower, sunflower, sunflower] []
linea2 = LineaDeDefensa [peaShooter, peaShooter, sunflower, nut] [zombieBase, newspaperzombie]
linea3 = LineaDeDefensa [sunflower, peaShooter] [gargantuar, zombieBase, zombieBase]

nivelDeMuerte:: Zombie -> Number
nivelDeMuerte = (length.nombre)

cantidadAccesorios:: Zombie -> Number
cantidadAccesorios = (length.accesorios)

especialidad:: Planta -> [Char]
especialidad planta | solesQueProduce(planta) /= 0 = "Proveedora"
                    | poderDeAtaque(planta) > puntosDeVida(planta) = "Atacante"
                    | otherwise = "Defensiva"

esPeligroso:: Zombie -> Bool
esPeligroso zombie  = cantidadAccesorios(zombie) >= 1 || nivelDeMuerte(zombie) > 10

agregarPlantaYZombie:: [Planta] -> [Zombie] -> Linea -> Linea
agregarPlantaYZombie  newPlants newZombies linea = LineaDeDefensa (plantas linea ++ newPlants) (zombies linea ++ newZombies)

totalataquePlantas:: [Planta] -> Number
totalataquePlantas [] = 0
totalataquePlantas planta  = (poderDeAtaque.head) planta + (totalataquePlantas.tail) planta

totalMordiscos:: [Zombie] -> Number
totalMordiscos [] = 0
totalMordiscos zombie = (dañoPorMordida.head) zombie + (totalMordiscos.tail) zombie

estaEnPeligro:: Linea -> Bool
estaEnPeligro linea = (not.(==0).totalMordiscos.zombies) linea && (((totalataquePlantas.plantas) linea) < ((totalMordiscos.zombies) linea)) || (((==0).length.filter(not.esPeligroso)) (zombies linea))

esProveedora:: Planta -> Bool
esProveedora = (=="Proveedora").especialidad

necesitaSerDefendida:: Linea -> Bool
necesitaSerDefendida = ((==0).length.filter(not.esProveedora).plantas)

alMenosDos:: [Planta] -> Bool
alMenosDos = ((>=2).length)

especialidades:: [Planta] -> [[Char]]
especialidades = map(especialidad)

distintaEspecialidad:: [Planta] -> Bool
distintaEspecialidad plantas    | ((<=1).length) plantas = True
                                | (head.especialidades) plantas /= (((tail.especialidades) plantas) !! 0) = True && (distintaEspecialidad.tail) plantas
                                | otherwise = False

lineaMixta:: Linea -> Bool
lineaMixta linea = (alMenosDos.plantas) linea && (distintaEspecialidad.plantas) linea

quitarLetras:: Zombie -> Number -> [Char]
quitarLetras zombie numero = drop numero (nombre zombie)

plantaAtacaZombie:: Planta -> Zombie -> [Char]
plantaAtacaZombie planta zombie = "Vida Restante: " ++ show ((length.(zombie  `quitarLetras`)) (poderDeAtaque planta))

restarVida:: Planta -> Number -> [Char]
restarVida planta daño  | ((puntosDeVida planta) - daño) <= 0 = "Vida Restante: 0"
                        | otherwise = "Vida Restante: " ++ show (((puntosDeVida planta) - daño))
 
zombieAtacaPlanta:: Zombie -> Planta -> [Char]
zombieAtacaPlanta zombie planta = restarVida planta (dañoPorMordida zombie)