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

peaShooter = UnaPlanta "" 5 0 2
repeater = UnaPlanta "" 5 0 4
sunflower = UnaPlanta "" 7 1 0
nut = UnaPlanta "" 100 0 0
tripleSunflower = UnaPlanta ""  7 3 0
canon = UnaPlanta "" 4 0 8

zombieBase = UnZombie "ZombieBase" [""] 1
ballonzombie = UnZombie "BallonZombie" ["Globo"] 1
newspaperzombie = UnZombie "NewspaperZombie" ["Diario"] 2
gargantuar = UnZombie "GargantuarHulkSmashPunyGod" ["PosteElectrico", "ZombieEnano"] 30

nivelDeMuerte:: Zombie -> Number
nivelDeMuerte = (length.nombre)

especialidad:: Planta -> [Char]
especialidad    | solesQueProduce /= 0 = "Proveedora"
                | poderDeAtaque > puntosDeVida = "Atacante"
                | otherwise = "Defensiva"

esPeligroso:: Zombie -> Bool
esPeligroso zombie  | accesorios(zombie) >= 1 || nivelDeMuerte(zombie) > 10 