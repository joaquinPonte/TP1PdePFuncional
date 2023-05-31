module Library where
import PdePreludat

--PARTE 1 DEL TP

--1)a)
data Planta = UnaPlanta {
    especie:: [Char],
    puntosDeVida:: Number,
    solesQueProduce:: Number,
    poderDeAtaque:: Number
}deriving(Show,Eq)

peaShooter = UnaPlanta {especie="PeaShooter", puntosDeVida=5, solesQueProduce=0, poderDeAtaque=2}
repeater = UnaPlanta {especie="Repeater", puntosDeVida=5, solesQueProduce=0, poderDeAtaque=4}
sunflower = UnaPlanta {especie="Sunflower", puntosDeVida=7, solesQueProduce=1, poderDeAtaque=0}
nut = UnaPlanta {especie="Nut", puntosDeVida=100, solesQueProduce=0, poderDeAtaque=0}
tripleSunflower = UnaPlanta {especie="TripleSunflower", puntosDeVida=7, solesQueProduce=3, poderDeAtaque=0}
cherryCanon = UnaPlanta {especie="CherryCanon", puntosDeVida=4, solesQueProduce=0, poderDeAtaque=15}

--1)b)

data Zombie = UnZombie {
    nombre:: [Char],
    accesorios:: [[Char]],
    dañoPorMordida:: Number
}deriving(Show,Eq)

zombieBase = UnZombie {nombre="ZombieBase", accesorios=[], dañoPorMordida=1}
ballonZombie = UnZombie {nombre="BallonZombie", accesorios=["Globo"], dañoPorMordida=1}
newspaperZombie = UnZombie {nombre="newspaperZombie", accesorios=["Diario"], dañoPorMordida=2}
gargantuar = UnZombie {nombre="GargantuarHulkSmashPunyGod", accesorios=["PosteElectrico", "ZombieEnano"], dañoPorMordida=30}

--2)a)

especialidad:: Planta -> [Char]
especialidad planta | ((/=0).solesQueProduce) planta = "Proveedora"
                    | poderDeAtaque(planta) > puntosDeVida(planta) = "Atacante"
                    | otherwise = "Defensiva"

--2)b)

nivelDeMuerte:: Zombie -> Number
nivelDeMuerte = (length.nombre)

cantidadAccesorios:: Zombie -> Number
cantidadAccesorios = (length.accesorios)

esPeligroso:: Zombie -> Bool
esPeligroso zombie  = ((>=1).cantidadAccesorios) zombie || ((>10).nivelDeMuerte) zombie

--3)

data Linea = LineaDeDefensa {
    plantas:: [Planta],
    zombies:: [Zombie]
}deriving(Show,Eq)

linea1 = LineaDeDefensa {plantas=[sunflower, sunflower, sunflower], zombies=[]}
linea2 = LineaDeDefensa {plantas=[peaShooter, peaShooter, sunflower, nut], zombies=[zombieBase, newspaperZombie]}
linea3 = LineaDeDefensa {plantas=[sunflower, peaShooter], zombies=[gargantuar, zombieBase, zombieBase]}

--3)a)

agregarPlantaYZombie:: [Planta] -> [Zombie] -> Linea -> Linea
agregarPlantaYZombie  newPlants newZombies linea = linea {plantas=(plantas linea ++ newPlants),zombies=(zombies linea ++ newZombies)}

--3)b)

totalAtaquePlantas:: [Planta] -> Number
totalAtaquePlantas = (sum.map (poderDeAtaque))

totalMordiscos:: [Zombie] -> Number
totalMordiscos = (sum.map (dañoPorMordida))

estaEnPeligro:: Linea -> Bool
estaEnPeligro linea = ((/=0).totalMordiscos.zombies) linea && (((totalAtaquePlantas.plantas) linea) < ((totalMordiscos.zombies) linea)) || ((null.filter(not.esPeligroso)) (zombies linea))

--3)c)

esProveedora:: Planta -> Bool
esProveedora = (=="Proveedora").especialidad

necesitaSerDefendida:: Linea -> Bool
necesitaSerDefendida = (not.any (not.esProveedora).plantas)

--4)

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

--5)a)

quitarLetras:: Zombie -> Number -> [Char]
quitarLetras zombie numero = drop numero (nombre zombie)

plantaAtacaZombie:: Planta -> Zombie -> Zombie
plantaAtacaZombie planta zombie = zombie {nombre=((zombie  `quitarLetras`) (poderDeAtaque planta))}

--5)b)

restarVida:: Planta -> Number -> Number
restarVida planta daño  | ((puntosDeVida planta) - daño) <= 0 = 0
                        | otherwise = ((puntosDeVida planta) - daño)
 
zombieAtacaPlanta:: Zombie -> Planta -> Planta
zombieAtacaPlanta zombie planta = planta {puntosDeVida=((restarVida planta) (dañoPorMordida zombie))}

-------------------------------------SEGUNDA PARTE TP-------------------------------------------------------------

--  1) I. Al consultar si una linea esta en peligro pasandole una lista en la que hay infinitos "zombieBase", 
--        el programa entraria en un bucle infinito y no terminaria la ejecucion.
--    II. Al consultar si la linea necesita ser defendida pasandole una lista en la que hay infinitos "peaShooter",
--        el programa no entra en un bucle infinito y devuelve "False". En el caso de que se le pase una cantidad
--        infinita de "sunflower" el programa entra en un bucle infinito y no terminaria la ejecucion.
--   III. Ambas respuestas se pueden justificar debido a la "lazy evaluation" o "evaluacion perezosa". En el primer
--        caso, la funcion requiere evaluar la lista completa, lo que hace imposible la evaluacion perezosa, y por
--        eso mismo el programa entra en un bucle infinito al tratar de calcular "totalMordiscos". En el segundo caso,
--        modifique la funcion para que la misma devuelva "False" en caso de encontrar 1 elemento que no sea "Proovedora",
--        por lo cual, si dicho elemento se encuentra, no es necesario evaluar el resto de la lista (caso de infinitos
--        "peaShooter") y el programa retorna "False" y si dicho elemento no se encuentra, se sigue evaluando la lista,
--        lo que hace que en el caso de infinitos "sunflower" el programa entre en un bucle infinito.

--2)
cactus =  UnaPlanta {especie="Cactus", puntosDeVida=9, solesQueProduce=0, poderDeAtaque=0}

plantaAtacaZombieMod:: Planta -> Zombie -> Zombie
plantaAtacaZombieMod planta zombie | ((=="Cactus").especialidad) planta && ((=="Globo").head.accesorios) zombie = zombie {accesorios = ((tail.accesorios) zombie)}
                                |otherwise= zombie {nombre=((zombie  `quitarLetras`) (poderDeAtaque planta))}

--3)

type Jardin = [Linea]
type Horda = [(Zombie,Linea)]

septimoRegimiento = [(newspaperZombie,linea1),(ballonZombie,linea2),(ballonZombie,linea2)]
region = [(gargantuar,linea1),(gargantuar,linea1),(gargantuar,linea2),(gargantuar,linea2),(gargantuar,linea3),(gargantuar,linea3)]
jardin = [linea1,linea2,linea3]


sumarZombie:: Linea -> [Zombie] -> Linea
sumarZombie linea newZombies = LineaDeDefensa {plantas= (plantas linea), zombies= (zombies linea)++newZombies}

buscarZombies:: Linea -> Horda -> [Zombie]
buscarZombies linea horda = map (fst) (filter ((==linea).snd) horda)

buscarPorLinea:: Linea -> Horda -> Linea
buscarPorLinea linea horda = sumarZombie linea (buscarZombies linea horda)

agregar:: Jardin -> Horda -> Jardin
agregar jardin horda = map (`buscarPorLinea` horda) jardin

--4)

rondaDeAtaque:: Planta -> Zombie -> Number -> (Planta,Zombie)
rondaDeAtaque planta zombie cantMordidas = ((planta {puntosDeVida= (planta `restarVida` ((*cantMordidas).dañoPorMordida) zombie)}),(plantaAtacaZombie planta zombie))

--5)

resultadoFuegoCruzado:: (Planta,Zombie) -> (Bool,Bool)
resultadoFuegoCruzado (planta,zombie) = (((==0).puntosDeVida) planta, ((=="").nombre) zombie)

--6)

ataqueSistematico:: [Planta] -> Zombie -> [Planta]
ataqueSistematico plantas zombie = [ (zombieAtacaPlanta zombie planta) | planta <- plantas ]

--7)

