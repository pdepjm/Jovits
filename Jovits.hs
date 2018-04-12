data UnAnillo = UnAnillo{
	peso:: Int,
	inscripcion:: String,
	cantidadDeDias:: Int}  deriving (Show)

data Hobbit = UnHobbit{
	nombre :: String,
	estatura :: Int,
	salud :: Int,
	fuerza :: Int,
	deLaComarca :: Bool,
	anillo:: UnAnillo} deriving Show

anilloUnico = UnAnillo 12 "Un Anillo para gobernarlos a todos. Un Anillo para encontrarlos, un Anillo para atraerlos a todos y atarlos en las tinieblas." 3
	
frodo = UnHobbit{
	nombre = "Frodo",
	estatura = 106,
	salud = 100,
	fuerza = 50,
	deLaComarca = True,
	anillo = anilloUnico} 

desayuno hobbit = hobbit{nombre = "Errrp" ++ nombre hobbit, salud = salud hobbit + 5}
segundoDesayuno cantidadManzanas hobbit = hobbit { salud = salud hobbit + 4 * cantidadManzanas }
merienda = segundoDesayuno 2.desayuno

anilloUtn = UnAnillo 50  "Un Anillo para aprobar todas las materias. Un Anillo para recibirse, un Anillo para espantar a los vende humo y enviarlos a las tinieblas." 4


cambiarAnillo hobbit nuevoAnillo= hobbit{anillo=nuevoAnillo{cantidadDeDias= 0}}

estaHambriento (UnHobbit nombre estatura salud fuerza esDeLaComarca (UnAnillo descripcion peso dias)) = salud > 50 && fuerza > 10 || dias > 30



coeficienteDeIngrediente ingrediente | head ingrediente == 'z' = 2
			             | otherwise = 1
potenciaDeAlmuerzo ingrediente cantidad = div (length ingrediente * cantidad) (coeficienteDeIngrediente ingrediente)
almuerzo ingrediente cantidadUtilizada hobbit = 
	hobbit{fuerza= potenciaDeAlmuerzo ingrediente cantidadUtilizada + fuerza hobbit, salud = potenciaDeAlmuerzo ingrediente cantidadUtilizada + salud hobbit} 






diaDeViaje = (almuerzo "liebre" 3).merienda.(segundoDesayuno 1).desayuno
