data Hobbit = UnHobbit {nombre::String, estatura::Int, salud::Int, fuerza::Int, esDeComarca::Bool, anillo::Anillo} deriving (Eq, Show)
data Anillo = UnAnillo {peso::Int, frase::String, diasCargado::Int} deriving (Eq, Show)

-- 1) Definir Hobbits y anillos

anilloUnico = UnAnillo {peso = 12, frase = "Un Anillo para gobernarlos a todos. Un Anillo para encontrarlos, un Anillo para atraerlos a todos y atarlos en las tinieblas.", diasCargado = 0}
anilloUtn = UnAnillo {peso = 1, frase = "Un Anillo para aprobar todas las materias. Un Anillo para recibirse, un Anillo para espantar a los vende humo y enviarlos a las tinieblas.", diasCargado = 0}
anilloPesado = UnAnillo {peso = 5000, frase = "Fuuuuuuuuuuuu", diasCargado = 0}


frodo = UnHobbit {nombre = "Frodo", estatura = 106, salud = 10, fuerza = 50, esDeComarca = True, anillo = anilloUnico}

bilbo = UnHobbit {nombre = "Bilbo", estatura = 100, salud = 35, fuerza = 40, esDeComarca = False, anillo = anilloUtn}

-- 2) Calcular el poder de un anillo

poderDelAnillo anillo = peso anillo * (length.frase) anillo

-- 3) Calcular la resistencia de un hobbit

resistenciaPorOrigen unHobbit
                  | esDeComarca unHobbit = estatura unHobbit * salud unHobbit + fuerza unHobbit
                  | otherwise = salud unHobbit * fuerza unHobbit

resistenciaMedia unHobbit = resistenciaPorOrigen unHobbit + resistenciaExtra unHobbit - (poderDelAnillo.anillo) unHobbit

esSuPrimeraLetra letra nombre = ((== letra).head) nombre

-- esSuPrimeraLetra letra = (== letra).head -- Aca esta con pointfree

resistenciaExtra unHobbit
                | esSuPrimeraLetra 'F' (nombre unHobbit) = 10
                | otherwise = 0

resistenciaHobbit = (max 0).resistenciaMedia

-- 4) Cambiar el anillo de un Hobbit

cambiarAnillo unHobbit nuevoAnillo = unHobbit{anillo = olvidarDiasCargadosPorHobbit nuevoAnillo}

-- 5) Implementar las comidas

desayuno unHobbit = unHobbit{nombre = (("Errrp"++).nombre) unHobbit , salud = salud unHobbit + 5}

segundoDesayuno cantManzanas unHobbit = unHobbit{fuerza = fuerza unHobbit + 4 * cantManzanas}

merienda = segundoDesayuno 2 . desayuno

-- 6) Averiguar que Hobbit tendra mayor resistencia despues de una merienda

cualTieneMayorResistencia unHobbit otroHobbit
                        | resistenciaPosMerienda unHobbit > resistenciaPosMerienda otroHobbit = unHobbit
                        | otherwise = otroHobbit

resistenciaPosMerienda = resistenciaHobbit.merienda

-- Parte 2

-- 2) Funcion adicional para cerar los dias que o cargo el hobbit

olvidarDiasCargadosPorHobbit anillo = anillo {diasCargado = 0}

-- 3) Averiguar si un hobbit tiene hambre

tieneHambre unHobbit = ((>50).salud) unHobbit && ((>=10).fuerza) unHobbit || ((>30).diasCargado.anillo) unHobbit

-- 4) Almuerzo

almuerzo ingredientePrincipal cantidad unHobbit = unHobbit{salud = salud unHobbit + 2 * cantidad, fuerza = fuerza unHobbit + bonusPorIngrediente ingredientePrincipal cantidad}

bonusPorIngrediente ingredientePrincipal cantidad
         | esSuPrimeraLetra 'z' ingredientePrincipal = fuerzaConseguida cantidad 2 ingredientePrincipal
         | otherwise = fuerzaConseguida cantidad 1 ingredientePrincipal

fuerzaConseguida cantidad divisor = (* cantidad).(flip div divisor).length

-- 5) Una mañana de viaje

manianaDeViaje = almuerzo "liebre" 3 . merienda . segundoDesayuno 1 . desayuno

-- Ejercicios hechos en clase de orden superior

-- 1)

aumentarSalud vida unHobbit = unHobbit {salud = salud unHobbit + vida}

wololo hobbits = map (aumentarSalud nivelDeAmenaza) hobbits

-- 2)

puedenViajar hobbits = filter esDeComarca hobbits

-- 3)

tieneElAnilloUnico hobbit = anilloUnico == anillo hobbit

tienenElAnilloUnico hobbits = any tieneElAnilloUnico hobbits

-- 4)

nivelDeAmenaza = 200

esAceptableSuSalud vidaMinima = (> vidaMinima).salud

estanSaludables hobbits = all (esAceptableSuSalud nivelDeAmenaza) hobbits

-- 5)

fuerzas hobbits = map fuerza hobbits

promedioDeLasFuerzas hobbits = div ((sum.fuerzas) hobbits) (length hobbits)

esUnGrupoFuerte hobbits = odd (promedioDeLasFuerzas hobbits)

