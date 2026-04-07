# Asignacion de valores a las variables
encuestado_id <- 1045
ingreso <- 350000.50 # Numerico: puede tener decimales
miembros_hogar <- 4L # Entero/Integer: no puede tener decimales (L: sirve para forzar que sea entero, "elimina la coma") 
estado <- "Ocupado" # Cadena de caracteres (String/Character)
busca_trabajo <- FALSE # Logical/Booleano (TRUE o FALSE)

horas_trabajadas <- 40.5 
edad_anios <- 28L
sector_actividad <- "Comercio"
categoria_ocupacional <- 'Cuentapropista'

# Identificacion de tipos de variables
class(encuestado_id)
class(ingreso)
class(miembros_hogar)
class(estado)
class(busca_trabajo)

# Funciones útiles
paste("Sector: ", sector_actividad, " - ", categoria_ocupacional) # Concatena elementos
grepl("propia", "Cuenta propia con local") # Busca un patrón dentro de una cadena de caracteres (devuelve TRUE o FALSE)
nchar(estado) # Cuenta el número de caracteres en una cadena de caracteres

# Operaciones con variables
salario_mensual <- 450000
salario_anual <- salario_mensual * 13

# Operadores de comparacion: == igual ; > mayor ; < menor ; 
#                            >= mayor o igual ; <= menor o igual ; != distinto
es_mayor_edad <- edad_anios >= 18 # Devuelve T si la edad es mayor, sino F
es_desocupado <- estado == "Desocupado" # Devuelve T si es cierto, sino F
# Operadores logicos: & -> AND ; | -> OR ; ! -> NOT
es_pea <- (estado == "Ocupado" | estado == "Desocupado") & edad_anios >= 16


# ESTRUCTURA IF...ELSE
if (salario_mensual < 200000) {
  print("Por debajo del salario mínimo")
}

if (salario_mensual > 800000) {
  decil <- "Alto"
} else if (salario_mensual >= 300000) {
  decil <- "Medio"
} else {
  decil <- "Bajo"
}

# ESTRUCTURA WHILE
meses_busqueda <- 0
while (meses_busqueda < 3) {
  print(paste("Mes", meses_busqueda, "buscando empleo"))
  meses_busqueda <- meses_busqueda + 1
}

meses_busqueda <- 0
while (TRUE) {
  meses_busqueda <- meses_busqueda + 1
  if (meses_busqueda == 2) {
    print("Empleo encontrado")
    break
  }
}

# ESTRUCTURA FOR
salarios_hora <- c(1500, 2200, 1800, 3100)
for (salario in salarios_hora) {
  print(salario * 8)
}

for (i in 1:length(salarios_hora)) { 
  salarios_hora[i] <- salarios_hora[i] * 8
  print(salarios_hora[i])
} # Obtengo los mismos resultados pero se cambiaron los valores de las variables 

# El 1 adelante indica que el indice comienza en 1,
# El length() devuelve la cantidad de elementos del vector
# Entonces i va desde 1 hasta el tamaño del vector.


# VECTORES Y LISTAS
edades_hogar <- c(45, 42, 16, 12) # Vector: Elementos del mismo tipo
promedio_edad <- mean(edades_hogar)
jefe_hogar <- list( # Lista: Contenedores heterogéneos
  id = 101,
  nombre = "Carlos",
  edades_familia = edades_hogar,
  es_propietario = TRUE
)

class(jefe_hogar)
length(jefe_hogar)

jefe_hogar$canasta <- list ( #Puedo agregar info a la lista despues de haberla creado
  x1="carne", 
  x2="verdura", 
  x3="leche",
  x4="ropa"
)

length(jefe_hogar)

jefe_hogar$canasta$x1$tipo <- list( # Puedo agregar info a elementos (lista) dentro de la lista 
  carne1 = "vacuna",
  carne2 = "pollo",
  carne3 = "cerdo"
)

length(jefe_hogar)


# MATRICES: Estructuras 2D del mismo tipo
# nrow: número de filas, byrow TRUE: llena la matriz por filas

datos_transicion <- c(80, 20, 15, 85)
matriz_transicion <- matrix(datos_transicion, nrow = 2, byrow = TRUE)
matriz_transicion2 <- matrix(datos_transicion, nrow = 2, byrow = FALSE)
class(matriz_transicion)

# ARRAY: Estructura multidimensional del mismo tipo
panel_laboral <- array(1:12, dim = c(2,2,3)) # 2 filas, 2 columnas, 3 capas (dimensiones) (seria un "cubo" en este caso)
panel_laboral


# DATA FRAMES: Estructuras tabulares heterogéneas
microdatos <- data.frame(
  id_persona = c(1, 2, 3),
  edad = c(34, 19, 52),
  ingreso = c(450000, 0, 780000),
  trabajo_semana_pasada = c(TRUE, FALSE, TRUE)
)

# Inspeccion: 
colnames(microdatos) # Indica los nombres de las columnas
rownames(microdatos) # Indica los nombres de las filas (por defecto son números del 1 al n)
str(microdatos) # Estructura del data frame
summary(microdatos) # Resumen estadístico de cada variable (min, max, mean, etc.)

microdatos$ingreso # Acceso a una columna específica

# FACTORES: 
vector_estados <- c("Ocupado", "Desocupado", "Inactivo", "Ocupado")
estado_factor <- factor(vector_estados)
estado_factor
levels(estado_factor) # Solo visualiza los niveles del factor

nivel_edu <- factor(c("Secundario", "Universitario", "Primario"),
                    levels = c("Primario", "Secundario", "Universitario"),
                    ordered = TRUE) 
# Ordena los niveles del factor según el orden que se le dio,
# ordered = TRUE indica que se tienen que ordenar de esa manera. 

nivel_edu2 <- factor(c("Secundario", "Universitario", "Primario", "Primario", "Universitario", "Secundario", "Primario", "Universitario", "Universitario", "Secundario"),
                    levels = c("Primario", "Secundario", "Universitario"),
                    ordered = TRUE) 
nivel_edu2
levels(nivel_edu2) # Solo visualiza los niveles del factor Y ordenados como le fue indicado. 


