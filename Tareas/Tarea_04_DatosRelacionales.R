install.packages("nycflights13")
library(nycflights13)
library(tidyverse)


# CAP 3 - DATA TRANSFORMATION ---------------------------------------------

# 3.1 Introduccion

glimpse(flights)

unique(flights$year)
unique(flights[1])

flights |> 
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(arr_delay = mean(arr_delay, na.rm = TRUE))


## 3.2 - ROWS --------------------------------------------------------------

# FILTER() -> muestra las filas que cumplen una condición sin modificar el orden. 

# Vuelos con destino a IAH
flights |> 
  filter(dest == "IAH")

# Vuelos con salida retrasada mas de dos horas
flights |> 
  filter(dep_delay > 120)

# Vuelos en fecha 1/1 
flights |> 
  filter(month == 1 & day == 1)

# Vuelos en enero o febrero
flights |> 
  filter(month == 1 | month == 2)

# Vuelos en enero o febrero -> lo mismo pero mas eficiente
flights |> 
  filter(month %in% c(1,2))

# Guardar el resultado en una variable
jan1 <- flights |> 
          filter(month == 1 & day == 1)

# ARRANGE() -> muestra todas las filas en un orden basado en el valor de la/las columnas. 

# Vuelos ordenados por d/m/y y hora de salida
flights |> 
  arrange(year, month, day, dep_time)

# Vuelos ordenados de forma descendente segun tiempo de retraso en la salida
flights |> 
  arrange(desc(dep_delay))

# DISTINCT() -> muestra las filas únicas de la tabla. 

# Vuelos unicos: sirve para eliminar duplicados
flights |> 
  distinct()

# Solo variables unicas pedidas de la tabla vuelos
flights |> 
  distinct(origin, dest)

# Variables unicas pedidas de la tabla vuelos pero manteniendo el resto de las columnas
flights |> 
  distinct(origin, dest, .keep_all = TRUE)

# Solo variables unicas pedidas de la tabla vuelos
flights |> 
  count(origin, dest, sort = TRUE)


### Ejercicios 3.2 ----------------------------------------------------------

# 1: In a single pipeline for each condition, find all flights that meet the condition:

  # Had an arrival delay of two or more hours
flights |> 
  filter(arr_delay >= 120)

  # Flew to Houston (IAH or HOU)
flights |>
  filter(dest %in% c("IAH", "HOU"))

flights |>
  filter(dest == "IAH" | dest == "HOU")

  # Were operated by United, American, or Delta
flights |>
  filter(carrier %in% c("UA", "AA", "DL"))

  # Departed in summer (July, August, and September)
flights |>
  filter(month %in% c(7,8,9))

  # Arrived more than two hours late but didn’t leave late
flights |>
  filter(arr_delay > 120 & dep_delay == 0)

  # Were delayed by at least an hour, but made up over 30 minutes in flight
flights |> 
  filter(dep_delay >= 60 & (dep_delay - arr_delay) > 30)

# 2: Sort flights to find the flights with the longest departure delays. 
flights |>
  arrange(desc(dep_delay))

  # Find the flights that left earliest in the morning.
flights |>
  arrange(sched_dep_time)


# 3: Sort flights to find the fastest flights. 
  # (Hint: Try including a math calculation inside of your function.)
flights |>
  arrange(desc(distance/air_time))

# 4: Was there a flight on every day of 2013?
flights |> 
  distinct(year, month, day)

if (nrow(flights |> distinct(year, month, day)) == 365) {
  print("Hubo un vuelo cada dia del año")
} else {
  print("No hubo un vuelo cada dia del año")
}

# 5: Which flights traveled the farthest distance? Which traveled the least distance?

flights |>
  arrange(desc(distance))

flights |>
  arrange(distance)

# 6: Does it matter what order you used filter() and arrange() if you’re using both? 
# Why/why not? Think about the results and how much work the functions would have to do.
flights |> 
  filter(arr_delay >= 120) |>
  arrange(desc(dep_delay))

flights |> 
  arrange(desc(dep_delay)) |>
  filter(arr_delay >= 120)

  # Es preferible filtrar primero y despues ordenar para evitar
  # que la maquina ordene datos que no van a formar parte de la tabla solicitada.


## 3.3 - COLUMNS -----------------------------------------------------------

# MUTATE() -> Crea nuevas columnas derivadas de las existentes.

flights |>
  mutate(
    gain = dep_delay - arr_delay, # tiempo ganado en el aire
    speed = distance / air_time * 60, # velocidad en millas x hora
    # por defecto las agrega a la derecha de la tabla
  )

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60, 
    .before = 1 # las agrega a la izquierda de la tabla
  )

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60, 
    .after = day # las agrega despues de la columna day
  )

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60, 
    gain_per_hour = gain / hours, 
    .keep = "used" # solo muestra las filas que fueron usadas en las nuevas columnas
  )

# SELECT() -> Seleccion de columnas.

# Selecciona las columnas indicadas
flights |> 
  select(year, month, day)

# Selecciona las columnas ENTRE las indicadas incluidas
flights |> 
  select(year: day)

# Selecciona las columnas EXCEPTO las indicadas incluidas
flights |> 
  select(!year: day)

# Selecciona las columnas cuyos datos son strings
flights |> 
  select(where(is.character))

# Selecciona una columna y le cambia el nombre al mismo tiempo
flights |>
  select(tail_num = tailnum) # el nuevo nombre va a la IZQUIERDA

# Otras funciones
  # starts_with("abc"): matches names that begin with “abc”.
  # ends_with("xyz"): matches names that end with “xyz”.
  # contains("ijk"): matches names that contain “ijk”.
  # num_range("x", 1:3): matches x1, x2 and x3.

# RENAME() -> Cambia el nombre de columnas.

flights |>
  rename(tail_num = tailnum)
  # != a select xq muestra todas las columnas, pero con el nuevo nombre. 

# RELOCATE() -> Cambia la posicion de columnas.

flights |> 
  relocate(time_hour, air_time) # por def las coloca a la izquierda

flights |> 
  relocate(year:dep_time, .after = time_hour)

flights |> 
  relocate(starts_with("arr"), .before = dep_time)

### Ejercicios 3.3 ----------------------------------------------------------

# 1: Compare dep_time, sched_dep_time, and dep_delay. 
# How would you expect those three numbers to be related?


# %/% -> devuelve el resultado de la division entera (sin decimales)
# %%  -> devuelve el resto de la division entera

flights |>
  mutate(
    dep_delay_calc = ((dep_time %/% 100 * 60 + dep_time %% 100) - (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100)), 
    .after = dep_delay 
  ) |>
  select(dep_time,sched_dep_time, dep_delay, dep_delay_calc)

  # dep_delay_calc == dep_delay

# 2: Brainstorm as many ways as possible to select:
# dep_time, dep_delay, arr_time, and arr_delay from flights

flights |> 
  select(dep_time, dep_delay, arr_time, arr_delay)

flights |> 
  select(dep_time:arr_delay)

flights |> 
  select(starts_with("dep"), starts_with("arr"))

# 3: What happens if you specify the name of the same variable multiple times in a select() call?

flights |> 
  select(dep_delay, arr_delay, dep_delay)

  # No repite la columna

# 4: What does the any_of() function do? Why might it be helpful in conjunction with this vector?

variables <- c("year", "month", "day", "dep_delay", "arr_delay", "country")
  # Agregué la columna "country" que no existe en el dataset

flights |> 
  select(any_of(variables)) # corre sin problemas porque no la toma en cuenta

flights |> 
  select(all_of(variables)) # da error porque no encuentra la columna 

  # any_of() selecciona las columnas que encuentre del vector en el dataset, si no encuentra alguna corre igualmente sin esa. 
  # El vector permite tener las variables de interes del proyecto sin estar escribiendolas cada vez. 

# 5: Does the result of running the following code surprise you? 
# How do the select helpers deal with upper and lower case by default? 
# How can you change that default?

flights |> select(contains("TIME")) 
  # Aunque este en mayusculas corre bien igual xq las funciones de select ignoran mayusculas y minusculas. 

  # Para cambiar el default y solo encontrar las efectivamente "TIME": 
flights |> select(contains("TIME", ignore.case = FALSE)) 

# 6: Rename air_time to air_time_min to indicate units of measurement 
  # and move it to the beginning of the data frame.

flights |> 
  rename(air_time_min = air_time) |>
  relocate(air_time_min)

# 7: Why doesn’t the following work, and what does the error mean?
flights |> 
  select(tailnum) |> 
  arrange(arr_delay)

  # No funciona porque selecciono primero solo esa columna, ya no considera la que se pide despues. 
flights |> 
  arrange(arr_delay)|> 
  select(tailnum)


## 3.4 - THE PIPE ----------------------------------------------------------

flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

# pipe: Ctrl+Shift+M = |> 


## 3.5 - GROUPS ------------------------------------------------------------

flights |> 
  group_by(month)

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), # Si no agrego el na.rm queda una columna de NA xq no puede calcular
    n = n() # Devuelve la cantidad de filas en cada grupo
  )

# df |> slice_head(n = 1) takes the first row from each group.
# df |> slice_tail(n = 1) takes the last row in each group.
# df |> slice_min(x, n = 1) takes the row with the smallest value of column x.
# df |> slice_max(x, n = 1) takes the row with the largest value of column x.
# df |> slice_sample(n = 1) takes one random row.

flights |> # Toma el vuelo que mas retraso de llegada tuvo de cada aeropuerto de destino
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |>
  relocate(dest, arr_delay)
  # "Problema": si hay valores empatados en un grupo, te los muestra 

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1, with_ties = FALSE) |> # Elimina los empatados
  relocate(dest, arr_delay)


daily <- flights |> 
  group_by(year, month, day)

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
    )

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "keep"
    )

daily |> 
  ungroup()

daily |> 
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    flights = n()
  )

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = month
  )

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )


### Ejercicios 3.5 ----------------------------------------------------------

# 1: Which carrier has the worst average delays? 
# Challenge: can you disentangle the effects of bad airports vs. bad carriers? 
# Why/why not? (Hint: think about flights |> group_by(carrier, dest) |> summarize(n()))

flights |> 
  group_by(carrier, dest) |> 
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE), 
    max_delay = max(arr_delay, na.rm = TRUE), 
    n = n()
  ) |> 
  arrange(desc(avg_delay))

# La aerolinea con peores retrasos es EV porque tiene mas cantidad de vuelos retrasados en promedio, 
# aunque no sea la de mayor retraso promedio. 

# 2: Find the flights that are most delayed upon departure to each destination.

flights |> 
  group_by(origin, dest) |> 
  slice_max(arr_delay, n = 1) |> 
  relocate(origin, dest)

# 3: How do delays vary over the course of the day? Illustrate your answer with a plot.
library(ggthemes)

# Creo una variable con los retrasos promedio por hora
avg_delay_by_hour <- flights |> 
  mutate(hour = sched_dep_time %/% 100 + (sched_dep_time %/% 100) /60) |> 
  filter(hour >= 5) |> # Saco el de hora 1 xq no tiene info
  group_by(hour) |> 
  summarize(
    avg_dep_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )

# Creo una variable con el maximo y el minimo diario
hitos <- avg_delay_by_hour |> 
  filter(avg_dep_delay == max(avg_dep_delay) | avg_dep_delay == min(avg_dep_delay))

# Grafico
ggplot(
  data = avg_delay_by_hour, 
  aes(x = hour, y = avg_dep_delay)
  ) +
geom_line(color = "blue", size = 1) +
geom_area(fill = "lightblue") +
geom_point(
  data = hitos, 
  color = "darkblue", 
  size = 2
) +
geom_text(data = hitos, aes(label = round(avg_dep_delay, 1)), 
            hjust = 1, vjust = -0.5, color = "darkblue", fontface = "bold") +
scale_x_continuous(breaks = 5:24) +
labs(
  title = "Evolución de los retrasos según la hora de salida programada",
  x = "Hora de salida programada",
  y = "Retraso en minutos"
) 

# 4: What happens if you supply a negative n to slice_min() and friends?

flights |> 
  slice_min(arr_delay, n = 1) |> 
  arrange(arr_delay)
# Devuelve las n filas con menores valores 

flights |> 
  slice_min(arr_delay, n = -1) |> 
  arrange(arr_delay)
# Devuelve todas las filas excepto las n con valores mas bajos

# 5: Explain what count() does in terms of the dplyr verbs you just learned. 
# What does the sort argument to count() do?

flights |> 
  group_by(dest) |> 
  summarize(n = n()) |> 
  arrange(desc(n))

flights |> 
  count(dest, sort = TRUE)

# 6: Suppose we have the following tiny data frame:

df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

# a) Write down what you think the output will look like, then check if you were correct, and describe what group_by() does.

df |>
  group_by(y) |> 
  summarize(n = n())
# Agrupa por y: a ; b. Cuenta la cantidad para cada uno. 

# b) Write down what you think the output will look like, then check if you were correct, and describe what arrange() does. 
# Also, comment on how it’s different from the group_by() in part (a).

df |>
  arrange(y)
# Ordena por y: a ; b. 

# c) Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does.

df |>
  group_by(y) |>
  summarize(mean_x = mean(x))
# Agrupa por y. Resume por la media de la variable x. 

# d) Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does. Then, comment on what the message says.
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))
# Agrupa por y y por z. Resume por la media de la variable x. 

# e) Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does. 
# How is the output different from the one in part (d)?

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")

# Agrupa por y y por z. Resume por la media de la variable x. 
# .groups = "drop_last" (default) -> resultado queda agrupado por y.
# .groups = "drop" -> resultado no tiene grupos.
# .groups = "keep" -> resultado mantiene la agrupación original (y y z).

# f) Write down what you think the outputs will look like, then check if you were correct, and describe what each pipeline does. How are the outputs of the two pipelines different?

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))
# Mantiene agrupado como fue pedido
df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))
# Crea una nueva columna en la base de datos


## 3.6 - CASE OF STUDY -----------------------------------------------------

install.packages("Lahman")
library(Lahman)

batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )

batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) + 
  geom_smooth(se = FALSE)



# CAP 19 - JOINS ----------------------------------------------------------

## CAP 19.2 - KEYS ---------------------------------------------------------

# PK: dato que hace unica a cada observacion
  # airlines -> carrier
  # airports -> faa
  # planes -> tailnum
  # weather -> origin & time_hour (compound)

# FK: dato que conecta dos tablas
  # flights$tailnum -> planes$tailnum
  # flights$carrier -> airlines$carrier
  # flights$origin -> airports$faa
  # flights$dest -> airports$faa
  # flights$origin-flights$time_hour -> weather$origin-weather$time_hour

# Verificar que sean efectivamente PK: no deben mostrar resultado
weather |> 
  count(time_hour, origin) |> 
  filter(n > 1)

# Verificar que no falten datos 
weather |> 
  filter(is.na(time_hour) | is.na(origin))

# Idenficar una PK
flights |> 
  count(time_hour, carrier, flight) |> 
  filter(n > 1)

# Crear una PK
flights2 <- flights |> 
  mutate(id = row_number(), .before = 1)


### Ejercicios 19.2 ---------------------------------------------------------

# 1: We forgot to draw the relationship between weather and airports in Figure 19.1. 
# What is the relationship and how should it appear in the diagram?
glimpse(weather)
glimpse(airports)

unique(airports$faa)
unique(weather$origin)

# Se relacionan con faa y origin. 

# 2: weather only contains information for the three origin airports in NYC. 
# If it contained weather records for all airports in the USA, what additional connection would it make to flights?

# Se podria conectar con dest. 

# 3: The year, month, day, hour, and origin variables almost form a compound key for weather, but there’s one hour that has duplicate observations. 
# Can you figure out what’s special about that hour?

weather |> 
  count(year, month, day, hour, origin) |> 
  filter(n > 1)

# Hay observaciones duplicadas por cambio de horario verano-invierno

# 4: We know that some days of the year are special and fewer people than usual fly on them (e.g., Christmas). 
# How might you represent that data as a data frame? 
# What would be the primary key? How would it connect to the existing data frames?

less_people_dates <- tribble(
  ~month, ~day, ~event, 
  1, 1, "New Year", 
  12, 24, "Christmas Eve", 
  12, 25, "Christmas Day",   
  12, 31, "New Years Eve", 
)

# PK: month & day

# 5: Draw a diagram illustrating the connections between the Batting, People, and Salaries data frames in the Lahman package. 
# Draw another diagram that shows the relationship between People, Managers, AwardsManagers. 
# How would you characterize the relationship between the Batting, Pitching, and Fielding data frames?

glimpse(Batting) # PK: playerID, yearID, stint
Batting |> 
  count(playerID, yearID, stint) |> 
  filter(n > 1)

glimpse(People) # PK: playerID
People |> 
  count(playerID) |> 
  filter(n > 1)

glimpse(Salaries) # PK: playerID, yearID, teamID
Salaries |> 
  count(playerID, yearID, teamID) |>
  filter(n > 1)


## CAP 19.3 - JOINS --------------------------------------------------------

# JOINS
# inner_join(x,y) -> Devuelve SOLO las filas que tienen coincidencias en AMBAS tablas. 
# left_join(x,y)  -> Devuelve TODAS las filas de la tabla X y SOLO las que tienen coincidencias Y. "Importar"
# right_join(x,y) -> Devuelve TODAS las filas de la tabla Y y SOLO las que tienen coincidencias X. "Importar" 
# full_join(x,y)  -> Devuelve TODAS las filas de AMBAS tablas, con NA en donde no hay coincidencias. 
# semi_join(x,y)  -> Devuelve SOLO las filas de la tabla X que tienen coincidencias en la tabla Y y elimina duplicados.  
# anti_join(x,y)  -> Devuelve SOLO las filas de la tabla X que NO tienen coincidencias en la tabla Y. 

flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)

flights2 |>
  left_join(airlines)

flights2 |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))

flights2 |> 
  left_join(planes |> select(tailnum, type, engines, seats))

flights2 |> 
  filter(tailnum == "N3ALAA") |> 
  left_join(planes |> select(tailnum, type, engines, seats))
# No hay info del avion  N3ALAA

flights2 |> 
  left_join(planes) 
# Selecciona mal xq hay columnas que se llaman igual pero indican cosas distintas y hay otra columna que si se corresponde, entonces genera error

flights2 |> 
  left_join(planes, join_by(tailnum))
# Indica con que columna conectarse, en este caso tienen el mismo nombre en las dos tablas

flights2 |> 
  left_join(airports, join_by(dest == faa))
# Indica con que columna conectarse, si tienen distinto nombre en cada columna

airports |> 
  semi_join(flights2, join_by(faa == origin))
# Devuelve las filas de airports que tienen match en flights2

flights2 |> 
  anti_join(airports, join_by(dest == faa)) |> 
  distinct(dest)
# Devuelve las filas de flights2 que NO tienen match en airports

### Ejercicios 19.3 ---------------------------------------------------------

# 1: Find the 48 hours (over the course of the whole year) that have the worst delays. 
# Cross-reference it with the weather data. Can you see any patterns?
glimpse(flights)
glimpse(weather)

weather_NYC <- weather |> # Tabla para evitar los triplicados
  group_by(time_hour) |> 
  summarize(
    temp = mean(temp, na.rm = TRUE),
    dewp = mean(dewp, na.rm = TRUE),
    humid = mean(humid, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    precip = mean(precip, na.rm = TRUE),
    visib = mean(visib, na.rm = TRUE)
  )

worst_48_hours <- flights |> 
  group_by(time_hour) |> 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  slice_max(avg_dep_delay, n = 48, with_ties = FALSE) |>
  arrange(desc(avg_dep_delay)) |> 
  left_join(weather_NYC, join_by(time_hour))

glimpse(worst_48_hours)

worst_48_hours_plot <- worst_48_hours |> 
  filter(avg_dep_delay < 250) # Saco el outlier


# Grafico retrasos - visibilidad: 
ggplot(
  data = worst_48_hours_plot, 
  aes(x = visib, y = avg_dep_delay)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Retrasos promedio vs visibilidad",
    x = "Visibilidad", 
    y = "Retraso promedio (min)"
  )
# No se observa gran correlacion: leve correlacion positiva


# Grafico retrasos - temperatura: 
ggplot(
  data = worst_48_hours_plot, 
  aes(x = temp, y = avg_dep_delay)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Retrasos promedio vs temperatura",
    x = "Temperatura", 
    y = "Retraso promedio (min)"
  )
# No se observa gran correlacion: leve correlacion negativa

# Grafico retrasos - precipitaciones: 
ggplot(
  data = worst_48_hours_plot, 
  aes(x = precip, y = avg_dep_delay)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Retrasos promedio vs precipitaciones",
    x = "Precipitaciones", 
    y = "Retraso promedio (min)"
  )
# No se observa gran correlacion: leve correlacion positiva pero pocos datos


# Grafico retrasos - velocidad del viento: 
ggplot(
  data = worst_48_hours_plot, 
  aes(x = wind_speed, y = avg_dep_delay)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Retrasos promedio vs velocidad del viento",
    x = "Velocidad del viento", 
    y = "Retraso promedio (min)"
  )
# No se observa gran correlacion: leve correlacion negativa


# 2: Imagine you’ve found the top 10 most popular destinations using this code:
# How can you find all flights to those destinations?
top_dest <- flights |>
  count(dest, sort = TRUE) |>
  head(10)

flights_to_top_dest <- flights |> 
  select(year:day, sched_dep_time, origin, dest, carrier) |> 
  semi_join(top_dest, join_by(dest))


# 3: Does every departing flight have corresponding weather data for that hour?

flights |> 
  anti_join(weather, join_by(origin == origin, time_hour == time_hour)) |> 
  nrow()
# Miro las filas de flights que no tienen datos en weather. 
# Hay vuelos que no tienen datos de clima para esa hora. 


# 4: What do the tail numbers that don’t have a matching record in planes have in common? 
# (Hint: one variable explains ~90% of the problems.)

flights |> 
  anti_join(planes, join_by(tailnum)) |> 
  count(carrier, sort = TRUE) |> 
  mutate(p = n / sum(n)) # % de vuelos sin datos de tailnum por carrier
# Miro las filas de flights$tailnum que no tienen datos en planes y el porcentaje. 
# Las aerolineas MQ y AA explican mas del 90% de los casos (48,3% + 42,9%)


# 5: Add a column to planes that lists every carrier that has flown that plane. 
# You might expect that there’s an implicit relationship between plane and airline, because each plane is flown by a single airline. 
# Confirm or reject this hypothesis using the tools you’ve learned in previous chapters.


# Verificacion de hipotesis: 
flights |>
  group_by(tailnum) |>
  summarize(n_carriers = n_distinct(carrier)) |>
  filter(n_carriers > 1) |> 
  nrow()
# Hay 18 aviones que fueron operados por mas de una aerolinea, no se cumple la hipotesis.  

carriers_per_plane <- flights |>
  filter(!is.na(tailnum)) |> # Saco los na de tailnum
  distinct(tailnum, carrier) |> # Tomo los valores unicos de tailnum y carrier para tener cada caso
  group_by(tailnum) |>
  summarize(all_carriers = str_flatten_comma(carrier, last = " y "))

# str_flatten_comma() -> pega los valores de carrier separados por coma, 
# si quisiera ultimo distinto:(last = " y "), separa con y por ejemplo. 
# en este caso me sirve para comprobar despues si hay multiples aerolineas. 

planes_and_carriers <- planes |>
  left_join(carriers_per_plane, by = "tailnum") |> 
  relocate(all_carriers, .after = tailnum)

planes_and_carriers |> 
  filter(str_detect(all_carriers, "y")) |> 
  select(tailnum, all_carriers)



# 6: Add the latitude and the longitude of the origin and destination airport to flights. 
# Is it easier to rename the columns before or after the join? After

flights_with_airports <- flights |>
  left_join(airports |> select(faa, lat, lon), join_by(origin == faa)) |> 
  rename(origin_lat = lat, origin_lon = lon) |> 
  left_join(airports |> select(faa, lat, lon), join_by(dest == faa)) |> 
  rename(dest_lat = lat, dest_lon = lon)

# 7: Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. 
# Here’s an easy way to draw a map of the United States:
# You might want to use the size or color of the points to display the average delay for each airport.

airports |>
  semi_join(flights, join_by(faa == dest)) |>
  ggplot(aes(x = lon, y = lat)) +
    borders("state") +
    geom_point() +
    coord_quickmap()

dest_avg_arr_delay <- flights |>
  group_by(dest) |> 
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE), n = n())

dest_avg_arr_delay |>
  inner_join(airports, join_by(dest == faa)) |>
  ggplot(aes(x = lon, y = lat)) +
    borders("state") +
    geom_point(aes(color = avg_arr_delay)) +
    scale_color_gradient(low = "lightblue", high = "darkblue") +
    coord_quickmap() +
    labs(
      title = "Distribución Espacial de Retrasos en EE.UU.",
      x = "Longitud", y = "Latitud",
      color = "Retraso Promedio (min)"
    ) 


# 8: What happened on June 13 2013? Draw a map of the delays, and then use Google to cross-reference with the weather.
flights |> 
  filter(year == 2013, month == 6, day == 13) |> 
  group_by(origin) |> 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE), n = n()) |> 
  inner_join(airports, join_by(origin == faa)) 

# Los vuelos tuvieron un promedio de entre 44 y 48hs de retraso
# Hubo tornados, granizo dañino y fuertes vientos y lluvias en la parte norte de la zona central de EEUU. 

# https://www.notiparaguay.com.py/eeuu/noticia-eeuu-tormentas-demoran-vuelos-derriban-tendidos-electricos-costa-atlantico-20130614004435.html

