install.packages("tidyverse")
library(tidyverse)

# Penguins dataframe
install.packages("palmerpenguins") 
library(palmerpenguins) # Llamado "penguins"

install.packages("ggthemes")
library(ggthemes)

penguins #Para que aparezca en Help: fn+f1

# VISUALIZACION: SCATTERPLOT

# Visualizacion con una regresion lineal para cada especie: 
ggplot(
  data = penguins, #indica el dataframe a usar para el grafico
  mapping = aes( #aesthetics
    x=flipper_length_mm, y=body_mass_g, #indica las variables a usar en los ejes del grafico
    color = species, shape = species) #"Scaling": indica que tiene que separar por color y forma segun la especie
) + 
  geom_point() + #agrega los puntos al grafico 
  geom_smooth(method = "lm") + #agrega una linea de tendencia al grafico, con metodo de regresion lineal (linear model)
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()


# Visualizacion con una sola regresion lineal: 
ggplot(
  data = penguins,
  mapping = aes(x=flipper_length_mm, y=body_mass_g) 
) + 
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") + #ahora agrega solo una regresion lineal
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()



### EJERCICIOS 1.2: 
# 1. How many rows are in penguins? How many columns?
nrow(penguins) #numero de filas: 344
ncol(penguins) #numero de columnas: 8

# 2. What does the bill_depth_mm variable in the penguins data frame describe? 
#   Read the help for ?penguins to find out.
penguins # fn+f1 para abrir la ayuda de la tabla de ayuda: a number denoting bill depth (millimeters)

# 3. Make a scatterplot of bill_depth_mm vs. bill_length_mm. 
#   That is, make a scatterplot with bill_depth_mm on the y-axis and bill_length_mm on the x-axis. 
#   Describe the relationship between these two variables.
ggplot(
  data = penguins, 
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) + 
  geom_point(mapping = aes(color = species, shape = species)) +
  labs(
    title = "Bill depth and bill length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Bill length (mm)", y = "Bill depth (mm)",
    color = "Species", shape = "Species"
  ) + 
  scale_color_colorblind()

# 4. What happens if you make a scatterplot of species vs. bill_depth_mm? 
#   What might be a better choice of geom?

ggplot(
  data = penguins, 
  mapping = aes(x = species, y = bill_depth_mm)
) + 
  geom_point() +
  labs(
    title = "Bill depth and species",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Species", y = "Bill depth (mm)"
  ) 

# Better choices: 
ggplot(
  data = penguins, 
  mapping = aes(x = species, y = bill_depth_mm)
) + 
  geom_boxplot() +
  labs(
    title = "Bill depth and species",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Species", y = "Bill depth (mm)"
  ) 

ggplot(
  data = penguins, 
  mapping = aes(x = species, y = bill_depth_mm)
) + 
  geom_violin() +
  labs(
    title = "Bill depth and species",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Species", y = "Bill depth (mm)"
  )

ggplot(
  data = penguins, 
  mapping = aes(x = species, y = bill_depth_mm)
) + 
  geom_jitter() +
  labs(
    title = "Bill depth and species",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Species", y = "Bill depth (mm)"
  )

# 5. Why does the following give an error and how would you fix it?

ggplot(data = penguins) + 
  geom_point()
# Error: geom_point necesita definir x e y

# Correccion: 
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm))

# Mejora: 
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm, 
                           color = species, shape = species)
             ) +
  labs(
    title = "Bill depth and bill length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Bill length (mm)", y = "Bill depth (mm)",
    color = "Species", shape = "Species"
  ) + 
  scale_color_colorblind()

# 6. What does the na.rm argument do in geom_point()? 
#   What is the default value of the argument? 
#   Create a scatterplot where you successfully use this argument set to TRUE.

# El argumento na.rm indica si se deben eliminar los valores NA antes de crear el gráfico.
# El valor predeterminado de na.rm es FALSE: los valores NA no se eliminarán automáticamente.
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm), na.rm = TRUE) +
  labs(
    title = "Bill depth and bill length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Bill length (mm)", y = "Bill depth (mm)"
  )

# 7. Add the following caption to the plot you made in the previous exercise: 
#   “Data come from the palmerpenguins package.” Hint: Take a look at the documentation for labs().
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm), na.rm = TRUE) +
  labs(
    title = "Bill depth and bill length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Bill length (mm)", y = "Bill depth (mm)",
    caption = "Data come from the palmerpenguins package."
  )

# 8. Recreate the following visualization. What aesthetic should bill_depth_mm be mapped to? 
#   And should it be mapped at the global level or at the geom level?

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)
 ) +
 geom_point(mapping = aes(color = bill_depth_mm)) +
 geom_smooth() + # No aplico regresion lineal porque quiero que siga a los puntos. 
 labs(
   title = "Flipper length and body mass",
   subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
   x = "Flipper length (mm)", y = "Body mass (g)",
   color = "Bill depth (mm)"
 )
