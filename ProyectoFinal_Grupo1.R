
# ANÁLISIS MLER - GRUPO 1 -------------------------------------------------


# Librerías y configuración -----------------------------------------------

rm(list = ls())

library(arrow)
library(haven)
library(data.table)
library(igraph)
library(ggraph)
library(ggplot2)
library(tidygraph)
library(scales)
library(dplyr)
library(RColorBrewer)
library(openxlsx)
library(reshape2)   

setwd("C:/Users/danii/Documents/UBA/Ciencia_de_datos/ProyectoFinal")
RUTA_PARQUET <- "MLER.parquet"
DIR_SALIDA   <- "MLER_Resultados"
dir.create(DIR_SALIDA, showWarnings = FALSE)

ds  <- open_dataset(RUTA_PARQUET)


## Periodos ----------------------------------------------------------------

PERIODOS <- list(
  list(num = 1, nombre = "Menem",              inicio = 199601L, fin = 199912L),
  list(num = 2, nombre = "Crisis",             inicio = 200001L, fin = 200305L), # NK asume el 25/5/03
  list(num = 3, nombre = "Nestor_Kirchner",    inicio = 200306L, fin = 200712L),
  list(num = 4, nombre = "CK_Primer_Mandato",  inicio = 200801L, fin = 201112L),
  list(num = 5, nombre = "CK_Segundo_Mandato", inicio = 201201L, fin = 201512L),
  list(num = 6, nombre = "Macri",              inicio = 201601L, fin = 201912L)
)


TITULOS_PERIODO <- c(
  "1" = "Menem (1996–1999)",
  "2" = "Período de Crisis (2000–2003)",
  "3" = "Néstor Kirchner (2004–2007)",
  "4" = "Cristina K. — 1° Mandato (2008–2011)",
  "5" = "Cristina K. — 2° Mandato (2012–2015)",
  "6" = "Macri (2016–2019)"
)


## Sectores (letra) ----------------------------------------------------------------
DISTINCT_COLORS <- c(
  "Sin definir"                                          = "#7F8C8D",
  "A - Agricultura, ganadería, caza y silvicultura"      = "#2ECC71",
  "B - Pesca y servicios conexos"                        = "#1ABC9C",
  "C - Explotación de minas y canteras"                  = "#34495E",
  "D - Industria manufacturera"                          = "#E74C3C",
  "E - Electricidad, gas y agua"                         = "#F1C40F",
  "F - Construcción"                                     = "#E67E22",
  "G - Comercio al por mayor y menor"                    = "#3498DB",
  "H - Hotelería y restaurantes"                         = "#9B59B6",
  "I - Transporte, almacenamiento y comunicaciones"      = "#16A085",
  "J - Intermediación financiera"                        = "#2980B9",
  "K - Servicios inmobiliarios, empresariales y alquiler"= "#8E44AD",
  "M - Enseñanza"                                        = "#D35400",
  "N - Servicios sociales y de salud"                    = "#C0392B",
  "O - Servicios comunitarios, sociales y personales"    = "#F39C12"
)

label_letra <- data.table(
  letra      = 0:14,
  letra_desc = names(DISTINCT_COLORS)
)


## Sectores (r34) ----------------------------------------------------------------

r34_txt <- c(
  "0 Sin definir",
  "111 Cultivo de cereales, oleaginosas y forrajeras",
  "112 Cultivo de hortalizas, legumbres y flores",
  "113 Cultivo de frutas y nueces",
  "114 Cultivos industriales y especias",
  "115 Producción de semillas",
  "121 Cría de ganado y producción de leche/lana",
  "122 Producción de granja y cría de animales",
  "141 Servicios agrícolas",
  "142 Servicios pecuarios",
  "150 Caza y captura de animales vivos",
  "201 Silvicultura",
  "202 Extracción de productos forestales",
  "203 Servicios forestales",
  "501 Pesca y recolección de productos marinos",
  "502 Acuicultura",
  "503 Servicios para la pesca",
  "1110 Extracción de petróleo crudo y gas natural",
  "1120 Servicios relacionados con petróleo y gas",
  "1310 Extracción de minerales de hierro",
  "1320 Extracción de minerales metalíferos no ferrosos",
  "1411 Extracción de rocas ornamentales",
  "1412 Extracción de piedra caliza y yeso",
  "1413 Extracción de arenas y canto rodado",
  "1414 Extracción de arcilla y caolín",
  "1421 Extracción de minerales para abonos",
  "1422 Extracción de sal",
  "1429 Minas y canteras n.c.p.",
  "1511 Producción y procesamiento de carne",
  "1512 Elaboración de pescado",
  "1513 Preparación de frutas y hortalizas",
  "1514 Elaboración de aceites y grasas vegetales",
  "1520 Elaboración de productos lácteos",
  "1531 Elaboración de productos de molinería",
  "1532 Elaboración de almidones",
  "1533 Alimentos preparados para animales",
  "1541 Elaboración de productos de panadería",
  "1542 Elaboración de azúcar",
  "1543 Elaboración de cacao, chocolate y confitería",
  "1544 Elaboración de pastas alimenticias",
  "1549 Productos alimenticios n.c.p.",
  "1551 Destilación y mezcla de bebidas alcohólicas",
  "1552 Elaboración de vinos y bebidas fermentadas",
  "1553 Elaboración de cerveza y malta",
  "1554 Elaboración de bebidas no alcohólicas y aguas",
  "1600 Elaboración de productos de tabaco",
  "1711 Hilandería y tejeduría de textiles",
  "1712 Acabado de productos textiles",
  "1721 Artículos confeccionados de materiales textiles",
  "1722 Fabricación de tapices y alfombras",
  "1723 Fabricación de cuerdas, cordeles y redes",
  "1729 Productos textiles n.c.p.",
  "1730 Fabricación de tejidos de punto",
  "1810 Fabricación de prendas de vestir",
  "1820 Terminación y teñido de pieles",
  "1911 Curtido y terminación de cueros",
  "1912 Fabricación de maletas, bolsos y marroquinería",
  "1920 Fabricación de calzado",
  "2010 Aserrado y cepillado de madera",
  "2021 Fabricación de hojas de madera y tableros",
  "2022 Partes y piezas de carpintería para edificios",
  "2023 Fabricación de recipientes de madera",
  "2029 Productos de madera n.c.p. y corcho",
  "2101 Fabricación de pasta de madera, papel y cartón",
  "2102 Fabricación de papel ondulado y envases",
  "2109 Artículos de papel y cartón",
  "2211 Edición de libros y folletos",
  "2212 Edición de periódicos y revistas",
  "2213 Edición de grabaciones",
  "2219 Edición n.c.p.",
  "2221 Impresión",
  "2222 Servicios relacionados con la impresión",
  "2230 Reproducción de grabaciones",
  "2310 Fabricación de productos de hornos de coque",
  "2320 Refinación del petróleo",
  "2330 Elaboración de combustible nuclear",
  "2411 Fabricación de sustancias químicas básicas",
  "2412 Fabricación de abonos y compuestos de nitrógeno",
  "2413 Fabricación de plásticos y caucho sintético",
  "2421 Fabricación de plaguicidas y agroquímicos",
  "2422 Fabricación de pinturas, barnices y tintas",
  "2423 Fabricación de productos farmacéuticos",
  "2424 Jabones, detergentes y cosméticos",
  "2429 Productos químicos n.c.p.",
  "2430 Fabricación de fibras manufacturadas",
  "2511 Cubiertas y cámaras de caucho",
  "2519 Productos de caucho n.c.p.",
  "2520 Fabricación de productos de plástico",
  "2610 Fabricación de vidrio y derivados",
  "2691 Cerámica no refractaria no estructural",
  "2692 Productos de cerámica refractaria",
  "2693 Cerámica no refractaria estructural",
  "2694 Elaboración de cemento, cal y yeso",
  "2695 Artículos de hormigón, cemento y yeso",
  "2696 Corte, tallado y acabado de la piedra",
  "2699 Productos minerales no metálicos n.c.p.",
  "2710 Industrias básicas de hierro y acero",
  "2720 Productos primarios de metales preciosos y no ferrosos",
  "2731 Fundición de hierro y acero",
  "2732 Fundición de metales no ferrosos",
  "2811 Productos metálicos estructurales",
  "2812 Tanques, depósitos y recipientes de metal",
  "2813 Fabricación de generadores de vapor",
  "2891 Forjado, prensado y estampado de metales",
  "2892 Tratamiento y revestimiento de metales",
  "2893 Cuchillería, herramientas de mano y ferretería",
  "2899 Productos elaborados de metal n.c.p.",
  "2911 Motores y turbinas",
  "2912 Bombas, compresores, grifos y válvulas",
  "2913 Cojinetes, engranajes y piezas de transmisión",
  "2914 Hornos, hogares y quemadores",
  "2915 Equipo de elevación y manipulación",
  "2919 Maquinaria de uso general n.c.p.",
  "2921 Fabricación de maquinaria agropecuaria",
  "2922 Fabricación de máquinas herramienta",
  "2923 Fabricación de maquinaria metalúrgica",
  "2924 Maquinaria para minas, canteras y construcción",
  "2925 Maquinaria para elaboración de alimentos y bebidas",
  "2926 Maquinaria para productos textiles y cueros",
  "2927 Fabricación de armas y municiones",
  "2929 Maquinaria de uso especial n.c.p.",
  "2930 Aparatos de uso doméstico n.c.p.",
  "3000 Maquinaria de oficina, contabilidad e informática",
  "3110 Motores, generadores y transformadores eléctricos",
  "3120 Aparatos de distribución y control eléctrico",
  "3130 Hilos y cables aislados",
  "3140 Acumuladores, pilas y baterías",
  "3150 Lámparas eléctricas y equipo de iluminación",
  "3190 Equipo eléctrico n.c.p.",
  "3210 Tubos, válvulas y componentes electrónicos",
  "3220 Transmisores de radio y telefonía",
  "3230 Receptores de radio, televisión y sonido",
  "3310 Aparatos e instrumentos médicos y de medición",
  "3311 Equipo médico y quirúrgico y aparatos ortopédicos",
  "3320 Instrumentos de óptica y equipo fotográfico",
  "3330 Fabricación de relojes",
  "3410 Fabricación de vehículos automotores",
  "3420 Carrocerías para automotores y remolques",
  "3430 Partes y accesorios para automotores",
  "3511 Construcción y reparación de buques",
  "3512 Construcción de embarcaciones de recreo",
  "3520 Locomotoras y material rodante ferroviario",
  "3530 Fabricación y reparación de aeronaves",
  "3590 Motocicletas y bicicletas",
  "3599 Equipo de transporte n.c.p.",
  "3610 Fabricación de muebles y colchones",
  "3691 Joyas y artículos conexos",
  "3692 Fabricación de instrumentos de música",
  "3693 Fabricación de artículos de deporte",
  "3694 Fabricación de juegos y juguetes",
  "3699 Industrias manufactureras n.c.p.",
  "3710 Reciclamiento de desperdicios metálicos",
  "3720 Reciclamiento de desperdicios no metálicos",
  "4011 Generación de energía eléctrica",
  "4012 Transporte de energía eléctrica",
  "4013 Distribución de energía eléctrica",
  "4020 Fabricación y distribución de gas por tuberías",
  "4030 Suministro de vapor y agua caliente",
  "4100 Captación, depuración y distribución de agua",
  "4511 Demolición y voladura de edificios",
  "4512 Perforación y sondeo",
  "4519 Movimiento de suelos y preparación de terrenos",
  "4520 Construcción de edificios e ingeniería civil",
  "4531 Instalaciones eléctricas y electromecánicas",
  "4532 Aislamiento térmico, acústico e hídrico",
  "4533 Instalaciones de gas, agua y sanitarios",
  "4539 Instalaciones para edificios y obras n.c.p.",
  "4541 Instalaciones de carpintería y herrería de obra",
  "4542 Terminación y revestimiento de paredes y pisos",
  "4543 Colocación de cristales en obra",
  "4544 Pintura y trabajos de decoración",
  "4549 Terminación de edificios y obras n.c.p.",
  "4550 Alquiler de equipo de construcción con operarios",
  "5011 Venta de vehículos automotores nuevos",
  "5012 Venta de vehículos automotores usados",
  "5020 Mantenimiento y reparación de vehículos",
  "5031 Venta al por mayor de partes de vehículos",
  "5032 Venta al por menor de partes de vehículos",
  "5050 Venta al por menor de combustible para vehículos",
  "5111 Venta al por mayor en comisión de productos agrícolas",
  "5119 Venta al por mayor en comisión n.c.p.",
  "5121 Venta al por mayor de materias primas agropecuarias",
  "5122 Venta al por mayor de alimentos",
  "5123 Venta al por mayor de bebidas",
  "5124 Venta al por mayor de cigarrillos y tabaco",
  "5131 Venta al por mayor de productos textiles y prendas",
  "5132 Venta al por mayor de libros, revistas y papel",
  "5133 Venta al por mayor de productos farmacéuticos",
  "5134 Venta al por mayor de artículos de óptica",
  "5135 Venta al por mayor de muebles y artefactos",
  "5139 Venta al por mayor de artículos de uso doméstico",
  "5141 Venta al por mayor de combustibles",
  "5142 Venta al por mayor de metales y minerales",
  "5143 Venta al por mayor de madera y construcción",
  "5149 Venta al por mayor de productos intermedios n.c.p.",
  "5151 Venta al por mayor de máquinas de uso especial",
  "5152 Venta al por mayor de máquinas-herramienta",
  "5153 Venta al por mayor de vehículos y transporte",
  "5154 Venta al por mayor de muebles industriales",
  "5159 Venta al por mayor de máquinas y equipo n.c.p.",
  "5190 Venta al por mayor de mercancías n.c.p.",
  "5211 Venta al por menor con predominio de alimentos",
  "5212 Venta al por menor sin predominio de alimentos",
  "5221 Venta al por menor de almacén y fiambrería",
  "5222 Venta al por menor de carnes y granja",
  "5223 Venta al por menor de frutas y hortalizas",
  "5224 Venta al por menor de pan y panadería",
  "5225 Venta al por menor de bebidas",
  "5229 Venta al por menor de alimentos n.c.p. y tabaco",
  "5231 Venta al por menor de productos farmacéuticos",
  "5232 Venta al por menor de productos textiles",
  "5233 Venta al por menor de prendas y accesorios",
  "5234 Venta al por menor de calzado y marroquinería",
  "5235 Venta al por menor de muebles y artefactos",
  "5236 Venta al por menor de materiales de construcción",
  "5237 Venta al por menor de artículos de óptica y joyería",
  "5238 Venta al por menor de libros, revistas y librería",
  "5239 Venta al por menor en comercios especializados n.c.p.",
  "5241 Venta al por menor de muebles usados",
  "5242 Venta al por menor de libros usados",
  "5249 Venta al por menor de artículos usados n.c.p.",
  "5251 Venta al por menor por correo, TV, internet",
  "5252 Venta al por menor en puestos móviles",
  "5259 Venta al por menor no en establecimientos",
  "5261 Reparación de calzado y marroquinería",
  "5262 Reparación de artículos eléctricos del hogar",
  "5269 Reparación de efectos personales n.c.p.",
  "5511 Servicios de alojamiento en camping",
  "5512 Servicios de alojamiento excepto camping",
  "5521 Restaurantes, bares y cafés con servicio de mesa",
  "5522 Preparación y venta de comidas para llevar",
  "6010 Servicio de transporte ferroviario",
  "6021 Servicio de transporte automotor de cargas",
  "6022 Servicio de transporte automotor de pasajeros",
  "6030 Servicio de transporte por tuberías",
  "6110 Servicio de transporte marítimo",
  "6120 Servicio de transporte fluvial",
  "6200 Servicio de transporte aéreo",
  "6310 Servicios de manipulación de carga",
  "6320 Servicios de almacenamiento y depósito",
  "6331 Servicios complementarios para transporte terrestre",
  "6332 Servicios complementarios para transporte por agua",
  "6333 Servicios complementarios para transporte aéreo",
  "6340 Agencias de viaje y asistencia turística",
  "6350 Gestión y logística para el transporte",
  "6410 Servicios de correos",
  "6420 Servicios de telecomunicaciones",
  "6521 Servicios de entidades financieras bancarias",
  "6522 Servicios de entidades financieras no bancarias",
  "6590 Servicios financieros n.c.p.",
  "6610 Servicios de seguros",
  "6620 Administración de fondos de jubilaciones (AFJP)",
  "6711 Servicios de administración de mercados financieros",
  "6712 Servicios bursátiles de mediación",
  "6719 Servicios auxiliares a la actividad financiera n.c.p.",
  "6721 Servicios auxiliares a los servicios de seguros",
  "6722 Servicios auxiliares a la administración de AFJP",
  "7010 Servicios inmobiliarios por cuenta propia",
  "7020 Servicios inmobiliarios por retribución o contrata",
  "7111 Alquiler de equipo de transporte terrestre",
  "7112 Alquiler de equipo de transporte acuático",
  "7113 Alquiler de equipo de transporte aéreo",
  "7120 Alquiler de maquinaria y equipo n.c.p.",
  "7130 Alquiler de efectos personales y enseres",
  "7210 Servicios de consultores en equipo de informática",
  "7220 Consultores en informática y suministro de software",
  "7230 Procesamiento de datos",
  "7240 Servicios relacionados con bases de datos",
  "7250 Mantenimiento de maquinaria de oficina e informática",
  "7290 Actividades de informática n.c.p.",
  "7300 Investigación y desarrollo",
  "7410 Servicios jurídicos, contabilidad y gestión",
  "7421 Arquitectura, ingeniería y asesoramiento técnico",
  "7422 Ensayos y análisis técnicos",
  "7430 Servicios de publicidad",
  "7491 Obtención y dotación de personal",
  "7492 Servicios de investigación y seguridad",
  "7493 Servicios de limpieza de edificios",
  "7494 Servicios de fotografía",
  "7495 Servicios de envase y empaque",
  "7496 Servicios de fotocopia e impresiones",
  "7499 Servicios empresariales n.c.p.",
  "7500 Agencias de empleo eventual",
  "8000 Enseñanza",
  "8510 Servicios relacionados con la salud humana",
  "8520 Servicios veterinarios",
  "8530 Servicios sociales",
  "9000 Eliminación de desperdicios y saneamiento",
  "9100 Servicios de asociaciones n.c.p.",
  "9211 Producción y distribución de filmes y videocintas",
  "9212 Exhibición de filmes y videocintas",
  "9213 Servicios de radio y televisión",
  "9214 Servicios teatrales, musicales y artísticos n.c.p.",
  "9219 Servicios de espectáculos artísticos n.c.p.",
  "9220 Servicios de agencias de noticias",
  "9230 Servicios de bibliotecas, archivos y museos",
  "9241 Servicios para prácticas deportivas",
  "9249 Servicios de esparcimiento n.c.p.",
  "9301 Lavado y limpieza de artículos de tela y cuero",
  "9302 Servicios de peluquería y tratamientos de belleza",
  "9303 Pompas fúnebres y servicios conexos",
  "9309 Servicios n.c.p."
)

label_r34 <- data.table(
  r34      = as.integer(sub("^([0-9]+) .*", "\\1", r34_txt)),
  r34_desc = sub("^[0-9]+ (.*)", "\\1", r34_txt)
)





# Carga y limpieza --------------------------------------------------------

cargar_periodo <- function(ds, inicio, fin) {
  dt <- ds |>
    filter(tiempo >= inicio, tiempo <= fin) |>
    select(id_trabajador, tiempo, r34, letra, rem_tot) |>
    collect() |>
    as.data.table()

  dt[, `:=`(
    id_trabajador = as.integer(as.numeric(zap_labels(id_trabajador))),
    tiempo        = as.integer(as.numeric(zap_labels(tiempo))),
    r34           = as.integer(as.numeric(zap_labels(r34))),
    letra         = as.integer(as.numeric(zap_labels(letra))),
    rem_tot       = as.numeric(zap_labels(rem_tot))
  )]

  # Filtros de calidad
  dt <- dt[
    tiempo >= inicio & tiempo <= fin &
    !is.na(r34) & !is.na(letra) & !is.na(rem_tot) &
    r34 != 0L   & letra != 0L
  ]
  return(dt)
}

resolver_pluriempleo <- function(dt) {
  setorder(dt, id_trabajador, tiempo, -rem_tot)
  dt <- dt[, .SD[1L], by = .(id_trabajador, tiempo)]
  return(dt)
}


# Construcción de nodos y links -------------------------------------------

calcular_tamano_nodos <- function(dt) {
  n_meses <- dt[, uniqueN(tiempo)]
  map_r34_letra <- dt[, .(letra = as.integer(names(which.max(table(letra))))), by = r34]

  tamano <- dt[, .(n = .N), by = .(r34, tiempo)][
    , .(T_promedio = sum(n) / n_meses), by = r34]
  tamano <- merge(tamano, map_r34_letra, by = "r34", all.x = TRUE)
  tamano <- merge(tamano, label_letra,   by = "letra", all.x = TRUE)
  tamano <- merge(tamano, label_r34,     by = "r34",   all.x = TRUE)
  setorder(tamano, -T_promedio)
  return(tamano)
}

construir_transiciones <- function(dt) {
  setkey(dt, id_trabajador, tiempo)
  dt[, r34_sig    := shift(r34, type = "lead"),           by = id_trabajador]
  dt[, mismo_trab := id_trabajador == shift(id_trabajador, type = "lead")]

  aristas <- dt[
    !is.na(r34_sig) & mismo_trab == TRUE & r34 != r34_sig,
    .(peso = .N),
    by = .(origen = r34, destino = r34_sig)
  ]
  dt[, `:=`(r34_sig = NULL, mismo_trab = NULL)]
  setorder(aristas, origen, destino)
  return(aristas)
}

construir_matrices <- function(aristas) {
  nodos <- sort(unique(c(aristas$origen, aristas$destino)))
  n     <- length(nodos)
  idx_o <- match(aristas$origen,  nodos)
  idx_d <- match(aristas$destino, nodos)

  mat_p <- matrix(0L, n, n, dimnames = list(nodos, nodos))

  for (k in seq_len(nrow(aristas))) {
    mat_p[idx_o[k], idx_d[k]] <- aristas$peso[k]
  }
  
  list(ponderada = mat_p, nodos = nodos)
}

construir_grafo <- function(dt, aristas = NULL, tamano = NULL) {
  if (is.null(aristas)) aristas <- construir_transiciones(dt)
  if (is.null(tamano))  tamano  <- calcular_tamano_nodos(dt)

  mat <- construir_matrices(aristas)

  g <- graph_from_data_frame(
    d         = aristas,
    directed  = TRUE,
    vertices  = data.frame(name = mat$nodos)
  )
  E(g)$weight <- aristas$peso

  # Agregar atributos a nodos
  V(g)$T_promedio <- tamano$T_promedio[match(as.integer(V(g)$name), tamano$r34)]
  V(g)$letra      <- tamano$letra[     match(as.integer(V(g)$name), tamano$r34)]
  V(g)$letra_desc <- tamano$letra_desc[match(as.integer(V(g)$name), tamano$r34)]
  V(g)$r34_desc   <- tamano$r34_desc[  match(as.integer(V(g)$name), tamano$r34)]

  V(g)$letra[is.na(V(g)$letra)] <- 0L
  V(g)$letra_desc[is.na(V(g)$letra_desc)] <- "Sin definir"

  return(g)
}


# Características descriptivas --------------------------------------------

calcular_caracteristicas <- function(dt, aristas = NULL) {
  if (is.null(aristas)) aristas <- construir_transiciones(dt)

  n_trab  <- dt[, uniqueN(id_trabajador)]
  n_meses <- dt[, uniqueN(tiempo)]

  # Actividades con mayor frecuencia de trabajadores únicos
  frec_sector <- dt[, .(N_Unicos = uniqueN(id_trabajador)), by = r34]
  frec_sector <- merge(frec_sector, label_r34, by = "r34", all.x = TRUE)[order(-N_Unicos)]

  # Sectores extractores (mayor flujo de entrada) y expulsores (mayor salida)
  entradas  <- aristas[, .(Entradas  = sum(peso)), by = .(r34 = destino)][order(-Entradas)]
  salidas   <- aristas[, .(Salidas   = sum(peso)), by = .(r34 = origen )][order(-Salidas)]
  extractores <- merge(entradas, label_r34, by = "r34", all.x = TRUE)
  expulsores  <- merge(salidas,  label_r34, by = "r34", all.x = TRUE)

  list(
    n_trabajadores  = n_trab,
    n_meses         = n_meses,
    frec_sector     = frec_sector,
    extractores     = extractores,
    expulsores      = expulsores,
    tamano          = calcular_tamano_nodos(dt)
  )
}


# Relaciones creadas y destruidas -----------------------------------------

relaciones_creadas_destruidas <- function(ds, dir_out = DIR_SALIDA) {
  message("Calculando nacimientos y muertes absolutas de relaciones...")
  
  lista_metricas <- list()
  lista_nuevas_absolutas <- list()
  lista_muertas_absolutas <- list()
  
  aristas_previas <- NULL
  
  for (i in seq_along(PERIODOS)) {
    per <- PERIODOS[[i]]
    
    # Carga COMPLETA del período (sin filtrar pesos)
    dt <- cargar_periodo(ds, per$inicio, per$fin)
    if (nrow(dt) == 0) next
    dt <- resolver_pluriempleo(dt)
    aristas <- construir_transiciones(dt)
    aristas[, id_rel := paste(origen, destino, sep = "-")]
    
    if (is.null(aristas_previas)) {
      # Período base (Menem), sin punto de comparación
      lista_metricas[[per$nombre]] <- data.table(
        Periodo = per$nombre, Nacimientos = NA_integer_, Muertes = NA_integer_
      )
    } else {
      ids_previos <- aristas_previas$id_rel
      ids_actuales <- aristas$id_rel
      
      # Lógica estricta de conjuntos: 0 a X y de X a 0
      ids_nacimientos <- setdiff(ids_actuales, ids_previos)
      ids_muertes     <- setdiff(ids_previos, ids_actuales)
      
      lista_metricas[[per$nombre]] <- data.table(
        Periodo = per$nombre, 
        Nacimientos = length(ids_nacimientos), 
        Muertes = length(ids_muertes)
      )
      
      # Extraer los datos de los nacimientos y retener su peso ACTUAL para medir representatividad
      if (length(ids_nacimientos) > 0) {
        nuevas <- aristas[id_rel %in% ids_nacimientos]
        nuevas[, Periodo := per$nombre]
        lista_nuevas_absolutas[[per$nombre]] <- nuevas
      }
      
      # Extraer los datos de las muertes y retener el peso PASADO (cuánto movían antes de morir)
      if (length(ids_muertes) > 0) {
        muertas <- aristas_previas[id_rel %in% ids_muertes]
        muertas[, Periodo := per$nombre]
        lista_muertas_absolutas[[per$nombre]] <- muertas
      }
    }
    
    # El período actual pasa a ser el previo para la siguiente iteración
    aristas_previas <- aristas
    rm(dt, aristas); gc()
  }
  
  # ─── 1. GRÁFICO DE NACIMIENTOS Y MUERTES ABSOLUTAS ─────────────────────────
  df_metricas <- rbindlist(lista_metricas)[!is.na(Nacimientos)]
  df_melt <- data.table::melt(df_metricas, id.vars = "Periodo", 
                              measure.vars = c("Nacimientos", "Muertes"),
                              variable.name = "Tipo", value.name = "Cantidad")
  
  # Invertir muertes para barra divergente
  df_melt[Tipo == "Muertes", Cantidad := -Cantidad]
  df_melt[, Periodo := factor(Periodo, levels = unique(df_metricas$Periodo))]
  
  p <- ggplot(df_melt, aes(x = Periodo, y = Cantidad, fill = Tipo)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.85, width = 0.6) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    scale_fill_manual(values = c("Nacimientos" = "#2D9596", "Muertes" = "#E74C3C")) +
    scale_y_continuous(labels = abs) +
    labs(
      title = "Dinámica Absoluta: Nacimientos y Muertes de Relaciones",
      subtitle = "Aparición (de 0 a X) y desaparición (de X a 0) de vínculos sectoriales respecto al período previo",
      x = NULL, y = "Cantidad de vínculos únicos"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", color = "#08306B", hjust = 0.5),
      plot.subtitle    = element_text(color = "#4A5568", hjust = 0.5, margin = margin(b = 15)),
      axis.text.x      = element_text(angle = 15, hjust = 1, face = "bold"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(dir_out, "00_Grafico_Nacimientos_Muertes_Absolutas.png"), 
         plot = p, width = 12, height = 7, dpi = 150, bg = "white")
  
  # ─── 2. EXCEL CON DETALLE ORDENADO POR REPRESENTATIVIDAD ───────────────────
  agregar_descripciones <- function(df) {
    if(nrow(df) == 0) return(df)
    df <- merge(df, label_r34, by.x = "origen", by.y = "r34", all.x = TRUE)
    setnames(df, "r34_desc", "Origen_Descripcion")
    df <- merge(df, label_r34, by.x = "destino", by.y = "r34", all.x = TRUE)
    setnames(df, "r34_desc", "Destino_Descripcion")
    setcolorder(df, c("Periodo", "origen", "Origen_Descripcion", 
                      "destino", "Destino_Descripcion", "peso"))
    df[, id_rel := NULL]
    # Ordenar estrictamente por Período y luego por el peso de la relación (de mayor a menor)
    return(df[order(Periodo, -peso)])
  }
  
  df_nuevas <- agregar_descripciones(rbindlist(lista_nuevas_absolutas))
  df_muertas <- agregar_descripciones(rbindlist(lista_muertas_absolutas))
  
  fname <- file.path(dir_out, "00_Detalle_Nacimientos_Muertes_Absolutas.xlsx")
  wb <- createWorkbook()
  
  addWorksheet(wb, "Nacimientos (0 a X)")
  writeDataTable(wb, "Nacimientos (0 a X)", df_nuevas, tableStyle = "TableStyleMedium2")
  setColWidths(wb, "Nacimientos (0 a X)", cols = 1:6, widths = c(20, 10, 45, 10, 45, 12))
  
  addWorksheet(wb, "Muertes (X a 0)")
  writeDataTable(wb, "Muertes (X a 0)", df_muertas, tableStyle = "TableStyleMedium3")
  setColWidths(wb, "Muertes (X a 0)", cols = 1:6, widths = c(20, 10, 45, 10, 45, 12))
  
  saveWorkbook(wb, fname, overwrite = TRUE)
  
  message("  ✓ Análisis absoluto completado. Excel y Gráfico generados.")
  return(invisible(list(grafico = p, nuevas = df_nuevas, muertas = df_muertas)))
}

relaciones_creadas_destruidas(ds)


# Métricas de redes -------------------------------------------------------

calcular_metricas <- function(g) {
  N     <- vcount(g)
  L     <- ecount(g)
  L_max <- N * (N - 1L)

  # Grado
  k_in  <- degree(g, mode = "in")
  k_out <- degree(g, mode = "out")
  k_tot <- k_in + k_out

  # Clustering
  C_local  <- transitivity(g, type = "local",  isolates = "zero")
  C_medio  <- round(mean(C_local, na.rm = TRUE), 6)
  C_global <- round(transitivity(g, type = "global"), 6)

  # Centralidades
  close_c   <- closeness(g,    mode = "all",  normalized = TRUE)
  between_c <- betweenness(g,  directed = TRUE, normalized = TRUE)

  # Tabla de métricas globales
  metricas_glob <- data.table(
    Metrica = c(
      "Nodos (N)", "Aristas (L)", "Aristas máximas (L_max)",
      "Densidad (d)", "Grado promedio <k>",
      "Clustering medio <C>", "Clustering global (transitividad)"
    ),
    Valor = c(
      N, L, L_max,
      round(L / L_max, 6),
      round(L / N, 4),
      C_medio, C_global
    )
  )

  # Tabla de métricas por nodo
  metricas_nodos <- data.table(
    Sector_r34  = as.integer(V(g)$name),
    Grado_in    = k_in,
    Grado_out   = k_out,
    Grado_total = k_tot,
    Closeness   = round(close_c,   6),
    Betweenness = round(between_c, 6),
    Clustering  = round(C_local,   6)
  )
  metricas_nodos <- merge(metricas_nodos, label_r34,
                          by.x = "Sector_r34", by.y = "r34", all.x = TRUE)
  setcolorder(metricas_nodos, c("Sector_r34", "r34_desc"))
  setorder(metricas_nodos, -Grado_total)

  list(
    globales = metricas_glob,
    nodos    = metricas_nodos,
    k_in     = k_in,
    k_out    = k_out,
    k_tot    = k_tot
  )
}

# VISUALIZACIÓN -----------------------------------------------------------

## Red ---------------------------------------------------------------------

# Visualizacion 1: con leyendas
visualizar_red <- function(g, nombre, num, dir_out = DIR_SALIDA,
                           max_trab = NULL, max_peso = NULL) {
  N <- vcount(g)
  if (N == 0) { message("  ⚠ Grafo vacío, se omite la red."); return(invisible(NULL)) }

  titulo <- TITULOS_PERIODO[as.character(num)]

  # Si no se proveen máximos globales, usar los del período
  if (is.null(max_trab)) max_trab <- max(V(g)$T_promedio, na.rm = TRUE)
  if (is.null(max_peso)) max_peso <- max(E(g)$weight,     na.rm = TRUE)

  # Estandarizar tamaño de nodo [4, 20] para mayor visibilidad
  T_vals <- pmax(replace(V(g)$T_promedio, is.na(V(g)$T_promedio), 1), 1)
  V(g)$sz <- 4 + 16 * (T_vals / max_trab)

  # Estandarizar peso de arista [0.2, 4]
  E(g)$ew <- 0.2 + 3.8 * (E(g)$weight / max_peso)

  set.seed(42)
  lay <- layout_with_fr(g, niter = 1200, weights = log1p(E(g)$weight))

  tg <- as_tbl_graph(g) |>
    activate(nodes) |>
    mutate(Macro_Sector = ifelse(is.na(letra_desc), "Sin definir", letra_desc)) |>
    activate(edges) |>
    mutate(ea = rescale(log1p(weight), to = c(0.1, 0.7)))

  p <- ggraph(tg, layout = lay) +
    geom_edge_arc(
      aes(width = ew, alpha = ea),
      arrow    = arrow(length = unit(2, "mm"), type = "closed"),
      end_cap  = circle(3, "mm"),
      color    = "#5D6D7E",
      strength = 0.15
    ) +
    geom_node_point(aes(size = sz, color = Macro_Sector), alpha = 0.90) +
    scale_color_manual(values = DISTINCT_COLORS,
                       name  = "Macro-Sector (Letra)",
                       drop  = FALSE) +
    scale_size_identity() +
    scale_edge_width(range = c(0.2, 4), guide = "none") +
    scale_edge_alpha(range = c(0.1, 0.7), guide = "none") +
    labs(
      title    = titulo
    ) +
    theme_void(base_size = 14) + # Base size incrementado
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", color = NA),
      plot.title       = element_text(size = 60, face = "bold", color = "#08306B",
                                      hjust = 0.5, margin = margin(t = 10, b = 8)),
      plot.subtitle    = element_text(size = 14,  color = "#4A5568",
                                      hjust = 0.5, margin = margin(b = 10)),
      plot.caption     = element_text(size = 10,  color = "#718096", hjust = 0.5),
      legend.position  = "right",
      legend.title     = element_text(size = 25,  face = "bold", color = "#08306B"),
      legend.text      = element_text(size = 20, color = "#2D3748"),
      legend.key.size  = unit(1.2, "cm"), # Leyenda más holgada
      plot.margin      = margin(2, 2, 2, 2) # Márgenes reducidos al mínimo para acercar la red
    )

  ggsave(file.path(dir_out, sprintf("%02d_Red_ppt1_%s.png", num, nombre)),
         plot = p, width = 22, height = 15, dpi = 150, bg = "white")
  message(sprintf("  ✓ Red guardada: período %d.", num))
  invisible(p)
}

# Visualizacion 2: sin leyendas
visualizar_red_ppt <- function(g, nombre, num, dir_out = DIR_SALIDA,
                           max_trab = NULL, max_peso = NULL) {
  N <- vcount(g)
  if (N == 0) { message("  ⚠ Grafo vacío, se omite la red."); return(invisible(NULL)) }

  titulo <- TITULOS_PERIODO[as.character(num)]

  # Si no se proveen máximos globales, usar los del período
  if (is.null(max_trab)) max_trab <- max(V(g)$T_promedio, na.rm = TRUE)
  if (is.null(max_peso)) max_peso <- max(E(g)$weight,     na.rm = TRUE)

  # Estandarizar tamaño de nodo [4, 20] para mayor visibilidad
  T_vals <- pmax(replace(V(g)$T_promedio, is.na(V(g)$T_promedio), 1), 1)
  V(g)$sz <- 4 + 16 * (T_vals / max_trab)

  # Estandarizar peso de arista [0.2, 4]
  E(g)$ew <- 0.2 + 3.8 * (E(g)$weight / max_peso)

  set.seed(42)
  lay <- layout_with_fr(g, niter = 1200, weights = log1p(E(g)$weight))

  tg <- as_tbl_graph(g) |>
    activate(nodes) |>
    mutate(Macro_Sector = ifelse(is.na(letra_desc), "Sin definir", letra_desc)) |>
    activate(edges) |>
    mutate(ea = rescale(log1p(weight), to = c(0.1, 0.7)))

  p <- ggraph(tg, layout = lay) +
    geom_edge_arc(
      aes(width = ew, alpha = ea),
      arrow    = arrow(length = unit(2, "mm"), type = "closed"),
      end_cap  = circle(3, "mm"),
      color    = "#5D6D7E",
      strength = 0.15
    ) +
    geom_node_point(aes(size = sz, color = Macro_Sector), alpha = 0.90) +
    scale_color_manual(values = DISTINCT_COLORS,
                       name  = "Macro-Sector (Letra)",
                       drop  = FALSE) +
    scale_size_identity() +
    scale_edge_width(range = c(0.2, 4), guide = "none") +
    scale_edge_alpha(range = c(0.1, 0.7), guide = "none") +
    labs(
      title    = titulo
    ) +
    theme_void(base_size = 14) + # Base size incrementado
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", color = NA),
      plot.title       = element_text(size = 60, face = "bold", color = "#08306B",
                                      hjust = 0.5, margin = margin(t = 10, b = 8)),
      legend.position  = "none",
      plot.margin      = margin(2, 2, 2, 2) # Márgenes reducidos al mínimo para acercar la red
    )

  ggsave(file.path(dir_out, sprintf("%02d_Red__ppt2_%s.png", num, nombre)),
         plot = p, width = 22, height = 15, dpi = 150, bg = "white")
  message(sprintf("  ✓ Red guardada: período %d.", num))
  invisible(p)
}


# Heat map ----------------------------------------------------------------

graficar_heatmap <- function(mat, nombre, num, dir_out = DIR_SALIDA) {

  m <- mat$ponderada
  nodos_ids <- as.integer(rownames(m))

  info_nodos <- data.table(r34 = nodos_ids)

  # 1. Asignar letra a cada nodo de forma vectorizada (evita el error de lista)
  info_nodos[, Letra := fcase(
    r34 < 200,  "A",
    r34 < 600,  "B",
    r34 < 2000, "C",
    r34 < 4000, "D",
    r34 < 4200, "E",
    r34 < 5000, "F",
    r34 < 5600, "G",
    r34 < 5700, "H",
    r34 < 6500, "I",
    r34 < 7000, "J",
    r34 < 8000, "K",
    r34 < 8100, "M",
    r34 < 8600, "N",
    default =   "O"
  )]
  
  setorder(info_nodos, Letra, r34)
  
  orden_r34 <- as.character(info_nodos$r34)

  # 2. Transformar matriz a formato largo
  mat_ord <- log1p(m[orden_r34, orden_r34])
  df_long <- reshape2::melt(mat_ord, varnames = c("Origen", "Destino"), value.name = "log1p_peso")

  # 3. Unir la letra para Origen y Destino
  df_long <- merge(df_long, info_nodos, by.x = "Origen", by.y = "r34", all.x = TRUE)
  setnames(df_long, "Letra", "Origen_Letra")
  
  df_long <- merge(df_long, info_nodos, by.x = "Destino", by.y = "r34", all.x = TRUE)
  setnames(df_long, "Letra", "Destino_Letra")

  # Convertir a factor para respetar el orden interno
  df_long$Origen  <- factor(df_long$Origen,  levels = rev(orden_r34))
  df_long$Destino <- factor(df_long$Destino, levels = orden_r34)

  # 4. Graficar usando facet_grid
  p <- ggplot(df_long, aes(x = Destino, y = Origen, fill = log1p_peso)) +
    geom_tile(color = "gray90", linewidth = 0.1) +
    facet_grid(Origen_Letra ~ Destino_Letra, scales = "free", space = "free", switch = "y") +
    scale_fill_gradientn(
      colors = c("#FFFFFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
      name   = "log(1+peso)"
    ) +
    labs(
      title = paste0("Heatmap de transiciones intersectoriales — ", TITULOS_PERIODO[as.character(num)]),
      x = "Destino", y = "Origen"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x       = element_text(angle = 90, hjust = 1, size = 5),
      axis.text.y       = element_text(size = 5),
      strip.placement   = "outside",
      strip.background  = element_rect(fill = "#E2E8F0", color = "gray70"),
      strip.text.x      = element_text(face = "bold", size = 8),
      strip.text.y.left = element_text(face = "bold", size = 8, angle = 0),
      plot.title        = element_text(face = "bold", color = "#08306B", hjust = 0.5, size = 14),
      panel.spacing     = unit(0, "lines"), 
      panel.border      = element_rect(color = "gray60", fill = NA, linewidth = 0.5),
      panel.grid        = element_blank()
    )

  fname <- file.path(dir_out, sprintf("%02d_Heatmap_%s.png", num, nombre))
  ggsave(fname, plot = p, width = 22, height = 20, dpi = 150, bg = "white")
  message("  ✓ Heatmap unificado con facetas guardado.")
  invisible(p)
}

graficar_heatmap_top80 <- function(mat, aristas, nombre, num, dir_out = DIR_SALIDA) {
  flujo_out <- aristas[, .(flujo_o = sum(peso)), by = .(r34 = origen)]
  flujo_in  <- aristas[, .(flujo_i = sum(peso)), by = .(r34 = destino)]
  flujo_nodos <- merge(flujo_out, flujo_in, by = "r34", all = TRUE)
  flujo_nodos[is.na(flujo_o), flujo_o := 0]
  flujo_nodos[is.na(flujo_i), flujo_i := 0]
  flujo_nodos[, flujo_total := flujo_o + flujo_i]
  
  setorder(flujo_nodos, -flujo_total)
  volumen_sistema <- sum(flujo_nodos$flujo_total)
  flujo_nodos[, pct_acum := cumsum(flujo_total) / volumen_sistema]
  nodos_80 <- flujo_nodos[pct_acum <= 0.80 | shift(pct_acum, fill = 0) < 0.80, r34]
  
  m <- mat$ponderada
  nodos_validos <- as.character(intersect(nodos_80, as.integer(rownames(m))))
  if(length(nodos_validos) == 0) {
    message("  ⚠ No hay nodos para graficar en el top 80%.")
    return(invisible(NULL))
  }
  
  m_sub <- m[nodos_validos, nodos_validos, drop = FALSE]
  info_nodos <- data.table(r34 = as.integer(nodos_validos))
  info_nodos[, Letra := fcase(
    r34 < 200, "A", r34 < 600, "B", r34 < 2000, "C", r34 < 4000, "D",
    r34 < 4200, "E", r34 < 5000, "F", r34 < 5600, "G", r34 < 5700, "H",
    r34 < 6500, "I", r34 < 7000, "J", r34 < 8000, "K", r34 < 8100, "M",
    r34 < 8600, "N", default = "O"
  )]
  setorder(info_nodos, Letra, r34)
  orden_r34 <- as.character(info_nodos$r34)
  
  mat_ord <- log1p(m_sub[orden_r34, orden_r34, drop = FALSE])
  df_long <- reshape2::melt(mat_ord, varnames = c("Origen", "Destino"), value.name = "log1p_peso")
  df_long <- merge(df_long, info_nodos, by.x = "Origen", by.y = "r34", all.x = TRUE)
  setnames(df_long, "Letra", "Origen_Letra")
  df_long <- merge(df_long, info_nodos, by.x = "Destino", by.y = "r34", all.x = TRUE)
  setnames(df_long, "Letra", "Destino_Letra")
  
  df_long$Origen  <- factor(df_long$Origen,  levels = rev(orden_r34))
  df_long$Destino <- factor(df_long$Destino, levels = orden_r34)
  
  p <- ggplot(df_long, aes(x = Destino, y = Origen, fill = log1p_peso)) +
    geom_tile(color = "gray90", linewidth = 0.1) +
    facet_grid(Origen_Letra ~ Destino_Letra, scales = "free", space = "free", switch = "y") +
    scale_fill_gradientn(colors = c("#FFFFFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"), name = "log(1+peso)") +
    labs(
      title = paste0("Heatmap Concentrado (Top 80% Movilidad) — ", TITULOS_PERIODO[as.character(num)]),
      subtitle = sprintf("Muestra solo los %d sectores que concentran el 80%% del flujo.", length(nodos_validos)),
      x = "Destino", y = "Origen"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      strip.placement = "outside",
      strip.background = element_rect(fill = "#E2E8F0", color = "gray70"),
      strip.text.x = element_text(face = "bold", size = 9),
      strip.text.y.left = element_text(face = "bold", size = 9, angle = 0),
      plot.title = element_text(face = "bold", color = "#08306B", hjust = 0.5, size = 16),
      panel.spacing = unit(0, "lines"), panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.5),
      panel.grid = element_blank()
    )
  
  ggsave(file.path(dir_out, sprintf("%02d_Heatmap_Top80_%s.png", num, nombre)), plot = p, width = 16, height = 14, dpi = 150, bg = "white")
  message(sprintf("  ✓ Heatmap concentrado guardado (%d sectores).", length(nodos_validos)))
  invisible(p)
}


# Grafico principal ---------------------------------------------------------------

grafico_principal <- function(ds, dir_out = DIR_SALIDA) {
  
  message("  Calculando métricas unificadas...")
  
  gini_pesos <- function(x) {
    x <- as.numeric(sort(x[x > 0]))
    n <- as.numeric(length(x))
    if (n == 0) return(NA_real_)
    pesos_ponderados <- as.numeric(seq_len(n)) * x
    round((2 * sum(pesos_ponderados) / (n * sum(x))) - (n + 1) / n, 6)
  }
  
  lista_datos <- list()
  
  for (per in PERIODOS) {
    dt_p <- cargar_periodo(ds, per$inicio, per$fin)
    if (nrow(dt_p) == 0) next
    
    dt_p <- resolver_pluriempleo(dt_p)
    aris <- construir_transiciones(dt_p)
    
    if (nrow(aris) == 0) next
    
    trab <- dt_p[, uniqueN(id_trabajador)]
    trans <- sum(aris$peso)
    
    nodos_activos <- length(unique(c(aris$origen, aris$destino)))
    aristas_reales <- nrow(aris)
    dens <- ifelse(nodos_activos > 1, aristas_reales / (nodos_activos * (nodos_activos - 1)), 0)
    
    concentracion <- gini_pesos(aris$peso)
    
    lista_datos[[per$nombre]] <- data.table(
      periodo = per$nombre,
      trabajadores = trab,
      transiciones = trans,
      densidad = dens,
      gini = concentracion
    )
  }
  
  df_plot <- rbindlist(lista_datos)
  
  if (nrow(df_plot) == 0) {
    stop("ERROR: La tabla de datos está vacía. Revisá los filtros de fecha en PERIODOS o la carga del dataset.")
  }
  
  df_plot[, periodo := factor(periodo, levels = sapply(PERIODOS, `[[`, "nombre"))]
  
  k <- max(df_plot$trabajadores) / max(df_plot$gini)
  
  p <- ggplot(df_plot, aes(x = periodo)) +
    
    # Eje Secundario: Gini
    geom_line(aes(y = gini * k, color = "Gini (Concentración)", group = 1), linewidth = 1.2) +
    geom_point(aes(y = gini * k, color = "Gini (Concentración)"), size = 3.5) +
    geom_text(aes(y = gini * k, label = round(gini, 2)), vjust = -1.5, size = 3.5, color = "#C0392B", fontface = "bold") +
    
    # Eje Secundario: Densidad
    geom_line(aes(y = densidad * k, color = "Densidad", group = 1), linewidth = 1.2) +
    geom_point(aes(y = densidad * k, color = "Densidad"), size = 3.5) +
    geom_text(aes(y = densidad * k, label = round(densidad, 3)), vjust = 2, size = 3.5, color = "#8E44AD", fontface = "bold") +
    
    # Eje Principal: Trabajadores
    geom_line(aes(y = trabajadores, color = "Trabajadores Únicos", group = 1), linewidth = 1.2) +
    geom_point(aes(y = trabajadores, color = "Trabajadores Únicos"), size = 3.5) +
    geom_text(aes(y = trabajadores, label = scales::comma(trabajadores)), vjust = 2, size = 3.5, color = "#08306B", fontface = "bold") +
    
    # Eje Principal: Transiciones
    geom_line(aes(y = transiciones, color = "Total Transiciones", group = 1), linewidth = 1.2) +
    geom_point(aes(y = transiciones, color = "Total Transiciones"), size = 3.5) +
    geom_text(aes(y = transiciones, label = scales::comma(transiciones)), vjust = -2, size = 3.5, color = "#E67E22", fontface = "bold") +
    
    scale_y_continuous(
      name = "Volumen Absoluto",
      labels = scales::label_comma(),
      limits = c(0, NA),
      sec.axis = sec_axis(~ . / k, name = "Índices (Densidad y Gini)", labels = scales::label_number()),
      expand = expansion(mult = c(0.1, 0.2)) # Expandido arriba para que las etiquetas no toquen el margen
    ) +
    scale_color_manual(
      values = c(
        "Densidad"  = "#8E44AD",
        "Total Transiciones"   = "#E67E22",
        "Trabajadores Únicos"  = "#08306B",
        "Gini (Concentración)" = "#C0392B"
      ),
      # Forzar el orden estricto de la leyenda
      breaks = c("Densidad", "Total Transiciones", "Trabajadores Únicos", "Gini (Concentración)"),
      name = NULL
    ) +
    labs(
      title = "Densidad, transiciones, volumen promedio de trabajadores e índice de Gini",
      subtitle = "Sector privado registrado — Argentina 1996–2019",
      x = "Período presidencial",
      caption = "Fuente: Elaboración propia en base a datos MLER."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", hjust = 0.5),
      plot.subtitle      = element_text(hjust = 0.5, color = "#4A5568", margin = margin(b = 15)),
      legend.position    = "bottom",
      axis.text.x        = element_text(angle = 15, hjust = 1, face = "bold"),
      panel.grid.minor   = element_blank()
    )
  
  fname <- file.path(dir_out, "00_Metricas_Unificadas.png")
  ggsave(fname, plot = p, width = 14, height = 8, dpi = 150, bg = "white")
  
  message("  ✓ Gráfico unificado guardado correctamente en: ", fname)
  
  invisible(p)
}

grafico_principal(ds)


# Sectores -------------------------------------------------------------

# Atractores/expulsores y evolucion empleo
graficar_dinamica_volumen <- function(ds, dir_out = DIR_SALIDA) {
  message("Calculando flujos y frecuencias para todos los períodos...")
  
  # 1. Recolección y cálculo transversal
  lista_datos <- lapply(PERIODOS, function(per) {
    dt_p <- cargar_periodo(ds, per$inicio, per$fin)
    if(nrow(dt_p) == 0) return(NULL)
    dt_p <- resolver_pluriempleo(dt_p)
    
    aris <- construir_transiciones(dt_p)
    
    # Frecuencia (Trabajadores únicos)
    frec <- dt_p[, .(frecuencia = uniqueN(id_trabajador)), by = r34]
    
    # Entradas y Salidas absolutas
    if(nrow(aris) > 0) {
      in_flow <- aris[, .(entradas = sum(peso)), by = .(r34 = destino)]
      out_flow <- aris[, .(salidas = sum(peso)), by = .(r34 = origen)]
    } else {
      in_flow <- data.table(r34=integer(), entradas=numeric())
      out_flow <- data.table(r34=integer(), salidas=numeric())
    }
    
    res <- merge(frec, in_flow, by="r34", all.x=TRUE)
    res <- merge(res, out_flow, by="r34", all.x=TRUE)
    res[is.na(entradas), entradas := 0]
    res[is.na(salidas), salidas := 0]
    
    # Letra del macro-sector para color
    map_letra <- dt_p[, .(letra = as.integer(names(which.max(table(letra))))), by = r34]
    res <- merge(res, map_letra, by="r34", all.x=TRUE)
    
    res[, periodo := per$nombre]
    res[, num_periodo := per$num]
    return(res)
  })
  
  df_total <- rbindlist(lista_datos)
  
  # Cruce con diccionarios
  df_total <- merge(df_total, label_r34, by="r34", all.x=TRUE)
  df_total <- merge(df_total, label_letra, by="letra", all.x=TRUE)
  df_total[is.na(r34_desc), r34_desc := paste0("Sector ", r34)]
  df_total[is.na(letra_desc), letra_desc := "Sin definir"]
  
  # Nombres limpios para los facetados
  niveles_limpios <- gsub("_", " ", sapply(PERIODOS, `[[`, "nombre"))
  df_total[, periodo_clean := factor(gsub("_", " ", as.character(periodo)), levels = niveles_limpios)]
  
  # GRÁFICO 1: Atracción vs Expulsión

  # Identificamos solo a los sectores con mayor dinámica total para no saturar de texto
  df_total[, label_plot1 := NA_character_]
  for(p in unique(df_total$periodo_clean)) {
    top_nodos <- df_total[periodo_clean == p][order(-(entradas + salidas))][1:8, r34]
    df_total[periodo_clean == p & r34 %in% top_nodos, label_plot1 := r34_desc]
  }
  
  # Forzar ejes X e Y simétricos
  max_flujo <- max(c(df_total$entradas, df_total$salidas), na.rm = TRUE)
  
  p1 <- ggplot(df_total, aes(x = salidas, y = entradas)) +
    # Línea de equilibrio neto nulo
    geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed", linewidth = 0.8) +
    geom_point(aes(size = frecuencia, color = letra_desc), alpha = 0.75) +
    ggrepel::geom_text_repel(
      aes(label = label_plot1), size = 3.2, fontface = "bold", 
      color = "#2D3748", box.padding = 0.6, max.overlaps = 20, segment.color = "gray70"
    ) +
    scale_color_manual(values = DISTINCT_COLORS, guide = "none") +
    scale_size_continuous(range = c(2, 18), labels = scales::label_comma(), name = "Trabajadores\nÚnicos") +
    scale_x_continuous(labels = scales::label_comma(), limits = c(0, max_flujo)) +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0, max_flujo)) +
    facet_wrap(~ periodo_clean, ncol = 3) +
    labs(
      title = "Atracción vs. Expulsión de Trabajadores",
      subtitle = "Diagonal: equilibrio neto. Por encima: atractores (absorben más de lo que expulsan). Por debajo: expulsores.",
      x = "Volumen de Salidas (Expulsión)",
      y = "Volumen de Entradas (Atracción)",
      caption = "Fuente: Elaboración propia en base a datos MLER."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 25, color = "#08306B"),
      plot.subtitle = element_text(size = 15, color = "#4A5568", margin = margin(b = 15)),
      strip.text = element_text(face = "bold", size = 11, color = "white"),
      strip.background = element_rect(fill = "#4A5568", color = NA),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
  
  ggsave(file.path(dir_out, "00_Matriz_Flujos.png"), plot = p1, width = 18, height = 11, dpi = 150, bg = "white")
  
  # GRÁFICO 2: Evolución Top Sectores con mayor nivel de empleo

  # Filtramos a los 10 sectores que más volumen movieron en la suma de toda la historia
  top_historico <- df_total[, .(vol_total = sum(frecuencia)), by = r34_desc][order(-vol_total)][1:10, r34_desc]
  df_frec <- df_total[r34_desc %in% top_historico]
  
  p2 <- ggplot(df_frec, aes(x = periodo_clean, y = frecuencia, group = r34_desc, color = r34_desc)) +
    geom_line(linewidth = 1.2, alpha = 0.85) +
    geom_point(size = 3.5) +
    ggrepel::geom_text_repel(
      data = df_frec[num_periodo == max(num_periodo)],
      aes(label = r34_desc),
      nudge_x = 0.5, hjust = 0, size = 3.5, fontface = "bold", direction = "y", segment.color = NA
    ) +
    scale_color_manual(values = setNames(colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(10), top_historico), guide = "none") +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_x_discrete(expand = expansion(mult = c(0.05, 0.45))) +
    labs(
      title = "Evolución del tamaño del sector: Los 10 gigantes del mercado",
      subtitle = "Cantidad de trabajadores únicos registrados por período.",
      x = NULL, y = "Volumen de Trabajadores Únicos",
      caption = "Fuente: Elaboración propia en base a datos MLER."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 25, color = "#08306B"),
      plot.subtitle = element_text(size = 15, color = "#4A5568", margin = margin(b = 15)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#E2E8F0", linetype = "dashed"),
      axis.text.x = element_text(face = "bold", color = "#2D3748", size = 11)
    )
  
  ggsave(file.path(dir_out, "00_Evolucion_Frecuencia.png"), plot = p2, width = 16, height = 8, dpi = 150, bg = "white")
  
  message("  ✓ Gráficos de Volumen y Flujo exportados con éxito.")
  invisible(list(p1 = p1, p2 = p2))
}

graficar_dinamica_volumen(ds)

graficar_atraccion_expulsion <- function(ds, dir_out = DIR_SALIDA) {
  message("Calculando flujos y tamaño promedio para matriz...")
  
  lista_datos <- lapply(PERIODOS, function(per) {
    dt_p <- cargar_periodo(ds, per$inicio, per$fin)
    if(nrow(dt_p) == 0) return(NULL)
    dt_p <- resolver_pluriempleo(dt_p)
    
    aris <- construir_transiciones(dt_p)
    
    # REEMPLAZO: Cálculo del tamaño promedio (T_promedio) por período
    n_meses <- dt_p[, uniqueN(tiempo)]
    tam <- dt_p[, .(n = .N), by = .(r34, tiempo)][, .(T_promedio = sum(n) / n_meses), by = r34]
    
    if(nrow(aris) > 0) {
      in_flow <- aris[, .(entradas = sum(peso)), by = .(r34 = destino)]
      out_flow <- aris[, .(salidas = sum(peso)), by = .(r34 = origen)]
    } else {
      in_flow <- data.table(r34=integer(), entradas=numeric())
      out_flow <- data.table(r34=integer(), salidas=numeric())
    }
    
    res <- merge(tam, in_flow, by="r34", all.x=TRUE)
    res <- merge(res, out_flow, by="r34", all.x=TRUE)
    res[is.na(entradas), entradas := 0]
    res[is.na(salidas), salidas := 0]
    
    map_letra <- dt_p[, .(letra = as.integer(names(which.max(table(letra))))), by = r34]
    res <- merge(res, map_letra, by="r34", all.x=TRUE)
    
    res[, periodo := per$nombre]
    res[, num_periodo := per$num]
    return(res)
  })
  
  df_total <- rbindlist(lista_datos)
  df_total <- merge(df_total, label_r34, by="r34", all.x=TRUE)
  df_total <- merge(df_total, label_letra, by="letra", all.x=TRUE)
  df_total[is.na(r34_desc), r34_desc := paste0("Sector ", r34)]
  df_total[is.na(letra_desc), letra_desc := "Sin definir"]
  
  niveles_limpios <- gsub("_", " ", sapply(PERIODOS, `[[`, "nombre"))
  df_total[, periodo_clean := factor(gsub("_", " ", as.character(periodo)), levels = niveles_limpios)]
  
  df_total[, label_plot1 := NA_character_]
  for(p in unique(df_total$periodo_clean)) {
    top_nodos <- df_total[periodo_clean == p][order(-(entradas + salidas))][1:8, r34]
    df_total[periodo_clean == p & r34 %in% top_nodos, label_plot1 := r34_desc]
  }
  
  max_flujo <- max(c(df_total$entradas, df_total$salidas), na.rm = TRUE)
  
  p1 <- ggplot(df_total, aes(x = salidas, y = entradas)) +
    geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed", linewidth = 0.8) +
    # Mapeo actualizado al Tamaño Promedio
    geom_point(aes(size = T_promedio, color = letra_desc), alpha = 0.75) +
    ggrepel::geom_text_repel(
      aes(label = label_plot1), size = 3.2, fontface = "bold", 
      color = "#2D3748", box.padding = 0.6, max.overlaps = 20, segment.color = "gray70"
    ) +
    scale_color_manual(values = DISTINCT_COLORS, guide = "none") +
    # Escala de leyenda actualizada
    scale_size_continuous(range = c(2, 18), labels = scales::label_comma(), name = "Trabajadores\nPromedio") +
    scale_x_continuous(labels = scales::label_comma(), limits = c(0, max_flujo)) +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0, max_flujo)) +
    facet_wrap(~ periodo_clean, ncol = 3) +
    labs(
      title = "Atracción vs. Expulsión de Trabajadores",
      subtitle = "Diagonal: equilibrio neto. El tamaño de la burbuja refleja el promedio mensual de trabajadores.",
      x = "Volumen de Salidas (Expulsión)",
      y = "Volumen de Entradas (Atracción)",
      caption = "Fuente: Elaboración propia en base a datos MLER."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 25, color = "#08306B"),
      plot.subtitle = element_text(size = 15, color = "#4A5568", margin = margin(b = 15)),
      strip.text = element_text(face = "bold", size = 11, color = "white"),
      strip.background = element_rect(fill = "#4A5568", color = NA),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
  
  ggsave(file.path(dir_out, "00_Matriz_Flujos_Promedio.png"), plot = p1, width = 18, height = 11, dpi = 150, bg = "white")
  message("  ✓ Gráfico de Matriz exportado con éxito.")
  invisible(p1)
}

graficar_evolucion_tamano <- function(ds, dir_out = DIR_SALIDA) {
  message("Calculando tamaño promedio histórico para gráfico de evolución (Top 10 + Representantes de Industria, Comercio y Transporte)...")
  
  lista_datos <- lapply(PERIODOS, function(per) {
    dt_p <- cargar_periodo(ds, per$inicio, per$fin)
    if(nrow(dt_p) == 0) return(NULL)
    dt_p <- resolver_pluriempleo(dt_p)
    
    n_meses <- dt_p[, uniqueN(tiempo)]
    tam <- dt_p[, .(n = .N), by = .(r34, tiempo)][, .(T_promedio = sum(n) / n_meses), by = r34]
    
    map_letra <- dt_p[, .(letra = as.integer(names(which.max(table(letra))))), by = r34]
    tam <- merge(tam, map_letra, by="r34", all.x=TRUE)
    
    tam[, periodo := per$nombre]
    tam[, num_periodo := per$num]
    return(tam)
  })
  
  df_total <- rbindlist(lista_datos)
  df_total <- merge(df_total, label_r34, by="r34", all.x=TRUE)
  df_total <- merge(df_total, label_letra, by="letra", all.x=TRUE)
  
  df_total[is.na(r34_desc), r34_desc := paste0("Sector ", r34)]
  
  niveles_limpios <- gsub("_", " ", sapply(PERIODOS, `[[`, "nombre"))
  df_total[, periodo_clean := factor(gsub("_", " ", as.character(periodo)), levels = niveles_limpios)]
  
  # 1. Identificar el Top 10 global (usando la media de los promedios)
  top_10_global <- df_total[, .(vol_promedio = mean(T_promedio, na.rm = TRUE)), by = r34_desc][order(-vol_promedio)][1:10, r34_desc]
  
  # 2. Identificar el Top 1 específico de Industria, Comercio y Transporte (usando la media)
  top_representantes <- df_total[grepl("Industria|Comercio|Transporte", letra_desc), 
                                 .(vol_promedio = mean(T_promedio, na.rm = TRUE)), 
                                 by = .(r34_desc, letra_desc)][
                                   order(letra_desc, -vol_promedio)
                                 ][, .SD[1], by = letra_desc]$r34_desc
  
  # 3. Unir los sectores a graficar
  sectores_grafico <- unique(c(top_10_global, top_representantes))
  
  df_frec <- df_total[r34_desc %in% sectores_grafico]
  
  n_lineas <- length(sectores_grafico)
  paleta <- setNames(colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_lineas), sectores_grafico)
  
  p2 <- ggplot(df_frec, aes(x = periodo_clean, y = T_promedio, group = r34_desc, color = r34_desc)) +
    geom_line(linewidth = 1.2, alpha = 0.85) +
    geom_point(size = 3.5) +
    ggrepel::geom_text_repel(
      data = df_frec[num_periodo == max(num_periodo)],
      aes(label = r34_desc),
      nudge_x = 0.5, hjust = 0, size = 3.5, fontface = "bold", direction = "y", segment.color = NA, max.overlaps = Inf
    ) +
    scale_color_manual(values = paleta, guide = "none") +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_x_discrete(expand = expansion(mult = c(0.05, 0.60))) +
    labs(
      title = "Evolución del tamaño del sector: Los 10 gigantes + Representantes Clave",
      subtitle = "Cantidad de trabajadores promedio mensuales registrados por período.",
      x = NULL, y = "Volumen Promedio de Trabajadores",
      caption = "Fuente: Elaboración propia en base a datos MLER."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 22, color = "#08306B"),
      plot.subtitle = element_text(size = 14, color = "#4A5568", margin = margin(b = 15)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#E2E8F0", linetype = "dashed"),
      axis.text.x = element_text(face = "bold", color = "#2D3748", size = 11)
    )
  
  ggsave(file.path(dir_out, "00_Evolucion_Tamano_Promedio.png"), plot = p2, width = 18, height = 9, dpi = 150, bg = "white")
  message("  ✓ Gráfico de Evolución exportado con éxito.")
  invisible(p2)
}

graficar_atraccion_expulsion(ds)
graficar_evolucion_tamano(ds)

# EXPORTACIÓN EXCEL -------------------------------------------------------

exportar_excel <- function(caract, metricas, aristas, mat, nombre, num,
                           dir_out = DIR_SALIDA) {
  wb <- createWorkbook()

  # Estilos
  s_titulo <- createStyle(fontName = "Arial", fontSize = 13, fontColour = "#FFFFFF",
                          fgFill = "#08306B", halign = "center", textDecoration = "bold",
                          border = "Bottom", borderColour = "#2166AC")
  s_header <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
                          fgFill = "#2166AC", halign = "center", textDecoration = "bold",
                          border = "TopBottomLeftRight", borderColour = "#08306B",
                          wrapText = TRUE)
  s_celd   <- createStyle(fontName = "Arial", fontSize = 9,
                          border = "TopBottomLeftRight", borderColour = "#9ECAE1")
  s_zebra  <- createStyle(fontName = "Arial", fontSize = 9, fgFill = "#EEF5FB",
                          border = "TopBottomLeftRight", borderColour = "#9ECAE1")
  s_num    <- createStyle(fontName = "Arial", fontSize = 9, numFmt = "#,##0",
                          halign = "right",
                          border = "TopBottomLeftRight", borderColour = "#9ECAE1")
  s_dec    <- createStyle(fontName = "Arial", fontSize = 9, numFmt = "#,##0.000000",
                          halign = "right",
                          border = "TopBottomLeftRight", borderColour = "#9ECAE1")

  # Helper para agregar hojas
  add_hoja <- function(nombre_hoja, titulo_hoja, df, col_anchos = NULL) {
    addWorksheet(wb, nombre_hoja, gridLines = FALSE)
    nc <- ncol(df); nr <- nrow(df)
    writeData(wb, nombre_hoja, titulo_hoja, startRow = 1, startCol = 1)
    addStyle(wb, nombre_hoja, s_titulo, rows = 1, cols = 1:max(nc, 3), gridExpand = TRUE)
    mergeCells(wb, nombre_hoja, rows = 1, cols = 1:max(nc, 3))
    writeData(wb, nombre_hoja, df, startRow = 3, startCol = 1,
              headerStyle = s_header, borders = "all")
    addStyle(wb, nombre_hoja, s_celd,
             rows = 4:(3 + nr), cols = 1:nc, gridExpand = TRUE, stack = TRUE)
    if (nr > 1) {
      filas_par <- seq(4, 3 + nr, by = 2)
      filas_par <- filas_par[filas_par <= 3 + nr]
      if (length(filas_par))
        addStyle(wb, nombre_hoja, s_zebra,
                 rows = filas_par, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
    }
    for (j in seq_len(nc)) {
      if (is.numeric(df[[j]])) {
        est <- if (all(df[[j]] %% 1 == 0, na.rm = TRUE)) s_num else s_dec
        addStyle(wb, nombre_hoja, est, rows = 4:(3 + nr), cols = j,
                 gridExpand = TRUE, stack = TRUE)
      }
    }
    if (!is.null(col_anchos))
      setColWidths(wb, nombre_hoja, cols = seq_along(col_anchos), widths = col_anchos)
    else
      setColWidths(wb, nombre_hoja, cols = 1:nc, widths = "auto")
    freezePane(wb, nombre_hoja, firstActiveRow = 4)
  }

  # Resumen
  resumen <- data.table(
    Indicador = c("Período","Intervalo","Trabajadores únicos","Meses",
                  "Sectores r34 activos","Total transiciones"),
    Valor = c(
      gsub("_", " ", nombre),
      TITULOS_PERIODO[as.character(num)],
      format(caract$n_trabajadores, big.mark = ".", decimal.mark = ","),
      caract$n_meses,
      nrow(caract$tamano),
      format(sum(aristas$peso), big.mark = ".", decimal.mark = ",")
    )
  )
  add_hoja("01_Resumen", paste0("RESUMEN — ", TITULOS_PERIODO[as.character(num)]),
           resumen, col_anchos = c(35, 30))

  # Tamaño de nodos
  nodos_df <- copy(caract$tamano)[, .(
    Sector_r34 = r34, Nombre_Sector = r34_desc,
    Macro_Sector = letra_desc, Promedio_Trabaj_Mes = round(T_promedio, 1)
  )]
  add_hoja("02_Tamano_Nodos", "TAMAÑO DE NODOS — Promedio mensual",
           nodos_df, col_anchos = c(12, 50, 45, 22))

  # Frecuencia de Trabajadores Únicos
  add_hoja("04_Frecuencia_Unicos", "ACTIVIDADES CON MAYOR CANTIDAD DE TRABAJADORES ÚNICOS",
           caract$frec_sector, col_anchos = c(12, 50, 15))

  # Métricas globales
  add_hoja("05_Metricas_Red", "MÉTRICAS GLOBALES DE LA RED",
           metricas$globales, col_anchos = c(35, 15, 80))

  # Centralidades por nodo
  add_hoja("06_Centralidades", "CENTRALIDADES POR SECTOR",
           metricas$nodos, col_anchos = c(12, 50, 10, 10, 12, 14, 14))

  # Flujos de transición
  flujos_df <- copy(aristas)[order(-peso)]
  setnames(flujos_df, c("Sector_Origen_r34", "Sector_Destino_r34", "N_Trabajadores"))
  flujos_df <- merge(flujos_df, label_r34, by.x = "Sector_Origen_r34",  by.y = "r34", all.x = TRUE)
  setnames(flujos_df, "r34_desc", "Origen_Descripcion")
  flujos_df <- merge(flujos_df, label_r34, by.x = "Sector_Destino_r34", by.y = "r34", all.x = TRUE)
  setnames(flujos_df, "r34_desc", "Destino_Descripcion")
  setcolorder(flujos_df, c("Sector_Origen_r34", "Origen_Descripcion",
                            "Sector_Destino_r34", "Destino_Descripcion",
                            "N_Trabajadores"))
  add_hoja("07_Flujos_Transicion", "FLUJOS DE TRANSICIÓN INTERSECTORIAL",
           flujos_df, col_anchos = c(18, 45, 18, 45, 15))

  # Matriz Ponderada
  addWorksheet(wb, "08_Matriz_Ponderada", gridLines = FALSE)
  mp_df <- cbind(data.frame(Sector = as.integer(rownames(mat$ponderada))),
                 as.data.frame(mat$ponderada))
  writeData(wb, "08_Matriz_Ponderada",
            "MATRIZ DE ADYACENCIA PONDERADA (Fila = Origen, Columna = Destino)", startRow = 1)
  addStyle(wb, "08_Matriz_Ponderada", s_titulo, rows = 1, cols = 1:ncol(mp_df), gridExpand = TRUE)
  writeData(wb, "08_Matriz_Ponderada", mp_df, startRow = 3, headerStyle = s_header, borders = "all")
  freezePane(wb, "08_Matriz_Ponderada", firstActiveRow = 4, firstActiveCol = 2)

  # Extractores / expulsores
  top_ext <- head(caract$extractores, 20)
  top_exp <- head(caract$expulsores,  20)
  add_hoja("11_Extractores_Top20",
           "TOP 20 SECTORES EXTRACTORES (mayor recepción de trabajadores)", top_ext)
  add_hoja("12_Expulsores_Top20",
           "TOP 20 SECTORES EXPULSORES (mayor salida de trabajadores)", top_exp)

  # Guardar
  fname <- file.path(dir_out, sprintf("%02d_Analisis_%s.xlsx", num, nombre))
  saveWorkbook(wb, fname, overwrite = TRUE)
  message(sprintf("  ✓ Excel guardado: %s", basename(fname)))
}

exportar_comparativo <- function(lista_metricas, dir_out = DIR_SALIDA) {
  wb <- createWorkbook()
  addWorksheet(wb, "Metricas_Comparadas")

  comp_df <- rbindlist(lapply(names(lista_metricas), function(p) {
    d <- copy(lista_metricas[[p]]$globales)
    d$Periodo <- p
    d
  }))
  comp_wide <- dcast(comp_df, Metrica + Descripcion ~ Periodo, value.var = "Valor")

  # Orden de columnas = orden cronológico de períodos
  cols_per <- intersect(sapply(PERIODOS, `[[`, "nombre"), names(comp_wide))
  setcolorder(comp_wide, c("Metrica", "Descripcion", cols_per))

  writeDataTable(wb, "Metricas_Comparadas", as.data.frame(comp_wide),
                 tableStyle = "TableStyleMedium2")
  setColWidths(wb, "Metricas_Comparadas", cols = 1:ncol(comp_wide), widths = "auto")

  fname <- file.path(dir_out, "00_Comparativo_Todos_Periodos.xlsx")
  saveWorkbook(wb, fname, overwrite = TRUE)
  message("  ✓ Excel comparativo guardado.")
}


# DATOS -------------------------------------------------------------------

calcular_escalas_globales <- function(ds) {
  message("Calculando escalas globales (puede tomar unos minutos)...")
  max_trab <- 0; max_peso <- 0
  for (per in PERIODOS) {
    dt_p <- cargar_periodo(ds, per$inicio, per$fin)
    if (nrow(dt_p) == 0) next
    dt_p  <- resolver_pluriempleo(dt_p)
    tam_p <- calcular_tamano_nodos(dt_p)
    if (nrow(tam_p) > 0) max_trab <- max(max_trab, max(tam_p$T_promedio, na.rm = TRUE))
    aris_p <- construir_transiciones(dt_p)
    if (nrow(aris_p) > 0) max_peso <- max(max_peso, max(aris_p$peso))
    rm(dt_p, tam_p, aris_p); gc()
  }
  message(sprintf("  max_trab = %.1f  |  max_peso = %d", max_trab, as.integer(max_peso)))
  list(max_trab = max_trab, max_peso = max_peso)
}

correr_periodo <- function(num_periodo, ds = NULL, escalas = NULL) {
  if (is.null(ds)) ds <- open_dataset(RUTA_PARQUET)

  per <- PERIODOS[[num_periodo]]
  cat(sprintf("\n══ Procesando período [%d/6]: %s ══\n", per$num, TITULOS_PERIODO[as.character(per$num)]))

  dt <- cargar_periodo(ds, per$inicio, per$fin)
  if (nrow(dt) == 0) { message("  Sin registros. Se omite."); return(invisible(NULL)) }

  dt      <- resolver_pluriempleo(dt)
  aristas <- construir_transiciones(dt)
  if (nrow(aristas) == 0) { message("  Sin transiciones. Se omite."); return(invisible(NULL)) }

  tamano  <- calcular_tamano_nodos(dt)
  mat     <- construir_matrices(aristas)
  g       <- construir_grafo(dt, aristas = aristas, tamano = tamano)
  caract  <- calcular_caracteristicas(dt, aristas = aristas)
  met     <- calcular_metricas(g)

  max_trab <- if (!is.null(escalas)) escalas$max_trab else max(tamano$T_promedio, na.rm = TRUE)
  max_peso <- if (!is.null(escalas)) escalas$max_peso else max(aristas$peso)

  # Excel
  exportar_excel(caract, met, aristas, mat, per$nombre, per$num, DIR_SALIDA)
  
  # Redes
  visualizar_red(g, per$nombre, per$num, DIR_SALIDA, max_trab, max_peso)
  visualizar_red_ppt(g, per$nombre, per$num, DIR_SALIDA, max_trab, max_peso)

  # Heat map
  graficar_heatmap_top80(mat, aristas, per$nombre, per$num, DIR_SALIDA)

  cat(sprintf("  ✓ Período %d completado.\n", num_periodo))
  invisible(list(dt = dt, g = g, caract = caract, metricas = met, aristas = aristas, mat = mat))
}

correr_todos <- function() {

  ds <- open_dataset(RUTA_PARQUET)

  # Escalas globales para comparabilidad entre redes
  escalas <- calcular_escalas_globales(ds)

  lista_metricas <- list()
  for (per in PERIODOS) {
    res <- correr_periodo(per$num, ds = ds, escalas = escalas)
    if (!is.null(res)) lista_metricas[[per$nombre]] <- res$metricas
    gc()
  }

  if (length(lista_metricas) > 0)
    exportar_comparativo(lista_metricas, DIR_SALIDA)

  cat("  ✓ ANÁLISIS COMPLETO. Ver carpeta:", DIR_SALIDA, "\n")
}


# RESULTADOS --------------------------------------------------------------

correr_todos()           
