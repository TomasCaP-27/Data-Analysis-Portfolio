#NOTA:
#Este script se desarrolló y ejecutó en un entorno Kaggle.
#Las rutas de archivo que hacen referencia a "/kaggle/input" son específicas de Kaggle.
#Para ejecución local, actualice la ruta "archivo" según corresponda.

# ------------------------------------------------------------
# Exploratory Data Analysis
# Independiente Santa Fe (2022–2025)
# ------------------------------------------------------------

# Verificar qué datasets están disponibles en el entorno de Kaggle
list.files("/kaggle/input")

# Verificar que el archivo Excel se encuentre correctamente cargado
list.files("/kaggle/input/independiente-santa-fe-match-data-20222025")
# Para ejecución local, use algo como:
# archivo <- "data/raw/SantaFe_Analisis_Datos.xlsx"

# ------------------------------------------------------------
# Carga de librerías necesarias
# ------------------------------------------------------------
# tidyverse: manipulación y visualización de datos
# readxl: lectura de archivos Excel
# ggrepel: evitar superposición de etiquetas en gráficos
library(tidyverse)
library(readxl)
library(ggrepel)

# ------------------------------------------------------------
# Definición de la ruta del archivo principal
# ------------------------------------------------------------
archivo <- "/kaggle/input/independiente-santa-fe-match-data-20222025/SantaFe_Analisis_Datos.xlsx"

# ------------------------------------------------------------
# Carga de las distintas hojas del archivo Excel
# ------------------------------------------------------------
# Partidos disputados por Santa Fe
partidos <- read_excel(archivo, sheet = "Partidos_SantaFe")

# Estadísticas de jugadores por partido
jugadores <- read_excel(archivo, sheet = "Jugadores_Partido")

# Estadísticas de porteros
porteros <- read_excel(archivo, sheet = "Porteros_Partido")

# Diccionario para agrupar posiciones por líneas
posiciones <- read_excel(archivo, sheet = "Diccionario_Posiciones")

# ------------------------------------------------------------
# Resumen general del rendimiento del equipo
# ------------------------------------------------------------
# Métricas globales considerando todos los partidos del periodo
partidos %>%
  summarise(
    Partidos = n(),
    Goles_Favor = sum(GF),
    Goles_Contra = sum(GC),
    Prom_GF = mean(GF),
    Prom_GC = mean(GC)
  )

# ------------------------------------------------------------
# Análisis del rendimiento por temporada
# ------------------------------------------------------------
# Se evalúan puntos, promedio de puntos y goles por temporada
partidos %>%
  group_by(Temporada) %>%
  summarise(
    Partidos = n(),
    Puntos = sum(Puntos),
    Prom_Puntos = Puntos / Partidos,
    Prom_GF = mean(GF),
    Prom_GC = mean(GC)
  )

# ------------------------------------------------------------
# Análisis por condición de juego (local / visitante)
# ------------------------------------------------------------
partidos %>%
  group_by(Condición) %>%
  summarise(
    Partidos = n(),
    Prom_Puntos = mean(Puntos),
    Prom_GF = mean(GF),
    Prom_GC = mean(GC)
  )

# ------------------------------------------------------------
# Análisis del rendimiento por entrenador
# ------------------------------------------------------------
# Se utilizan métricas promedio para permitir comparaciones justas
partidos %>%
  group_by(Entrenador) %>%
  summarise(
    Partidos = n(),
    Prom_Puntos = mean(Puntos),
    Prom_GF = mean(GF),
    Prom_GC = mean(GC),
    Win_Rate = mean(Resultado == "Victoria")
  ) %>%
  arrange(desc(Prom_Puntos))

# ------------------------------------------------------------
# Dataset específico para analizar eficiencia de entrenadores
# ------------------------------------------------------------
entrenadores_resumen <- partidos %>%
  group_by(Entrenador) %>%
  summarise(
    Partidos = n(),
    PPP = mean(Puntos),
    Win_Pct = mean(Resultado == "Victoria") * 100
  )

# ------------------------------------------------------------
# Análisis de jugadores por línea (defensa, mediocampo, ataque)
# ------------------------------------------------------------
# Se realiza un join para traducir las siglas de posición
jugadores_pos <- jugadores %>%
  left_join(posiciones, by = c("Posición" = "Sigla"))

jugadores_pos %>%
  group_by(Línea) %>%
  summarise(
    Minutos = sum(Minutos, na.rm = TRUE),
    Goles = sum(Goles, na.rm = TRUE)
  )

# ------------------------------------------------------------
# Cálculo de puntos por partido según condición
# ------------------------------------------------------------
ppp_condicion <- partidos %>%
  group_by(Condición) %>%
  summarise(PPP = mean(Puntos))

ppp_condicion

# Separación del PPP como local y visitante
ppp_local <- ppp_condicion$PPP[ppp_condicion$Condición == "Local"]
ppp_visitante <- ppp_condicion$PPP[ppp_condicion$Condición == "Visitante"]

# Estimación simple de puntos esperados en una temporada
# (10 partidos como local y 10 como visitante)
puntos_esperados <- (ppp_local * 10) + (ppp_visitante * 10)
puntos_esperados

# ------------------------------------------------------------
# Variabilidad del PPP entre temporadas
# ------------------------------------------------------------
ppp_por_temp <- partidos %>%
  group_by(Temporada) %>%
  summarise(
    PPP = mean(Puntos),
    Partidos = n()
  )

range(ppp_por_temp$PPP)

# ------------------------------------------------------------
# Visualización: eficiencia de entrenadores
# ------------------------------------------------------------
# Relación entre puntos por partido y porcentaje de victorias
ggplot(entrenadores_resumen,
       aes(x = PPP, y = Win_Pct, label = Entrenador)) +
  geom_point(size = 3, color = "darkorchid") +
  geom_text_repel(
    size = 3,
    max.overlaps = 10
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = "Eficiencia de entrenadores: PPP vs % de victorias",
    x = "Puntos por partido",
    y = "Porcentaje de victorias (%)"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# Creación de tablas finales para exportación
# ------------------------------------------------------------

# Resumen general del equipo
tabla_equipo_general <- partidos %>%
  summarise(
    Partidos = n(),
    Goles_Favor = sum(GF),
    Goles_Contra = sum(GC),
    PPP = mean(Puntos),
    Prom_GF = mean(GF),
    Prom_GC = mean(GC)
  )

# Resumen por temporada
tabla_temporada <- partidos %>%
  group_by(Temporada) %>%
  summarise(
    Partidos = n(),
    Puntos = sum(Puntos),
    PPP = mean(Puntos),
    Prom_GF = mean(GF),
    Prom_GC = mean(GC)
  )

# Resumen por condición de juego
tabla_condicion <- partidos %>%
  group_by(Condición) %>%
  summarise(
    Partidos = n(),
    PPP = mean(Puntos),
    Prom_GF = mean(GF),
    Prom_GC = mean(GC)
  )

# Tabla de jugadores ofensivos (mínimo 180 minutos)
tabla_jugadores <- jugadores %>%
  group_by(Jugador) %>%
  summarise(
    Minutos = sum(Minutos, na.rm = TRUE),
    Goles = sum(Goles, na.rm = TRUE),
    Asistencias = sum(Asistencias, na.rm = TRUE)
  ) %>%
  filter(Minutos >= 180) %>%
  mutate(
    GA_90 = (Goles + Asistencias) / Minutos * 90
  )

# Tabla de porteros
tabla_porteros <- porteros %>%
  group_by(Jugador) %>%
  summarise(
    Partidos = n(),
    Goles_Recibidos = sum(`Goles en contra`, na.rm = TRUE),
    Prom_GR = mean(`Goles en contra`, na.rm = TRUE)
  )

# ------------------------------------------------------------
# Exportación de tablas a archivos CSV
# ------------------------------------------------------------
write_csv(tabla_equipo_general, "tabla_equipo_general.csv")
write_csv(tabla_temporada, "tabla_temporada.csv")
write_csv(tabla_condicion, "tabla_condicion.csv")
write_csv(tabla_jugadores, "tabla_jugadores.csv")
write_csv(tabla_porteros, "tabla_porteros.csv")