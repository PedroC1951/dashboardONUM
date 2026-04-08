library(dplyr)
library(sf)
library(ggplot2)
library(officer)
base<-readr::read_rds("~/Downloads/Base para dashboard/panel_total.rds")|>
  mutate(CVEGEO = sprintf("%02d",entidad))

shp<-st_read("~/Downloads/dest22gw_c/dest22cw.shp") |> 
  select(CVEGEO, NOMGEO)


bd <- base %>%
  mutate(cuidados_total_horas = cuidados_h + (cuidados_m / 60),
grupo_edad = case_when(
    edad %in% c(97, 98, 99) ~ "No sabe / No contesta",
    edad >= 15 & edad <= 29 ~ "15 a 29 años",
    edad >= 30 & edad <= 44 ~ "30 a 44 años",
    edad >= 45 & edad <= 59 ~ "45 a 59 años",
    edad >= 60 & edad <= 74 ~ "60 a 74 años",
    edad >= 75 & edad <= 89 ~ "75 a 89 años",
    
    edad >= 90 & edad <= 96 ~ "90 años o más",
    
    TRUE ~ "Otros" 
  ))





library(tidyverse)
library(rmapshaper)
library(arrow) # Para lectura ultra rápida

# 1. Cargar y simplificar mapa
mapa <- st_read("~/Downloads/dest22gw_c/dest22cw.shp") %>%
  ms_simplify(keep = 0.05) %>% # Mantén solo el 5% de detalle
  st_transform(4326) # Formato estándar para mapas web

# 2. Resumir la base gigante (usando factor de expansión)
# DENTRO DE TU SCRIPT DE PRE-PROCESAMIENTO
bd_resumen <- bd %>%
  group_by(anio, entidad, CVEGEO, sexo, grupo_edad) %>%
  summarise(
    # --- 1. BLOQUE DE OCUPACIÓN (Población Total) ---
    # Usamos a todos para que la tasa de ocupación sea real
    peso_total_pob   = sum(factor, na.rm = TRUE),
    ocupados_total   = sum(factor[ocupado == 1], na.rm = TRUE),
    
    # --- 2. BLOQUE DE CUIDADOS (Población Cuidadora) ---
    # Solo sumamos el factor de quienes reportan > 0 horas
    cuidados_total   = sum(cuidados_total_horas * factor, na.rm = TRUE),
    peso_cuidadores  = sum(factor[cuidados_total_horas > 0], na.rm = TRUE),
    
    # Desglose por situación laboral (solo cuidadores)
    cuidados_ocu     = sum(cuidados_total_horas[ocupado == 1] * factor[ocupado == 1], na.rm = TRUE),
    peso_cuid_ocu    = sum(factor[cuidados_total_horas > 0 & ocupado == 1], na.rm = TRUE),
    
    cuidados_no_ocu  = sum(cuidados_total_horas[ocupado == 0] * factor[ocupado == 0], na.rm = TRUE),
    peso_cuid_no_ocu = sum(factor[cuidados_total_horas > 0 & ocupado == 0], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(sexo_lab = ifelse(sexo == 1, "Hombre", "Mujer"))

# Guardar con todas las variables necesarias

bd_resumen <- bd_resumen %>%
  left_join(st_drop_geometry(mapa), by = "CVEGEO")

write_parquet(bd_resumen, "data/bd_resumen.parquet")
