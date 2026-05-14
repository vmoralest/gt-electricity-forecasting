# =============================================================================
# 01_import_clean.R
# Proyecto: gt-electricity-forecasting
# Propósito: Importar todos los archivos GM anuales del AMM, extraer la
#            demanda mensual del SNI (GWh) y construir un tsibble limpio
#            listo para análisis con fpp3.
#
# Fuente: Administrador del Mercado Mayorista (AMM) — Guatemala
# Serie:  DEMANDA S.N.I. [GWH] — mensual, 2016–2026
# Autor:  [Victor Hugo Morales Trujillo]
# Fecha:  Mayo 2026
# =============================================================================
# --- 0. Paquetes -------------------------------------------------------------
library(tidyverse)   # manipulación y visualización
library(readxl)      # lectura de .xls y .xlsx
library(tsibble)     # series de tiempo tipo tidy
library(here)        # rutas relativas al proyecto .Rproj
# --- 1. Listar archivos ------------------------------------------------------

# Todos los GM*.xls y GM*.xlsx en data/raw/
# Patrón: GM + 4 dígitos de año + 4 dígitos (mmdd) + extensión
archivos_gm <- list.files(
  path    = here("data", "raw"),
  pattern = "^GM\\d{8}\\.(xls|xlsx)$",
  full.names = TRUE
)

# Verify files found
if (length(archivos_gm) == 0) {
  stop("No se encontraron archivos GM en data/raw/. ",
       "Verifica que los archivos estén nombrados como GM20200101.xls")
}

message("Archivos encontrados: ", length(archivos_gm))
message(paste(" -", basename(archivos_gm), collapse = "\n"))

# --- 2. Función para importar y extraer demanda mensual ----------------------
#
#   Formato A (GM2016): col de planta en col 1, meses abreviados (ENE…DIC)
#   Formato B (GM2017+): col de planta en col 2, meses completos (ENERO…DIC)
#
# En ambos casos buscamos la fila que contiene "DEMANDA S.N.I." en
# la columna de etiquetas, y extraemos los 12 valores mensuales.

# --- 2. Función de extracción ------------------------------------------------
 
# Esta función maneja las dos variantes de formato presentes en los GM:
#
#   Formato A (GM2016): col de planta en col 1, meses abreviados (ENE…DIC)
#   Formato B (GM2017+): col de planta en col 2, meses completos (ENERO…DIC)
#
# En ambos casos buscamos la fila que contiene "DEMANDA S.N.I." en
# la columna de etiquetas, y extraemos los 12 valores mensuales.
 
extraer_demanda <- function(ruta_archivo) {
  
  anio <- as.integer(substr(basename(ruta_archivo), 3, 6))
  
  df_raw <- read_excel(
    path      = ruta_archivo,
    sheet     = 1,
    col_names = FALSE,
    .name_repair = "minimal"
  )
  
  # ✅ FIX: col_etiqueta = 1 y col_meses = 2:13 para TODOS los años
  # read_excel descarta la col A vacía de los archivos 2017+,
  # haciendo que las etiquetas siempre queden en col 1
  col_etiqueta <- 1
  col_meses    <- 2:13
  
  etiquetas <- df_raw[[col_etiqueta]]
  fila_demanda <- which(str_detect(
    string  = as.character(etiquetas),
    pattern = "DEMANDA S\\.N\\.I\\."
  ))
  
  if (length(fila_demanda) == 0) {
    warning("No se encontró 'DEMANDA S.N.I.' en: ", basename(ruta_archivo),
            " — archivo omitido.")
    return(NULL)
  }
  
  fila_demanda <- fila_demanda[1]
  valores_mensuales <- as.numeric(df_raw[fila_demanda, col_meses])
  
  tibble(
    anio        = anio,
    mes         = 1:12,
    demanda_gwh = valores_mensuales
  )
}



# --- 3. Aplicar a todos los archivos -----------------------------------------
 
# map() aplica la función a cada archivo; list_rbind() une los resultados
datos_crudos <- map(archivos_gm, extraer_demanda) |>
  list_rbind()

# Verificar resultado inicial
glimpse(datos_crudos)
 
# --- 4. Limpiar y preparar ---------------------------------------------------
 
datos_limpios <- datos_crudos |>
 
  # Eliminar meses sin dato (año incompleto, ej. 2026 en curso)
  filter(!is.na(demanda_gwh)) |>
 
  # Eliminar valores claramente erróneos (negativos o cero)
  filter(demanda_gwh > 0) |>
 
  # Crear índice temporal yearmonth compatible con tsibble/fpp3
  mutate(
    periodo = tsibble::yearmonth(paste(anio, mes, "01", sep = "-"))
  ) |>
 
  # Ordenar cronológicamente
  arrange(periodo) |>
 
  # Seleccionar columnas finales
  select(periodo, anio, mes, demanda_gwh)
 
# Resumen rápido para verificar
cat("\n--- Resumen de la serie limpia ---\n")
cat("Rango temporal: ", as.character(min(datos_limpios$periodo)),
    " a ", as.character(max(datos_limpios$periodo)), "\n")
cat("Observaciones totales:", nrow(datos_limpios), "\n")
cat("Meses faltantes esperados por año incompleto (2026): normales\n\n")
 
summary(datos_limpios$demanda_gwh)

# --- 5. Convertir a tsibble --------------------------------------------------

demanda_ts <- datos_limpios |>
  as_tsibble(index = periodo)

# Verificar que no hay gaps en la serie
gaps <- tsibble::count_gaps(demanda_ts)

if (nrow(gaps) == 0) {
  message("Serie continua — sin gaps detectados.")
} else {
  message("ADVERTENCIA: se detectaron gaps en la serie:")
  print(gaps)
  message("Considera imputar con tidyr::fill() o interpolar.")
}

# --- 6. Guardar --------------------------------------------------------------

# CSV plano para reproducibilidad y lectura en otros entornos
write_csv(
  datos_limpios,
  here("data", "processed", "demanda_sni_mensual.csv")
)

# Objeto R con el tsibble (para cargar rápido en scripts siguientes)
saveRDS(
  demanda_ts,
  here("data", "processed", "demanda_ts.rds")
)

message("\nArchivos guardados en data/processed/:")
message("  - demanda_sni_mensual.csv")
message("  - demanda_ts.rds")

# --- 7. Visualización rápida de sanidad (no va al reporte) ------------------

demanda_ts |>
  ggplot(aes(x = periodo, y = demanda_gwh)) +
  geom_line(colour = "#2E86AB", linewidth = 0.8) +
  geom_point(colour = "#2E86AB", size = 1.2, alpha = 0.6) +
  labs(
    title    = "Demanda mensual del SNI — Guatemala",
    subtitle = "Fuente: AMM, Despacho de Carga Ejecutado",
    x        = NULL,
    y        = "Demanda (GWh)",
    caption  = "Procesado con 01_import_clean.R"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = here("output", "figures", "00_sanity_check_demanda.png"),
  width = 10, height = 4, dpi = 150
)

message("Gráfica de verificación guardada en output/figures/")
