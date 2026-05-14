# =============================================================================
# 02_eda.R
# Proyecto: gt-electricity-forecasting
# Propósito: Análisis exploratorio de la demanda mensual del SNI.
#            Produce 6 visualizaciones que documentan tendencia, estacionalidad,
#            autocorrelación y crecimiento anual — insumos directos para el
#            reporte Quarto y para decidir la familia de modelos.
#
# Input:  data/processed/demanda_ts.rds
# Output: output/figures/01_timeplot.png
#         output/figures/02_stl_decomp.png
#         output/figures/03_seasonal.png
#         output/figures/04_subseries.png
#         output/figures/05_acf_pacf.png
#         output/figures/06_crecimiento_anual.png
#
# Autor:  Victor Hugo Morales Trujillo
# Fecha:  Mayo 2026
# =============================================================================

# --- 0. Paquetes -------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(feasts)    # visualizaciones para series de tiempo (fpp3 ecosystem)
library(fpp3)      # carga feasts + fable + tsibble + más
library(here)
library(patchwork) # combinar gráficas ggplot

# Paleta consistente para todo el EDA
COL_MAIN  <- "#2E86AB"   # azul principal
COL_ACC   <- "#E84855"   # rojo acento (anomalías, highlights)
COL_GRAY  <- "#8D99AE"   # gris secundario
COL_BG    <- "white"

# Helper: guardar con ajustes consistentes
guardar <- function(nombre, ancho = 10, alto = 5, dpi = 150) {
  ggsave(
    filename = here("output", "figures", nombre),
    width    = ancho,
    height   = alto,
    dpi      = dpi,
    bg       = COL_BG
  )
  message("Guardado: output/figures/", nombre)
}

# --- 1. Cargar datos ---------------------------------------------------------

demanda_ts <- readRDS(here("data", "processed", "demanda_ts.rds"))

# Vista rápida
glimpse(demanda_ts)
cat("\nRango:", as.character(min(demanda_ts$periodo)),
    "→", as.character(max(demanda_ts$periodo)), "\n")
cat("Observaciones:", nrow(demanda_ts), "\n")

# =============================================================================
# FIGURA 1: Time plot con anotaciones
# Objetivo: mostrar tendencia + marcar el impacto COVID
# =============================================================================

p1 <- demanda_ts |>
  autoplot(demanda_gwh, colour = COL_MAIN, linewidth = 0.7) +
  # Banda COVID (Mar 2020 – Dic 2020)
  annotate("rect",
           xmin = yearmonth("2020 Mar"), xmax = yearmonth("2020 Dec"),
           ymin = -Inf, ymax = Inf,
           fill = COL_ACC, alpha = 0.08) +
  annotate("text",
           x = yearmonth("2020 Jul"), y = 1290,
           label = "COVID-19", colour = COL_ACC,
           size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Demanda mensual del SNI — Guatemala (2016–2026)",
    subtitle = "Tendencia alcista sostenida con caída transitoria en 2020",
    x        = NULL,
    y        = "Demanda (GWh)",
    caption  = "Fuente: AMM, Despacho de Carga Ejecutado"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p1)
guardar("01_timeplot.png", ancho = 11, alto = 5)

# =============================================================================
# FIGURA 2: Descomposición STL
# Objetivo: separar tendencia, estacionalidad y ruido residual
# STL es robusta a outliers (COVID) gracias al parámetro robust = TRUE
# =============================================================================

stl_fit <- demanda_ts |>
  model(
    STL(demanda_gwh ~ trend(window = 13) + season(window = "periodic"),
        robust = TRUE)
  )

p2 <- stl_fit |>
  components() |>
  autoplot(colour = COL_MAIN) +
  labs(
    title   = "Descomposición STL — Demanda mensual SNI",
    caption = "STL robusta (robust=TRUE) — resistente a outliers como COVID-19"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

print(p2)
guardar("02_stl_decomp.png", ancho = 10, alto = 8)

# Extraer y reportar magnitud de cada componente
componentes <- stl_fit |> components()
cat("\n--- Magnitud de componentes STL ---\n")
cat("Tendencia (rango):", round(range(componentes$trend, na.rm = TRUE), 1), "GWh\n")
cat("Estacionalidad (amplitud):",
    round(diff(range(componentes$season_year, na.rm = TRUE)), 1), "GWh pico-a-pico\n")
cat("Residuos (sd):",
    round(sd(componentes$remainder, na.rm = TRUE), 1), "GWh\n")

# =============================================================================
# FIGURA 3: Gráfica estacional (gg_season)
# Objetivo: ver si el patrón estacional es estable o cambia con los años
# =============================================================================

p3 <- demanda_ts |>
  gg_season(demanda_gwh, alpha = 0.6, linewidth = 0.7) +
  scale_colour_viridis_c(option = "plasma", name = "Año") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Patrón estacional por año — Demanda SNI",
    subtitle = "Cada línea representa un año; el color avanza del pasado (oscuro) al presente (claro)",
    x        = "Mes",
    y        = "Demanda (GWh)",
    caption  = "Fuente: AMM"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p3)
guardar("03_seasonal.png", ancho = 10, alto = 5)

# =============================================================================
# FIGURA 4: Gráfica de subseries (gg_subseries)
# Objetivo: ver la tendencia de cada mes individualmente
# ¿Hay meses que crecen más rápido que otros?
# =============================================================================

p4 <- demanda_ts |>
  gg_subseries(demanda_gwh, colour = COL_MAIN) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Tendencia mensual por sub-serie — Demanda SNI",
    subtitle = "Línea horizontal = media del mes; tendencia = crecimiento por mes específico",
    x        = NULL,
    y        = "Demanda (GWh)",
    caption  = "Fuente: AMM"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

print(p4)
guardar("04_subseries.png", ancho = 11, alto = 5)

# =============================================================================
# FIGURA 5: ACF + PACF
# Objetivo: identificar estructura de autocorrelación para guiar ARIMA/ETS
# =============================================================================

acf_plot <- demanda_ts |>
  ACF(demanda_gwh, lag_max = 36) |>
  autoplot(colour = COL_MAIN) +
  labs(
    title    = "ACF — Demanda mensual SNI",
    subtitle = "Autocorrelación hasta lag 36 (3 años)",
    x        = "Lag (meses)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 11))

pacf_plot <- demanda_ts |>
  PACF(demanda_gwh, lag_max = 36) |>
  autoplot(colour = COL_ACC) +
  labs(
    title    = "PACF — Demanda mensual SNI",
    subtitle = "Autocorrelación parcial hasta lag 36",
    x        = "Lag (meses)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 11))

p5 <- acf_plot / pacf_plot  # patchwork: apila verticalmente

print(p5)
guardar("05_acf_pacf.png", ancho = 10, alto = 7)

# Diagnóstico textual de la ACF
acf_vals <- demanda_ts |> ACF(demanda_gwh, lag_max = 24) |> pull(acf)
cat("\n--- Diagnóstico ACF ---\n")
cat("ACF lag 1: ",  round(acf_vals[1], 3), "\n")
cat("ACF lag 12:", round(acf_vals[12], 3), "(estacionalidad)\n")
cat("ACF lag 24:", round(acf_vals[24], 3), "(estacionalidad doble)\n")

# =============================================================================
# FIGURA 6: Crecimiento anual (%)
# Objetivo: cuantificar la aceleración del crecimiento post-COVID
# =============================================================================

crecimiento_anual <- demanda_ts |>
  as_tibble() |>
  group_by(anio) |>
  summarise(
    demanda_total = sum(demanda_gwh, na.rm = TRUE),
    meses_datos   = sum(!is.na(demanda_gwh)),
    .groups = "drop"
  ) |>
  # Solo años completos (12 meses) para comparación justa
  filter(meses_datos == 12) |>
  mutate(
    crec_pct = (demanda_total / lag(demanda_total) - 1) * 100,
    color    = case_when(
      anio == 2020 ~ "COVID",
      crec_pct < 0 ~ "Negativo",
      TRUE         ~ "Positivo"
    )
  ) |>
  filter(!is.na(crec_pct))

p6 <- crecimiento_anual |>
  ggplot(aes(x = factor(anio), y = crec_pct, fill = color)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = COL_GRAY) +
  geom_text(aes(label = sprintf("%+.1f%%", crec_pct),
                vjust = ifelse(crec_pct >= 0, -0.4, 1.3)),
            size = 3.5, fontface = "bold") +
  scale_fill_manual(
    values = c("Positivo" = COL_MAIN, "Negativo" = COL_ACC, "COVID" = COL_ACC),
    guide  = "none"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.1, 0.15))) +
  labs(
    title    = "Crecimiento anual de la demanda eléctrica — Guatemala",
    subtitle = "Variación porcentual respecto al año anterior (solo años completos)",
    x        = NULL,
    y        = "Crecimiento (%)",
    caption  = "Fuente: AMM, Despacho de Carga Ejecutado"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(p6)
guardar("06_crecimiento_anual.png", ancho = 10, alto = 5)

# =============================================================================
# RESUMEN ANALÍTICO — imprime en consola para el reporte
# =============================================================================

cat("\n")
cat("=======================================================\n")
cat("  RESUMEN EDA — DEMANDA MENSUAL SNI GUATEMALA\n")
cat("=======================================================\n")

cat("\n[1] TENDENCIA\n")
cat("  Demanda promedio 2016:", round(mean(filter(as_tibble(demanda_ts), anio == 2016)$demanda_gwh), 0), "GWh/mes\n")
cat("  Demanda promedio 2025:", round(mean(filter(as_tibble(demanda_ts), anio == 2025)$demanda_gwh), 0), "GWh/mes\n")

cat("\n[2] ESTACIONALIDAD\n")
cat("  Amplitud pico-a-pico:", round(diff(range(componentes$season_year, na.rm = TRUE)), 0), "GWh\n")
cat("  → El patrón estacional es estable — apto para modelos ETS y SARIMA\n")

cat("\n[3] ACF\n")
cat("  ACF lag 1 =", round(acf_vals[1], 3), "→ autocorrelación positiva fuerte\n")
cat("  ACF lag 12 =", round(acf_vals[12], 3), "→ componente estacional claro\n")
cat("  → Confirma necesidad de diferenciación estacional en ARIMA\n")

cat("\n[4] CRECIMIENTO\n")
cat("  Crecimiento promedio anual:\n")
print(select(crecimiento_anual, anio, crec_pct))

cat("\n[5] RECOMENDACIÓN DE MODELOS\n")
cat("  - Benchmarks : SNAIVE, NAIVE, MEAN\n")
cat("  - Modelos ETS: ETS(A,A,A) o ETS(A,Ad,A) — tendencia + estacionalidad aditiva\n")
cat("  - ARIMA      : ARIMA(p,1,q)(P,1,Q)[12] — diferenciación regular + estacional\n")
cat("  → Evaluar con RMSE y MASE en ventana de test = últimos 12 meses\n")
cat("=======================================================\n\n")
