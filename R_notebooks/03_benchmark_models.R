# =============================================================================
# 03_benchmark_models.R
# Proyecto: gt-electricity-forecasting
# Propósito: Establecer modelos de referencia (benchmarks) contra los cuales
#            comparar ETS y ARIMA en el script siguiente.
#
#            Los benchmarks son intencionalmente simples — su rol es definir
#            el "piso mínimo" de desempeño que cualquier modelo serio debe
#            superar. Un modelo complejo que no mejora a SNAIVE no justifica
#            su complejidad.
#
# Modelos ajustados:
#   MEAN   — promedio histórico global
#   NAIVE  — último valor observado (random walk)
#   SNAIVE — último valor del mismo mes del año anterior
#   DRIFT  — random walk con tendencia lineal (RW + drift)
#
# Split:
#   Train  : 2016 Jan – 2024 Dec (108 meses, 9 años)
#   Test   : 2025 Jan – 2025 Dec (12 meses, 1 año completo)
#   Futuro : 2026 Jan – 2026 Mar (observado pero fuera del test formal)
#
# Input:  data/processed/demanda_ts.rds
# Output: output/figures/07_benchmarks_forecast.png
#         output/figures/08_benchmarks_residuals.png
#         output/tables/benchmark_accuracy.csv
#
# Autor:  Victor Hugo Morales Trujillo
# Fecha:  Mayo 2026
# =============================================================================

# --- 0. Paquetes -------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fpp3)
library(here)
library(patchwork)

COL_ACTUAL <- "#2E86AB"
COL_SNAIVE <- "#E84855"
COL_NAIVE  <- "#F4A261"
COL_MEAN   <- "#8D99AE"
COL_DRIFT  <- "#2D6A4F"

guardar <- function(nombre, ancho = 11, alto = 5.5, dpi = 150) {
  ggsave(
    filename = here("output", "figures", nombre),
    width = ancho, height = alto, dpi = dpi, bg = "white"
  )
  message("Guardado: output/figures/", nombre)
}

# --- 1. Cargar y dividir datos -----------------------------------------------

demanda_ts <- readRDS(here("data", "processed", "demanda_ts.rds"))

# Train: 2016 Jan – 2024 Dec (9 años completos)
# Test:  2025 Jan – 2025 Dec (12 meses de evaluación)
demanda_train <- demanda_ts |>
  filter_index("2016 Jan" ~ "2024 Dec")

demanda_test <- demanda_ts |>
  filter_index("2025 Jan" ~ "2025 Dec")

cat("--- Split del dataset ---\n")
cat("Train:", nrow(demanda_train), "observaciones",
    "(", as.character(min(demanda_train$periodo)), "→",
    as.character(max(demanda_train$periodo)), ")\n")
cat("Test: ", nrow(demanda_test), "observaciones",
    "(", as.character(min(demanda_test$periodo)), "→",
    as.character(max(demanda_test$periodo)), ")\n\n")

# --- 2. Ajustar benchmarks ---------------------------------------------------

# Todos los modelos se ajustan sobre el set de entrenamiento
benchmarks_fit <- demanda_train |>
  model(
    MEAN   = MEAN(demanda_gwh),
    NAIVE  = NAIVE(demanda_gwh),
    SNAIVE = SNAIVE(demanda_gwh ~ lag("year")),
    DRIFT  = RW(demanda_gwh ~ drift())
  )

# Resumen de los modelos
cat("--- Modelos ajustados ---\n")
print(benchmarks_fit)

# --- 3. Generar forecasts (h = 12 meses) ------------------------------------

benchmarks_fc <- benchmarks_fit |>
  forecast(h = 12)

# Vista de los forecasts
cat("\n--- Forecasts generados ---\n")
head(benchmarks_fc, 8)

# --- 4. Visualización: forecasts vs actuals ----------------------------------

# Paleta nombrada para que ggplot la use consistentemente
colores_modelos <- c(
  MEAN   = COL_MEAN,
  NAIVE  = COL_NAIVE,
  SNAIVE = COL_SNAIVE,
  DRIFT  = COL_DRIFT
)

# Serie completa para contexto (últimos 3 años de train + test)
demanda_contexto <- demanda_ts |>
  filter_index("2022 Jan" ~ "2025 Dec")

p7 <- benchmarks_fc |>
  autoplot(
    data   = demanda_contexto,
    level  = 80,          # banda de confianza al 80%
    alpha  = 0.15,
    size   = 0.7
  ) +
  # Puntos reales del test para comparar visualmente
  geom_line(
    data    = demanda_test,
    mapping = aes(x = periodo, y = demanda_gwh),
    colour  = COL_ACTUAL,
    linewidth = 1.1,
    linetype = "solid"
  ) +
  geom_point(
    data    = demanda_test,
    mapping = aes(x = periodo, y = demanda_gwh),
    colour  = COL_ACTUAL,
    size    = 2
  ) +
  # Anotación de la línea real
  annotate("text",
           x     = yearmonth("2025 Jun"),
           y     = max(demanda_test$demanda_gwh) + 35,
           label = "Demanda real 2025",
           colour = COL_ACTUAL,
           size  = 3.2, fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = colores_modelos, name = "Modelo") +
  scale_fill_manual(values  = colores_modelos, name = "Modelo") +
  facet_wrap(~ .model, nrow = 2) +
  labs(
    title    = "Benchmarks de pronóstico — Demanda mensual SNI Guatemala",
    subtitle = "Horizon: 12 meses (2025). Banda = 80% IC. Línea azul = demanda real.",
    x        = NULL,
    y        = "Demanda (GWh)",
    caption  = "Train: 2016–2024 | Test: 2025 | Fuente: AMM"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title  = element_text(face = "bold"),
    legend.position = "none",
    strip.text  = element_text(face = "bold", size = 11)
  )

print(p7)
guardar("07_benchmarks_forecast.png", ancho = 12, alto = 7)

# --- 5. Accuracy — métricas de error -----------------------------------------

accuracy_benchmarks <- benchmarks_fit |>
  forecast(h = 12) |>
  accuracy(demanda_test)

cat("\n--- Métricas de error en Test (2025) ---\n")
accuracy_benchmarks |>
  select(.model, RMSE, MAE, MAPE, MASE) |>
  arrange(RMSE) |>
  print()

# Guardar tabla
accuracy_benchmarks |>
  select(.model, RMSE, MAE, MAPE, MASE, RMSSE) |>
  arrange(RMSE) |>
  write_csv(here("output", "tables", "benchmark_accuracy.csv"))

message("Tabla guardada: output/tables/benchmark_accuracy.csv")

# --- 6. Diagnóstico de residuos del mejor benchmark (SNAIVE) -----------------

# SNAIVE suele ser el benchmark más difícil de superar en series estacionales.
# Sus residuos nos muestran la "estructura no capturada" que ETS/ARIMA deben
# modelar para agregar valor real.

cat("\n--- Test de Ljung-Box en residuos de SNAIVE ---\n")
cat("H0: residuos son ruido blanco\n")
cat("p < 0.05 → estructura restante que los modelos avanzados deben capturar\n\n")

benchmarks_fit |>
  select(SNAIVE) |>
  gg_tsresiduals(lag_max = 24) +
  labs(
    title   = "Diagnóstico de residuos — SNAIVE",
    caption = "Si ACF de residuos muestra estructura → ETS/ARIMA pueden mejorar"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

guardar("08_benchmarks_residuals.png", ancho = 11, alto = 7)

# Test formal de autocorrelación en residuos
benchmarks_fit |>
  select(SNAIVE) |>
  augment() |>
  features(.innov, ljung_box, lag = 12, dof = 0) |>
  print()

# --- 7. Resumen para el reporte ---------------------------------------------

cat("\n")
cat("=======================================================\n")
cat("  RESUMEN — BENCHMARKS\n")
cat("=======================================================\n")

mejor <- accuracy_benchmarks |>
  arrange(RMSE) |>
  slice(1)

cat("\nMejor benchmark:", mejor$.model, "\n")
cat("  RMSE :", round(mejor$RMSE, 1), "GWh\n")
cat("  MAPE :", round(mejor$MAPE, 1), "%\n")
cat("  MASE :", round(mejor$MASE, 3), "\n")

cat("\n[Interpretación MASE]\n")
cat("  MASE < 1 → el modelo supera a SNAIVE\n")
cat("  MASE = 1 → equivalente a SNAIVE (no hay valor agregado)\n")
cat("  MASE > 1 → peor que SNAIVE\n")
cat("\n  Meta para ETS/ARIMA: MASE < 1 y RMSE <", round(mejor$RMSE, 0), "GWh\n")

cat("\n[Próximo paso]\n")
cat("  → 04_ets_arima.R: ajustar ETS y ARIMA sobre train,\n")
cat("    evaluar en test 2025, comparar vs estos benchmarks.\n")
cat("=======================================================\n\n")
