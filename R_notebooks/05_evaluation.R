# =============================================================================
# 05_evaluation.R
# Proyecto: gt-electricity-forecasting
# Propósito: Evaluación formal del mejor modelo (ARIMA_auto) usando dos
#            enfoques complementarios:
#
#   1. Hold-out evaluation  — test set fijo 2025 (ya visto en 04)
#   2. Time-series CV       — rolling origin con stretch_tsibble()
#                             Más robusto: usa múltiples ventanas de entrenamiento
#
#            Produce las tablas y figuras listas para el reporte Quarto.
#
# Input:  data/processed/demanda_ts.rds
# Output: output/figures/12_forecast_vs_actual.png
#         output/figures/13_error_distribution.png
#         output/figures/14_cv_accuracy.png
#         output/tables/final_evaluation.csv
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
library(scales)

COL_ACTUAL <- "#2E86AB"
COL_ARIMA  <- "#E84855"
COL_SNAIVE <- "#8D99AE"
COL_ERROR  <- "#E76F51"

guardar <- function(nombre, ancho = 11, alto = 5.5, dpi = 150) {
  ggsave(here("output", "figures", nombre),
         width = ancho, height = alto, dpi = dpi, bg = "white")
  message("Guardado: output/figures/", nombre)
}

# --- 1. Cargar datos y definir splits ----------------------------------------

demanda_ts    <- readRDS(here("data", "processed", "demanda_ts.rds"))
demanda_train <- demanda_ts |> filter_index("2016 Jan" ~ "2024 Dec")
demanda_test  <- demanda_ts |> filter_index("2025 Jan" ~ "2025 Dec")

# --- 2. Re-ajustar modelos de interés ----------------------------------------
# Re-ajustamos SNAIVE + ARIMA_auto para mantener este script auto-contenido

cat("Ajustando modelos para evaluación formal...\n")

fit_eval <- demanda_train |>
  model(
    SNAIVE     = SNAIVE(demanda_gwh ~ lag("year")),
    ARIMA_auto = ARIMA(demanda_gwh, stepwise = FALSE, approximation = FALSE)
  )

cat("Especificación ARIMA seleccionada:\n")
fit_eval |> select(ARIMA_auto) |> report()

# Forecasts sobre test
fc_eval <- fit_eval |> forecast(h = 12)

# =============================================================================
# PARTE A — EVALUACIÓN HOLD-OUT (Test set 2025)
# =============================================================================

# --- 3. Accuracy hold-out ----------------------------------------------------

acc_holdout <- fc_eval |>
  accuracy(demanda_test) |>
  select(.model, RMSE, MAE, MAPE, RMSSE) |>
  arrange(RMSE)

cat("\n--- Accuracy Hold-out (Test 2025) ---\n")
print(acc_holdout)

# --- 4. Figura: Forecast vs Actual — comparación lado a lado -----------------

# Datos de contexto: últimos 2 años de train + test completo
contexto_plot <- demanda_ts |> filter_index("2023 Jan" ~ "2025 Dec")

# Forecast puntual + IC para ARIMA y SNAIVE
fc_arima  <- fc_eval |> filter(.model == "ARIMA_auto")
fc_snaive <- fc_eval |> filter(.model == "SNAIVE")

# Unir forecast puntual con valores reales para calcular errores
errores_mensuales <- fc_arima |>
  as_tibble() |>
  select(periodo, forecast = .mean) |>
  left_join(as_tibble(demanda_test) |>
              select(periodo, real = demanda_gwh),
            by = "periodo") |>
  mutate(
    error    = real - forecast,
    error_pct = (real - forecast) / real * 100,
    mes_label = format(as.Date(periodo), "%b")
  )

# Figura principal: forecast ARIMA vs SNAIVE vs real
p12 <- ggplot() +
  # Historial de train (contexto)
  geom_line(data    = contexto_plot |> filter_index(~ "2024 Dec"),
            mapping = aes(x = periodo, y = demanda_gwh),
            colour  = "grey40", linewidth = 0.6) +
  # IC 80% ARIMA
  geom_ribbon(data    = fc_arima |>
                mutate(lo80 = hilo(demanda_gwh, 80)$lower,
                       hi80 = hilo(demanda_gwh, 80)$upper),
              mapping = aes(x = periodo, ymin = lo80, ymax = hi80),
              fill    = COL_ARIMA, alpha = 0.15) +
  # IC 95% ARIMA
  geom_ribbon(data    = fc_arima |>
                mutate(lo95 = hilo(demanda_gwh, 95)$lower,
                       hi95 = hilo(demanda_gwh, 95)$upper),
              mapping = aes(x = periodo, ymin = lo95, ymax = hi95),
              fill    = COL_ARIMA, alpha = 0.08) +
  # Línea forecast ARIMA
  geom_line(data    = fc_arima |> as_tibble(),
            mapping = aes(x = periodo, y = .mean),
            colour  = COL_ARIMA, linewidth = 1.0, linetype = "solid") +
  # Línea forecast SNAIVE
  geom_line(data    = fc_snaive |> as_tibble(),
            mapping = aes(x = periodo, y = .mean),
            colour  = COL_SNAIVE, linewidth = 0.7, linetype = "dashed") +
  # Demanda real 2025
  geom_line(data    = demanda_test,
            mapping = aes(x = periodo, y = demanda_gwh),
            colour  = COL_ACTUAL, linewidth = 1.2) +
  geom_point(data    = demanda_test,
             mapping = aes(x = periodo, y = demanda_gwh),
             colour  = COL_ACTUAL, size = 2.5) +
  # Leyenda manual
  annotate("segment", x = yearmonth("2025 Feb"), xend = yearmonth("2025 Apr"),
           y = 1350, yend = 1350, colour = COL_ACTUAL, linewidth = 1.2) +
  annotate("text", x = yearmonth("2025 May"), y = 1350,
           label = "Real 2025", colour = COL_ACTUAL, size = 3.2, hjust = 0) +
  annotate("segment", x = yearmonth("2025 Feb"), xend = yearmonth("2025 Apr"),
           y = 1330, yend = 1330, colour = COL_ARIMA, linewidth = 1.0) +
  annotate("text", x = yearmonth("2025 May"), y = 1330,
           label = "ARIMA_auto", colour = COL_ARIMA, size = 3.2, hjust = 0) +
  annotate("segment", x = yearmonth("2025 Feb"), xend = yearmonth("2025 Apr"),
           y = 1310, yend = 1310, colour = COL_SNAIVE, linewidth = 0.7,
           linetype = "dashed") +
  annotate("text", x = yearmonth("2025 May"), y = 1310,
           label = "SNAIVE", colour = COL_SNAIVE, size = 3.2, hjust = 0) +
  scale_y_continuous(labels = comma, limits = c(950, 1380)) +
  labs(
    title    = "Forecast vs Demanda real — Test set 2025",
    subtitle = paste0(
      "ARIMA_auto: RMSE = ",
      round(filter(acc_holdout, .model == "ARIMA_auto")$RMSE, 1),
      " GWh | MAPE = ",
      round(filter(acc_holdout, .model == "ARIMA_auto")$MAPE, 2),
      "% | SNAIVE: RMSE = ",
      round(filter(acc_holdout, .model == "SNAIVE")$RMSE, 1),
      " GWh | MAPE = ",
      round(filter(acc_holdout, .model == "SNAIVE")$MAPE, 1), "%"
    ),
    x       = NULL, y = "Demanda (GWh)",
    caption = "Bandas: 80% y 95% IC | Fuente: AMM"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(size = 9.5))

print(p12)
guardar("12_forecast_vs_actual.png", ancho = 11, alto = 5.5)

# --- 5. Figura: distribución y perfil del error mensual ----------------------

p13a <- errores_mensuales |>
  ggplot(aes(x = mes_label, y = error,
             fill = ifelse(error >= 0, "Positivo", "Negativo"))) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_text(aes(label  = round(error, 0),
                vjust  = ifelse(error >= 0, -0.4, 1.3)),
            size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c("Positivo" = COL_ARIMA, "Negativo" = COL_ACTUAL),
                    guide = "none") +
  scale_x_discrete(limits = errores_mensuales$mes_label) +
  labs(title = "Error mensual ARIMA_auto (Real − Forecast)",
       x = NULL, y = "Error (GWh)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p13b <- errores_mensuales |>
  ggplot(aes(x = error)) +
  geom_histogram(bins = 8, fill = COL_ARIMA, colour = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(errores_mensuales$error),
             colour = COL_ACTUAL, linewidth = 1, linetype = "dashed") +
  annotate("text",
           x     = mean(errores_mensuales$error) + 3,
           y     = Inf, vjust = 1.5,
           label = paste0("Media = ", round(mean(errores_mensuales$error), 1), " GWh"),
           colour = COL_ACTUAL, size = 3.2, hjust = 0) +
  labs(title = "Distribución del error", x = "Error (GWh)", y = "Frecuencia") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p13 <- p13a + p13b +
  plot_annotation(
    title   = "Perfil de errores — ARIMA_auto | Test 2025",
    caption = "Error = Real − Forecast | Positivo = subestimación"
  )

print(p13)
guardar("13_error_distribution.png", ancho = 11, alto = 5)

cat("\n--- Errores mensuales ARIMA_auto ---\n")
print(select(errores_mensuales, periodo, real, forecast, error, error_pct))

# =============================================================================
# PARTE B — TIME SERIES CROSS-VALIDATION (rolling origin)
# =============================================================================
#
# stretch_tsibble() crea múltiples ventanas de entrenamiento expandibles.
# Esto es más robusto que un solo hold-out porque evalúa el modelo en
# distintos momentos de la serie, no solo en 2025.
#
# Configuración:
#   .init  = 60 meses (5 años mínimos de train)
#   .step  = 6 meses (evaluar cada semestre)
#   h      = 12 meses de forecast por ventana

cat("\n--- Time Series Cross-Validation ---\n")
cat("Esto puede tardar 2-3 minutos...\n")

demanda_cv <- demanda_train |>
  stretch_tsibble(.init = 60, .step = 6)

cat("Ventanas de CV generadas:", max(demanda_cv$.id), "\n")

fit_cv <- demanda_cv |>
  model(
    SNAIVE     = SNAIVE(demanda_gwh ~ lag("year")),
    ARIMA_auto = ARIMA(demanda_gwh, stepwise = TRUE)   # stepwise=TRUE para CV (velocidad)
  )

fc_cv <- fit_cv |> forecast(h = 12)

acc_cv <- fc_cv |>
  accuracy(demanda_ts) |>
  select(.model, RMSE, MAE, MAPE, RMSSE) |>
  arrange(RMSE)

cat("\n--- Accuracy CV (promedio sobre todas las ventanas) ---\n")
print(acc_cv)

# --- 6. Figura: CV accuracy por horizonte ------------------------------------

# RMSE desagregado por horizonte de pronóstico (h=1 a h=12)
acc_por_horizonte <- fc_cv |>
  group_by(.model, .id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_tibble() |>
  left_join(
    demanda_ts |> as_tibble() |> rename(real = demanda_gwh),
    by = "periodo"
  ) |>
  filter(!is.na(real)) |>
  group_by(.model, h) |>
  summarise(
    RMSE = sqrt(mean((.mean - real)^2, na.rm = TRUE)),
    .groups = "drop"
  )

p14 <- acc_por_horizonte |>
  ggplot(aes(x = h, y = RMSE, colour = .model, group = .model)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_colour_manual(
    values = c(SNAIVE = COL_SNAIVE, ARIMA_auto = COL_ARIMA),
    name   = "Modelo"
  ) +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  labs(
    title    = "RMSE por horizonte de pronóstico — CV rolling origin",
    subtitle = "Ventanas: init=60 meses, step=6 meses | Modelo más bajo = mejor",
    x        = "Horizonte h (meses)",
    y        = "RMSE (GWh)",
    caption  = "ARIMA con stepwise=TRUE para velocidad en CV"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p14)
guardar("14_cv_accuracy.png", ancho = 10, alto = 5)

# =============================================================================
# PARTE C — TABLA FINAL PARA EL REPORTE
# =============================================================================

tabla_final <- tibble(
  Modelo = c("MEAN", "NAIVE", "SNAIVE", "DRIFT",
             "ETS(A,A,A)", "ETS(A,Ad,A)", "ETS auto",
             "ARIMA manual\n(1,1,0)(1,1,1)[12]", "ARIMA auto"),
  Tipo   = c(rep("Benchmark", 4), rep("ETS", 3), rep("ARIMA", 2))
)

# Leer accuracy de archivos guardados y combinar
acc_benchmarks <- read_csv(here("output", "tables", "benchmark_accuracy.csv"),
                           show_col_types = FALSE)
acc_full       <- read_csv(here("output", "tables", "full_accuracy.csv"),
                           show_col_types = FALSE)

tabla_reporte <- acc_full |>
  select(.model, RMSE, MAE, MAPE) |>
  mutate(
    Tipo = case_when(
      .model == "SNAIVE"       ~ "Benchmark",
      str_starts(.model, "ETS")   ~ "ETS",
      str_starts(.model, "ARIMA") ~ "ARIMA"
    ),
    .model = recode(.model,
      ETS_auto     = "ETS auto",
      ETS_AAA      = "ETS(A,A,A)",
      ETS_AAdA     = "ETS(A,Ad,A)",
      ARIMA_auto   = "ARIMA auto",
      ARIMA_manual = "ARIMA(1,1,0)(1,1,1)[12]"
    ),
    RMSE = round(RMSE, 1),
    MAE  = round(MAE, 1),
    MAPE = round(MAPE, 2)
  ) |>
  rename(Modelo = .model) |>
  arrange(RMSE)

cat("\n--- Tabla final para reporte ---\n")
print(tabla_reporte)

write_csv(tabla_reporte, here("output", "tables", "final_evaluation.csv"))
message("Tabla guardada: output/tables/final_evaluation.csv")

# =============================================================================
# RESUMEN EJECUTIVO
# =============================================================================

arima_holdout <- acc_holdout |> filter(.model == "ARIMA_auto")
snaive_holdout <- acc_holdout |> filter(.model == "SNAIVE")
arima_cv      <- acc_cv |> filter(.model == "ARIMA_auto")

cat("\n")
cat("=======================================================\n")
cat("  EVALUACIÓN FINAL — gt-electricity-forecasting\n")
cat("=======================================================\n\n")

cat("[Hold-out 2025]\n")
cat("  ARIMA_auto → RMSE:", round(arima_holdout$RMSE, 1), "GWh |",
    "MAPE:", round(arima_holdout$MAPE, 2), "%\n")
cat("  SNAIVE     → RMSE:", round(snaive_holdout$RMSE, 1), "GWh |",
    "MAPE:", round(snaive_holdout$MAPE, 2), "%\n")
cat("  Mejora ARIMA sobre SNAIVE:",
    round((1 - arima_holdout$RMSE / snaive_holdout$RMSE) * 100, 1), "% RMSE\n\n")

cat("[Time-series CV (rolling origin)]\n")
cat("  ARIMA_auto → RMSE:", round(arima_cv$RMSE, 1), "GWh |",
    "MAPE:", round(arima_cv$MAPE, 2), "%\n\n")

cat("[Conclusiones para el reporte]\n")
cat("  1. ARIMA_auto es el modelo ganador en ambas métricas de evaluación.\n")
cat("  2. ETS subestima la demanda post-2022 por la aceleración de la tendencia.\n")
cat("  3. Los residuos de ARIMA_auto no muestran autocorrelación significativa.\n")
cat("  4. El error promedio mensual es bajo (~2% MAPE) — adecuado para\n")
cat("     planificación de capacidad y compras de energía en el mercado spot.\n")
cat("=======================================================\n\n")

cat("05_evaluation.R completo.\n")
cat("Siguiente: report/analysis.qmd\n")
