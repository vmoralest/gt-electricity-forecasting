# =============================================================================
# 04_ets_arima.R
# Proyecto: gt-electricity-forecasting
# Propósito: Ajustar modelos ETS y ARIMA, comparar contra benchmarks,
#            seleccionar el mejor modelo y generar el forecast final.
#
# Modelos:
#   ETS automático  — selección por AICc sobre todas las combinaciones
#   ETS(A,A,A)      — error aditivo, tendencia aditiva, estacionalidad aditiva
#   ETS(A,Ad,A)     — igual pero con tendencia amortiguada (damped)
#   ARIMA automático — selección por AICc, stepwise=FALSE (búsqueda completa)
#   ARIMA manual    — (1,1,0)(1,1,1)[12] sugerido por ACF/PACF del EDA
#
# Comparación final incluye SNAIVE como baseline.
#
# Input:  data/processed/demanda_ts.rds
# Output: output/figures/09_ets_arima_forecast.png
#         output/figures/10_best_model_residuals.png
#         output/figures/11_final_forecast.png
#         output/tables/full_accuracy.csv
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
library(rlang)
library(fable)

COL_ACTUAL  <- "#2E86AB"
COL_ETS     <- "#2D6A4F"
COL_ARIMA   <- "#E76F51"
COL_SNAIVE  <- "#8D99AE"
COL_BEST    <- "#E84855"

guardar <- function(nombre, ancho = 11, alto = 6, dpi = 150) {
  ggsave(here("output", "figures", nombre),
         width = ancho, height = alto, dpi = dpi, bg = "white")
  message("Guardado: output/figures/", nombre)
}

# --- 1. Cargar y dividir datos -----------------------------------------------

demanda_ts <- readRDS(here("data", "processed", "demanda_ts.rds"))

demanda_train <- demanda_ts |> filter_index("2016 Jan" ~ "2024 Dec")
demanda_test  <- demanda_ts |> filter_index("2025 Jan" ~ "2025 Dec")

cat("Train:", nrow(demanda_train), "obs |",
    "Test:", nrow(demanda_test), "obs\n\n")

# --- 2. Ajustar modelos ETS + ARIMA + benchmark ------------------------------

cat("Ajustando modelos... (ARIMA con stepwise=FALSE puede tardar ~1 min)\n")

todos_los_modelos <- demanda_train |>
  model(
    # --- Benchmarks (referencia) ---
    SNAIVE       = SNAIVE(demanda_gwh ~ lag("year")),

    # --- ETS ---
    # Auto-ETS: busca la mejor combinación (E, T, S) minimizando AICc
    ETS_auto     = ETS(demanda_gwh),

    # ETS(A,A,A): error aditivo + tendencia aditiva + estacionalidad aditiva
    # Apropiado cuando la amplitud estacional no escala con el nivel
    ETS_AAA      = ETS(demanda_gwh ~ error("A") + trend("A") + season("A")),

    # ETS(A,Ad,A): tendencia amortiguada — más conservadora en el largo plazo
    # Suele generalizar mejor cuando la tendencia podría desacelerarse
    ETS_AAdA     = ETS(demanda_gwh ~ error("A") + trend("Ad") + season("A")),

    # --- ARIMA ---
    # Auto-ARIMA: búsqueda exhaustiva con stepwise=FALSE (más lento, más preciso)
    ARIMA_auto   = ARIMA(demanda_gwh, stepwise = FALSE, approximation = FALSE),

    # ARIMA manual: órdenes sugeridos por ACF/PACF del EDA
    # (1,1,0)(1,1,1)[12]: AR(1) regular + SAR(1) + SMA(1), d=1, D=1
    ARIMA_manual = ARIMA(demanda_gwh ~ pdq(1,1,0) + PDQ(1,1,1))
  )

cat("\nModelos ajustados:\n")
print(todos_los_modelos)

# --- 3. Inspeccionar especificaciones seleccionadas --------------------------

cat("\n--- Especificación ETS automático ---\n")
todos_los_modelos |> select(ETS_auto) |> report()

cat("\n--- Especificación ARIMA automático ---\n")
todos_los_modelos |> select(ARIMA_auto) |> report()

cat("\n--- AICc de modelos ETS ---\n")
todos_los_modelos |>
  select(ETS_auto, ETS_AAA, ETS_AAdA) |>
  glance() |>
  select(.model, AICc, BIC) |>
  arrange(AICc) |>
  print()

cat("\n--- AICc de modelos ARIMA ---\n")
todos_los_modelos |>
  select(ARIMA_auto, ARIMA_manual) |>
  glance() |>
  select(.model, AICc, BIC) |>
  arrange(AICc) |>
  print()

# --- 4. Forecasts sobre el test set (h = 12) ---------------------------------

todos_fc <- todos_los_modelos |>
  forecast(h = 12)

# --- 5. Accuracy en test set -------------------------------------------------

accuracy_completa <- todos_fc |>
  accuracy(demanda_test) |>
  select(.model, RMSE, MAE, MAPE, MASE) |>
  arrange(RMSE)

cat("\n--- Accuracy completa en Test 2025 ---\n")
print(accuracy_completa)

# Guardar tabla completa
todos_fc |>
  accuracy(demanda_test) |>
  arrange(RMSE) |>
  write_csv(here("output", "tables", "full_accuracy.csv"))

message("Tabla guardada: output/tables/full_accuracy.csv")

# Identificar el mejor modelo automáticamente
mejor_modelo_nombre <- accuracy_completa |>
  filter(!.model %in% c("SNAIVE")) |>    # excluir benchmark de la selección
  slice_min(RMSE) |>
  pull(.model)

cat("\n→ Mejor modelo:", mejor_modelo_nombre, "\n")

# --- 6. Figura: forecasts de ETS y ARIMA vs actuals --------------------------

contexto <- demanda_ts |> filter_index("2022 Jan" ~ "2025 Dec")

colores <- c(
  SNAIVE       = COL_SNAIVE,
  ETS_auto     = COL_ETS,
  ETS_AAA      = "#52B788",
  ETS_AAdA     = "#95D5B2",
  ARIMA_auto   = COL_ARIMA,
  ARIMA_manual = "#F4A261"
)

p9 <- todos_fc |>
  filter(.model != "SNAIVE") |>   # SNAIVE en gris de fondo, no en el facet
  autoplot(data = contexto, level = NULL, linewidth = 0.8) +
  geom_line(data    = demanda_test,
            mapping = aes(x = periodo, y = demanda_gwh),
            colour  = COL_ACTUAL, linewidth = 1.2) +
  geom_point(data    = demanda_test,
             mapping = aes(x = periodo, y = demanda_gwh),
             colour  = COL_ACTUAL, size = 2.2) +
  # Forecast SNAIVE en gris como referencia en todos los paneles
  geom_line(data = todos_fc |>
              filter(.model == "SNAIVE") |>
              as_tibble() |>
              select(periodo = periodo, .mean),
            mapping   = aes(x = periodo, y = .mean),
            colour    = COL_SNAIVE, linewidth = 0.6,
            linetype  = "dashed") +
  scale_colour_manual(values = colores, guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ .model, nrow = 2) +
  labs(
    title    = "ETS y ARIMA vs Demanda real 2025",
    subtitle = "Línea azul = real | Línea gris punteada = SNAIVE (baseline)",
    x        = NULL, y = "Demanda (GWh)",
    caption  = "Train: 2016–2024 | Test: 2025 | Fuente: AMM"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title  = element_text(face = "bold"),
        strip.text  = element_text(face = "bold"))

print(p9)
guardar("09_ets_arima_forecast.png", ancho = 13, alto = 7)

# --- 7. Diagnóstico de residuos del mejor modelo ----------------------------

cat("\n--- Diagnóstico de residuos:", mejor_modelo_nombre, "---\n")

todos_los_modelos |>
  select(all_of(mejor_modelo_nombre)) |>
  gg_tsresiduals(lag_max = 24) +
  labs(
    title   = paste("Diagnóstico de residuos —", mejor_modelo_nombre),
    caption = "Ideal: residuos sin autocorrelación, media ≈ 0, distribución aprox. normal"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

guardar("10_best_model_residuals.png", ancho = 11, alto = 7)

# Test de Ljung-Box para el mejor modelo
cat("\nLjung-Box —", mejor_modelo_nombre, ":\n")
todos_los_modelos |>
  select(all_of(mejor_modelo_nombre)) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 0) |>
  print()

# --- 8. Forecast final: 2025 + horizonte futuro 2026 ------------------------

cat("\n--- Generando forecast final (train completo → horizonte 2026) ---\n")

# Para el forecast de producción entrenamos con TODOS los datos disponibles
# (incluye 2025 y los 3 meses de 2026 que ya tenemos)
# Horizon = 9 meses para llegar a dic 2026

#modelo_final <- demanda_ts |>
#  model(
#    !!mejor_modelo_nombre := !!parse_expr(
#      case_when(
#        mejor_modelo_nombre == "ETS_auto"     ~ "ETS(demanda_gwh)",
#        mejor_modelo_nombre == "ETS_AAA"      ~ "ETS(demanda_gwh ~ error('A') + trend('A') + season('A'))",
#        mejor_modelo_nombre == "ETS_AAdA"     ~ "ETS(demanda_gwh ~ error('A') + trend('Ad') + season('A'))",
#        mejor_modelo_nombre == "ARIMA_auto"   ~ "ARIMA(demanda_gwh, stepwise=FALSE, approximation=FALSE)",
#        mejor_modelo_nombre == "ARIMA_manual" ~ "ARIMA(demanda_gwh ~ pdq(1,1,0) + PDQ(1,1,1))",
#        TRUE ~ "ETS(demanda_gwh)"
#      )
#    )
#  )

modelos <- list(
  ETS_auto = function(var) ETS({{ var }}),
  ETS_AAA  = function(var) ETS({{ var }} ~ error("A") + trend("A") + season("A")),
  ETS_AAdA = function(var) ETS({{ var }} ~ error("A") + trend("Ad") + season("A")),
  ARIMA_auto = function(var) ARIMA({{ var }}, stepwise = FALSE, approximation = FALSE),
  ARIMA_manual = function(var) ARIMA({{ var }} ~ pdq(1,1,0) + PDQ(1,1,1))
)



f_modelo <- modelos[[mejor_modelo_nombre]]

if (is.null(f_modelo)) {
  f_modelo <- function(var) ETS({{ var }})
}

modelo_final <- demanda_ts |>
  model(
    !!mejor_modelo_nombre := f_modelo(demanda_gwh)
  )


# h = meses restantes de 2026 (hasta dic)
ultimo_mes <- max(demanda_ts$periodo)
h_restante <- 12 - month(as.Date(ultimo_mes))

fc_final <- modelo_final |>
  forecast(h = h_restante)

# Figura del forecast final
p11 <- fc_final |>
  autoplot(
    data  = demanda_ts |> filter_index("2022 Jan" ~ .),
    level = c(80, 95),
    alpha = 0.2,
    colour = COL_BEST,
    linewidth = 0.9
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(
    values = c("80%" = COL_BEST, "95%" = COL_BEST),
    labels = c("80% IC", "95% IC"),
    name   = ""
  ) +
  labs(
    title    = paste("Forecast final —", mejor_modelo_nombre,
                     "— Demanda SNI Guatemala"),
    subtitle = paste0("Entrenado con serie completa (2016–",
                      year(ultimo_mes), ") | Horizon: ", h_restante, " meses"),
    x        = NULL, y = "Demanda (GWh)",
    caption  = "Fuente: AMM | Bandas: 80% y 95% IC"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p11)
guardar("11_final_forecast.png", ancho = 11, alto = 5.5)

# Tabla del forecast final
cat("\n--- Valores del forecast final ---\n")
fc_final |>
  as_tibble() |>
  select(periodo, .mean) |>
  mutate(.mean = round(.mean, 1)) |>
  rename(forecast_gwh = .mean) |>
  print()

# =============================================================================
# RESUMEN FINAL
# =============================================================================

cat("\n")
cat("=======================================================\n")
cat("  RESUMEN FINAL — ETS / ARIMA\n")
cat("=======================================================\n\n")

cat("Comparación completa (ordenada por RMSE):\n")
print(accuracy_completa)

mejor_row <- accuracy_completa |>
  filter(.model == mejor_modelo_nombre)

snaive_row <- accuracy_completa |>
  filter(.model == "SNAIVE")

mejora_rmse <- (1 - mejor_row$RMSE / snaive_row$RMSE) * 100
mejora_mape <- (1 - mejor_row$MAPE / snaive_row$MAPE) * 100

cat("\n→ Mejor modelo:   ", mejor_modelo_nombre, "\n")
cat("  RMSE:  ", round(mejor_row$RMSE, 1), "GWh",
    "(mejora", round(mejora_rmse, 1), "% vs SNAIVE)\n")
cat("  MAPE:  ", round(mejor_row$MAPE, 2), "%",
    "(mejora", round(mejora_mape, 1), "% vs SNAIVE)\n")
cat("  MASE:  ", round(mejor_row$MASE, 3), "\n")
cat("\n[MASE < 1 confirma que", mejor_modelo_nombre,
    "supera al SNAIVE baseline]\n")
cat("=======================================================\n\n")
