# gt-electricity-forecasting

**Time series forecasting of Guatemala's national electricity demand (SNI) using R and Quarto**

![R](https://img.shields.io/badge/R-4.3%2B-276DC3?logo=r&logoColor=white)
![fpp3](https://img.shields.io/badge/fpp3-tsibble%20%7C%20fable%20%7C%20feasts-2D6A4F)
![Quarto](https://img.shields.io/badge/Quarto-report-blue?logo=quarto)
![License](https://img.shields.io/badge/license-MIT-lightgrey)

---

## Overview

This project builds and evaluates statistical forecasting models for the monthly electricity demand of Guatemala's *Sistema Nacional Interconectado* (SNI), using 10 years of official dispatch data from the *Administrador del Mercado Mayorista* (AMM).

**Key result:** an automatically selected ARIMA model achieves **RMSE = 29 GWh** and **MAPE = 2.08%** on a held-out 2025 test set — a **50.6% improvement** over the seasonal naïve benchmark.

---

## 📄 Report
👉 [View full analysis report](https://vmoralest.github.io/gt-electricity-forecasting/analysis.html)

---

## Results at a glance

| Model | RMSE (GWh) | MAPE (%) | vs SNAIVE |
|---|---|---|---|
| **ARIMA auto** | **29.0** | **2.08** | **−50.6%** ✅ |
| ARIMA(1,1,0)(1,1,1)[12] | 54.2 | 4.08 | −7.7% |
| SNAIVE *(baseline)* | 58.7 | 4.38 | — |
| ETS(A,A,A) | 68.0 | 5.16 | +15.8% |
| ETS(A,Ad,A) | 78.1 | 5.98 | +33.0% |
| ETS auto | 80.7 | 6.30 | +37.5% |

> ETS models underperform SNAIVE because Guatemala's electricity demand growth **accelerated** post-2022 (+5–6%/year). Additive ETS averages the full training trend and systematically underestimates recent demand. ARIMA with `d=1` differencing adapts to the recent level.

---

## Data

| Attribute | Detail |
|---|---|
| Source | AMM — *Despacho de Carga Ejecutado del SNI* |
| Series | Monthly SNI electricity demand |
| Unit | GWh |
| Coverage | January 2016 – March 2026 (123 months) |
| Train / Test | 2016–2024 / 2025 (108 / 12 months) |

Raw data files (`GM[year]0101.xls/xlsx`) are sourced directly from the [AMM website](https://www.amm.org.gt) and placed in `data/raw/`. They are not tracked by git due to file size.

---

## Repository structure

```
gt-electricity-forecasting/
├── data/
│   ├── raw/               ← AMM source files (not tracked)
│   └── processed/
│       ├── demanda_sni_mensual.csv
│       └── demanda_ts.rds
├── R/
│   ├── 01_import_clean.R  ← Multi-format .xls/.xlsx ingestion → tsibble
│   ├── 02_eda.R           ← 6 EDA figures (STL, seasonal, ACF/PACF, growth)
│   ├── 03_benchmark_models.R  ← MEAN, NAIVE, SNAIVE, DRIFT
│   ├── 04_ets_arima.R     ← ETS + ARIMA fitting and model selection
│   └── 05_evaluation.R    ← Hold-out + rolling-origin CV evaluation
├── report/
│   └── analysis.qmd       ← Quarto report (renders to self-contained HTML)
├── output/
│   ├── figures/           ← All saved plots (.png)
│   └── tables/            ← Accuracy tables (.csv)
├── gt-electricity-forecasting.Rproj
└── README.md
```

---

## Methodology

### 1. Data ingestion (`01_import_clean.R`)
AMM annual dispatch files span three structural formats across the 2016–2026 period. The import function detects the `DEMANDA S.N.I.` row dynamically via regex — no hardcoded row indices — making it robust to format changes. Output: a clean `tsibble` indexed by `yearmonth`.

### 2. Exploratory analysis (`02_eda.R`)
STL decomposition (robust to outliers), seasonal plots, subseries plots, ACF/PACF, and annual growth bars. Key findings:
- Stable additive seasonal pattern (~125 GWh peak-to-peak amplitude)
- Single structural shock: COVID-19 (Mar–Dec 2020, −1.1% annual demand)
- Post-2022 trend acceleration: average +5.7%/year

### 3. Benchmarks (`03_benchmark_models.R`)
SNAIVE achieves RMSE = 58.7 GWh (MAPE = 4.4%) and serves as the performance floor. Ljung-Box test on SNAIVE residuals (p ≈ 0) confirms significant autocorrelation structure remains — justifying more complex models.

### 4. ETS + ARIMA (`04_ets_arima.R`)
Five models fitted using `fpp3::model()`. Auto-ARIMA (`stepwise = FALSE`) selects the specification with lowest AICc over the training set. All models evaluated on the 2025 hold-out set.

### 5. Evaluation (`05_evaluation.R`)
Two evaluation protocols:
- **Hold-out**: fixed 2025 test set (12 months)
- **Rolling-origin CV**: `stretch_tsibble(init = 60, step = 6)` generating 9 expanding windows

---

## How to reproduce

### Requirements

```r
install.packages(c("tidyverse", "tsibble", "fpp3",
                   "readxl", "here", "patchwork",
                   "knitr", "kableExtra"))
```

Requires R ≥ 4.3 and [Quarto](https://quarto.org) ≥ 1.4.

### Run

1. Clone the repository and open `gt-electricity-forecasting.Rproj` in RStudio
2. Download the AMM annual files (`GM[year]0101.xls/xlsx`, years 2016–2026) and place them in `data/raw/`
3. Run the scripts in order:

```r
source("R/01_import_clean.R")
source("R/02_eda.R")
source("R/03_benchmark_models.R")
source("R/04_ets_arima.R")
source("R/05_evaluation.R")
quarto::quarto_render("report/analysis.qmd")
```

The rendered report will be saved as `report/analysis.html` (self-contained, no external dependencies).

---

## Key figures

**10-year demand series with STL decomposition**

The series grew from ~830 GWh/month (2016) to ~1,250 GWh/month (2025), with a stable seasonal pattern and a single COVID-19 shock in 2020.

**ARIMA auto forecast vs actual demand 2025**

The model closely tracks the observed 2025 values, with all actual points falling within the 80% confidence bands for most months.

**Cross-validation RMSE by forecast horizon**

ARIMA outperforms SNAIVE at every horizon h = 1 to 12.

---

## Technical notes

- **MASE = NaN** in fpp3 accuracy tables: known behavior when the denominator (in-sample SNAIVE MAE) cannot be computed for all models in the same mable. RMSE and MAPE are used as primary metrics.
- **ETS underperformance**: documented and analyzed in the report (Section 5). The finding is analytically meaningful, not a bug.
- **ARIMA with `stepwise = FALSE`**: exhaustive search may take ~2 minutes on 108 observations. Expected behavior.

---

## Author

**Victor Hugo Morales Trujillo**  
Electronic Engineer | MBA | MSc Big Data & Business Intelligence  
Guatemala

[![LinkedIn](https://img.shields.io/badge/LinkedIn-Connect-0A66C2?logo=linkedin)](https://linkedin.com/in/victor-hugo-morales-trujillo-688a4b173/)
[![GitHub](https://img.shields.io/badge/GitHub-Portfolio-181717?logo=github)](https://github.com/vmoralest/)

---

## License

MIT License — see [LICENSE](LICENSE) for details.

*Data source: Administrador del Mercado Mayorista (AMM), Guatemala. All rights to source data remain with AMM.*
