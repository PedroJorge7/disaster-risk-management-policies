# The Effectiveness of Disaster Risk Management Policies in Brazilian Municipalities

This repository contains the data and code for the paper 'The Effectiveness of Disaster Risk Management Policies in Brazilian Municipalities' by Pedro Jorge Alves, Ricardo Carvalho de Andrade Lima, and Lucas Emanuel.

## About the Project

This study examines the effectiveness of disaster risk management (DRM) policies implemented by Brazilian municipalities in mitigating the social damage caused by floods. Specifically, it analyzes three commonly adopted DRM policies: flood risk mapping, supervision of flood-prone areas, and implementation of warning systems. Using an instrumental variables approach based on the spatial diffusion of DRM policies among neighboring municipalities, the paper shows that the adoption of these policies is not sufficient in mitigating the human damage caused by floods, underscoring a need for complementary strategies or enhanced implementation measures.

## Repository Structure

```
├── output/
│   ├── data_did.dta
│   └── data_did.rds
├── results/
│   ├── figures/
│   │   ├── 00_map_munic
│   │   ├── 01_histogram_desabrigados
│   │   └── 01_map_munic
│   └── tables/
│       ├── 00_adocao_das_politicas.xlsx
│       ├── 00_adocao_das_politicas.R
│       ├── 01_summary_statistics.xlsx
│       ├── 01_summary_statistics.R
│       ├── 02_effect_drm_disaster.xlsx
│       ├── 02_effect_drm_disaster.R
│       ├── 03_affect_drm_disaster_iv.xlsx
│       ├── 03_affect_drm_disaster_iv.R
│       ├── 04_robustness_check.xlsx
│       ├── 04_robustness_check.R
│       ├── 04_robustness_check_add_controls.xlsx
│       ├── 04_robustness_check_add_controls.R
│       ├── 04_robustness_check_alternative_measure.xlsx
│       ├── 04_robustness_check_alternative_measure.R
│       ├── 04_robustness_check_alternative_outcome.xlsx
│       ├── 04_robustness_check_alternative_outcome.R
│       ├── 04_robustness_check_alternative_ways_of_*.xlsx
│       ├── 04_robustness_check_alternative_ways_of_*.R
│       ├── 04_robustness_check_drop_outliers.xlsx
│       ├── 04_robustness_check_drop_outliers.R
│       ├── 05_ols_controls.xlsx
│       └── 05_ols_controls.R
```

## Analysis Scripts and Results

Each analysis has a corresponding R script that generates the Excel output file with the results.

### Policy Adoption Analysis
- **Script:** `00_adocao_das_politicas.R`
- **Output:** `00_adocao_das_politicas.xlsx`
- **Description:** Analysis of disaster risk management policy adoption patterns across Brazilian municipalities

### Summary Statistics
- **Script:** `01_summary_statistics.R`
- **Output:** `01_summary_statistics.xlsx`
- **Description:** Descriptive statistics and summary of key variables used in the analysis

### Main Effects Analysis
- **Script:** `02_effect_drm_disaster.R`
- **Output:** `02_effect_drm_disaster.xlsx`
- **Description:** Main estimation results on the effects of DRM policies on disaster outcomes

### Instrumental Variables Analysis
- **Script:** `03_affect_drm_disaster_iv.R`
- **Output:** `03_affect_drm_disaster_iv.xlsx`
- **Description:** Instrumental variables estimation using spatial diffusion of policies as instruments

### Robustness Checks

#### Main Robustness Check
- **Script:** `04_robustness_check.R`
- **Output:** `04_robustness_check.xlsx`

#### Additional Controls
- **Script:** `04_robustness_check_add_controls.R`
- **Output:** `04_robustness_check_add_controls.xlsx`
- **Description:** Robustness check including additional control variables

#### Alternative Measures
- **Script:** `04_robustness_check_alternative_measure.R`
- **Output:** `04_robustness_check_alternative_measure.xlsx`
- **Description:** Robustness check using alternative policy measurement approaches

#### Alternative Outcomes
- **Script:** `04_robustness_check_alternative_outcome.R`
- **Output:** `04_robustness_check_alternative_outcome.xlsx`
- **Description:** Robustness check with alternative outcome variables

#### Alternative Methodological Approaches
- **Script:** `04_robustness_check_alternative_ways_of_*.R`
- **Output:** `04_robustness_check_alternative_ways_of_*.xlsx`
- **Description:** Various alternative methodological specifications

#### Outlier Analysis
- **Script:** `04_robustness_check_drop_outliers.R`
- **Output:** `04_robustness_check_drop_outliers.xlsx`
- **Description:** Analysis excluding outlier observations

### OLS Controls Analysis
- **Script:** `05_ols_controls.R`
- **Output:** `05_ols_controls.xlsx`
- **Description:** OLS estimation results with various control variable specifications

## Figures

The `results/figures/` directory contains:

- `00_map_munic`: Map visualization of Brazilian municipalities
- `01_histogram_desabrigados`: Histogram showing distribution of displaced population
- `01_map_munic`: Additional municipal-level mapping

## Data

The final analytical dataset is located in the `output/` directory:

- **`data_did.rds`**: Final dataset in R format (1.294 KB)
- **`data_did.dta`**: Final dataset in Stata format (4.515 KB)

### Data Sources

- **Disaster Data:** Brazilian disaster dataset (S2ID - Integrated Disaster Information System)
- **Municipal Policies Data:** 2020 MUNIC Survey (Brazilian Institute of Geography and Statistics - IBGE)
- **Population Data:** 2010 and 2020 Census (IBGE)
- **Rainfall Data:** `brclimr` R package, which provides daily weather data for Brazilian municipalities
- **Geospatial Data:** `geobr` R package for municipality boundaries
- **Socioeconomic Data:** IPEAData, PNUD (Human Development Atlas)
- **Public Finance Data:** FINBRA (Finances of Brazil)
- **Land Cover Data:** MapBiomas

### Key Variables

| Variable | Definition | Source |
|---|---|---|
| `plano_prev_ench` | Master plan that includes flood prevention | MUNIC 2020 |
| `plano_contig` | Contingency plan | MUNIC 2020 |
| `plano_red_risc` | Municipal risk reduction plan | MUNIC 2020 |
| `lei_prev_ench` | Land use law that includes flood prevention | MUNIC 2020 |
| `map_risco_inund` | Flood risk mapping | MUNIC 2020 |
| `prog_hab_realoc` | Housing program for relocation of low-income population in risk areas | MUNIC 2020 |
| `fisc_ench` | Control and inspection mechanisms to prevent occupation in disaster-prone areas | MUNIC 2020 |
| `sist_alert_ench` | Early warning system for disasters | MUNIC 2020 |
| `cemaden` | Municipality monitored by CEMADEN | CEMADEN |
| `deceased` | Number of deceased people due to disasters | S2ID |
| `displaced` | Number of displaced people due to disasters | S2ID |
| `homeless` | Number of homeless people due to disasters | S2ID |
| `count_100mm` | Number of days with rainfall greater than 100mm | brclimr |

## Methodology

The study employs an instrumental variable (IV) approach to estimate the causal effect of DRM policies on flood-related damages. The instruments are based on the spatial diffusion of policies from neighboring municipalities. The main estimations are performed using Two-Stage Least Squares (2SLS) and fixed-effects models.

## How to Replicate

1. Make sure you have R installed with the required packages
2. The final analytical dataset is available in the `output/` directory in both R (`data_did.rds`) and Stata (`data_did.dta`) formats
3. Run any of the analysis scripts in the `results/tables/` directory to reproduce the corresponding results
4. Each R script will generate its corresponding Excel output file with the estimation results

## Citation

Alves, P. J., Lima, R. C. A., & Emanuel, L. (2025). The effectiveness of disaster risk management policies in Brazilian municipalities. *Papers in Regional Science*, 104, 100080. https://doi.org/10.1016/j.pirs.2025.100080
