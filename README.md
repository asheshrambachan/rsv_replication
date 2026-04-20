# Program Evaluation with Remotely Sensed Outcomes

**Ashesh Rambachan** (MIT) · **Rahul Singh** (Harvard) · **Davide Viviano** (Harvard)

[arXiv:2411.10959](https://arxiv.org/abs/2411.10959) · [`remoteoutcome` R package](https://github.com/asheshrambachan/remoteoutcome)

---

## Overview

This is the replication package for [*Program Evaluation with Remotely Sensed Outcomes*](https://arxiv.org/abs/2411.10959). We study causal inference in experiments and quasi-experiments where the economic outcome is imperfectly measured by a remotely sensed variable (RSV), such as satellite imagery or mobile phone activity. 

The [`remoteoutcome`](https://github.com/asheshrambachan/remoteoutcome) R package implements our method. This repository contains the replication code for three empirical applications.


## Requirements

All R and Python dependencies are managed via conda. Create and activate the environment with:

```bash
conda update conda
conda config --set channel_priority strict
conda env create -f environment.yml
conda activate rsv_replication
```

## Data

### SHRUG shapefiles

The smartcard and cropburn applications use shapefiles from [SHRUG v2.1](https://www.devdatalab.org/shrug_download/) by Development Data Lab (CC BY-NC-SA 4.0). Due to licensing restrictions these are not included in the repository. To obtain them:

1. Go to the [SHRUG download page](https://www.devdatalab.org/shrug_download/) and navigate to the **Open Polygons and Spatial Statistics** tab.
2. Download the following and place the unzipped folders in `data/raw/smartcards/`:
   - PC11 District Polygons
   - Shrid Polygons
3. Download the following and place the unzipped folders in `data/raw/cropburn/`:
   - PC11 District Polygons
   - PC11 Village Polygons

### Pre-processed clean data

Pre-processed clean datasets are provided in `data/clean/` and can be used to skip data construction entirely. 


## Replication

Each application has a single entry point that runs the full pipeline.

### Smartcards

Reanalyzes the effect of Smartcards (biometric payments infrastructure) on village-level poverty in Andhra Pradesh, India ([Muralidharan et al. 2016](https://doi.org/10.1093/qje/qjw029)). Uses MOSAIKS satellite features and nighttime luminosity extracted at village coordinates. Demonstrates that the RSV estimator recovers the treatment effect with the same precision as an unbiased regression estimator that has access to outcomes for all units.

**Data:** Muralidharan et al. (2016) replication package + SHRUG shapefiles (see above).

```r
source("code/smartcards/00_run_all.R")
```

### Uganda forest cover: deforestation simulation

Simulation study calibrated to a payments-for-ecosystem-services (PES) experiment in Uganda ([Jayachandran et al. 2017](https://doi.org/10.1126/science.aan0568)). Uses Hansen Global Forest Change tiles and MOSAIKS satellite features to assess RSV performance with and without experimental outcomes.

**Data:** Jayachandran et al. (2017) replication files + Hansen GFC tiles (downloaded) + MOSAIKS features (downloaded).

```bash
bash code/ugandaforestcover/00_run_all.sh
```

**Note:** The pre-built clean data in `data/clean/ugandaforestcover/` can be used to skip data construction.

### Cropburn — India crop burning

Empirical study using a randomized experiment on crop burning in India ([Jack et al. 2025](https://doi.org/10.1257/aer.20211015)). Illustrates common-practice bias in the presence of a post-outcome RSV (satellite color saturation).

**Data:** Jack et al. (2025) replication package + SHRUG shapefiles (see above).

```r
source("code/cropburn/00_run_all.R")
```

## Package Structure

```
replication_package/
├── R/                              # RSV estimator source
├── code/
│   ├── utils/                      # Shared utilities (FTE theme, simulation helpers)
│   ├── ugandaforestcover/          # Uganda forest cover application
│   │   ├── 00_run_all.sh
│   │   ├── 01_data_construction/   # Download and process raw data; train predictors
│   │   ├── 02_assumption_tests/    # Stability test (KS)
│   │   ├── 03_sims_noexpoutcomes/  # Simulation: no experimental outcomes
│   │   ├── 04_sims_expoutcomes/    # Simulation: experimental outcomes
│   │   ├── 05_figures/             # Figures
│   │   └── 06_tables/              # Tables
│   ├── smartcards/                 # India smartcard application
│   │   ├── 00_run_all.R
│   │   ├── 01_data_construction/   # Prepare survey data, satellite features, PCA
│   │   ├── 02_assumption_tests/    # No-direct-effect and stability tests (KS)
│   │   ├── 03_sims_noexpoutcomes/  # Simulation: no experimental outcomes
│   │   ├── 04_empirical/           # Empirical estimation and summarize scripts
│   │   ├── 05_figures/             # Figures
│   │   └── 06_tables/              # Tables
│   ├── cropburn/                   # India crop burning application
│   │   ├── 00_run_all.R
│   │   ├── 01_data_construction/
│   │   ├── 02_empirical/
│   │   └── 03_tables/
│   └── publications/               # Literature review figure
│       ├── 00_run_all.R
│       ├── 01_clean_data.R         # Clean literature review spreadsheet
│       └── 02_figure_publication_counts.R  # Publication count figure
├── data/
│   ├── raw/                        # Raw data (not tracked; see Data section above)
│   ├── interim/                    # Intermediate outputs (model fits, simulation results)
│   └── clean/                      # Analysis-ready datasets (CSV)
│       ├── ugandaforestcover/
│       ├── smartcards/
│       └── cropburn/
├── figures/
│   ├── ugandaforestcover/
│   │   ├── stability_Ybin/         # Stability assumption density plots
│   │   ├── sims_noexpoutcomes_Ybin/# Simulation figures: no experimental outcomes
│   │   └── sims_expoutcomes_Ybin/  # Simulation figures: experimental outcomes
│   └── smartcards/
│       ├── maps/                   # Study area maps
│       ├── stability/              # Stability assumption density plots
│       ├── no_direct_effect/       # No-direct-effect assumption density plots
│       ├── sims_noexpoutcomes/     # Simulation figures: no experimental outcomes
│       ├── empirical_full/         # Empirical results: main sample
│       └── empirical_nospillover/  # Empirical results: no-spillover robustness
└── tables/
    ├── ugandaforestcover/
    ├── smartcards/
    │   ├── empirical_full/         # Empirical tables: main sample
    │   └── empirical_nospillover/  # Empirical tables: no-spillover robustness
    └── cropburn/
```


## Reference

Rambachan, A., Singh, R., and Viviano, D. (2025). "Program Evaluation with Remotely Sensed Outcomes." [arXiv:2411.10959](https://arxiv.org/abs/2411.10959)
