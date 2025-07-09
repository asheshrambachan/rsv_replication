# Program evaluation with remotely sensed outcomes: Replication package

## Repository Structure

- `code/`: Python and R scripts for cleaning raw data, running the analysis, and generating tables and figures
  - `antipoverty/`
  - `cropburn/`
  - `publications/`
  - `common`
- `data/`: Raw and cleaned data files.  
  - `clean/`: Cleaned experiment data, cleaned shapefiles, simulation data, and data codebooks.
    - `antipoverty/`
    - `cropburn/`
    - `publications_data.csv`
  - `raw`: Original replication packages, SHRUG shapefiles, ...
    - `antipoverty/`
    - `cropburn/`
    - `shurg/`
    - `literature_review.xlsx`
- `output`: Contains tables and figures generated from the analysis.
  - `figures`
  - `tables`

## Getting Started

### Step 1: Set Up a Conda Environment
Create a Conda environment with the required dependencies for Python and R:

```sh
cd path/to/rsv_replication
conda update conda
conda config --set channel_priority strict
conda env create -f conda_env.yaml
conda activate rsv_replication
```

### Step 2: Downloading SHRUG Data 

This directory contains scripts that uses data from [SHRUG v2.1](https://www.devdatalab.org/shrug_download/) by Development Data Lab.

Note: Due to licensing restrictions (CC BY-NC-SA 4.0), the raw and processed data are not included in this repository. To obtain it, follow these steps:

1. Go to the SHRUG Download page [[SHURUG download page](https://www.devdatalab.org/shrug_download/)](https://www.devdatalab.org/shrug_download/)
2. Navigate to the tab labeled **Open Polygons and Spatial Statistics**, and download the `SHP` format for:
	- PC11 State Polygons
    - PC11 District Polygons
	- PC11 Village Polygons
	- Shrid Polygons
3. Navigate to **Night-time lights**, and download the `CSV` format for:
	- VIIRS Night Lights (2012 - 2021)
4. Navigate to **Socio-Economic and Caste Census (2012)**, and download the `CSV` format for:
    - SECC Rural
    - SECC Urban
5. Navigate to **SECC consumption**, and download the `CSV` format for:
    - SECC Rural Consumption
    - SECC Urban Consumption
6. Replace the placeholder folder in `./data/raw/shrug`  with the unzipped folders.

## Replication Options

### Poverty Application 

You can replicate the results using one of the following methods:

1. **Full Replication:** Run the following to execute all steps in sequence:
    ```
    Rscript ./code/poverty/00_run_all.R
    ```

2. **Partial Replication:** Run individual scripts for specific steps. 

    1. Data cleaning.
        ```sh
        Rscript ./code/poverty/01_clean/00_run_all.R
        ```

    2. Run simulations. 
        ```sh
        Rscript ./code/poverty/02_analysis/00_run_all.R
        ```

    3. Generate summary statistics table.
        ```sh
        Rscript ./code/poverty/03_summary_stats.R
        ```

    4. Generate figures.
        ```sh
        Rscript ./code/poverty/04_figures/00_run_all.R
        ```

### Cropburn Application 

You can replicate the results using one of the following methods:

1. **Full Replication:** Run the following to execute all steps in sequence:
    ```
    Rscript ./code/cropburn/00_run_all.R
    ```

2. **Partial Replication:** Run individual scripts for specific steps. 

    1. Data cleaning.
        ```sh
        Rscript ./code/cropburn/01_clean_data.R
        ```

    2. Run bootstrap simulations. 
        ```sh
        Rscript ./code/cropburn/02_estimate_te.R
        ```

    3. Generate treatment effect tables.
        ```sh
        Rscript ./code/cropburn/03_tabulate_results.R
        ```

    4. Generate figures.
        ```sh
        Rscript ./code/cropburn/04_plot_assignment_maps.R
        ```


### Publication Counts

You can replicate the results using one of the following methods:

1. **Full Replication:** Run the following to execute all steps in sequence:
    ```
    Rscript ./code/publications/run_all.R
    ```

2. **Partial Replication:** Run individual scripts for specific steps. 

    1. Data cleaning.
        ```sh
        Rscript ./code/publications/01_clean_data.R
        ```

    2. Generate figures.
        ```sh
        Rscript ./code/publications/02_plot_publication_counts.R
        ```


## Data Usage Disclaimer

This repository contains proprietary data that is intended for internal use only. Redistribution or external sharing of this data is strictly prohibited.

## TODO
- [ ] `conda env export > conda_env.yml`
- [ ] Add `boot.pval` to conda env
- [ ] Add a codebook to data in kelseyjack_et_al using the [google doc file](Crop Burning Application Notes)
- [ ] Change google doc to private
- [ ] repeat the analysis using the new data from shrug