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

This directory contains scripts to generate selected shapefiles using data from SHRUG v2.1.  
Due to licensing restrictions (CC BY-NC-SA 4.0), the shapefiles themselves are not redistributed here.  
To obtain the data, please follow the steps below:

1. Visit [SHURUG download page](https://www.devdatalab.org/shrug_download/) from Development Data Lab.
2. Navigate to "Open Polygons and Spatial Statistics."
3. Download the following SHP files:
	- PC11 State Polygons
    - PC11 District Polygons
	- PC11 Subdistrict Polygons
	- PC11 Village Polygons
4. Replace the placeholder folder in `./data/raw/shrug`  with the unzipped folders.

## Replication Options

### Cropburn Application 

You can replicate the results using one of the following methods:

1. **Full Replication:** Run the following to execute all steps in sequence:
    ```
    Rscript ./code/cropburn/run_all.R
    ```

2. **Partial Replication:** Run individual scripts for specific steps. 

    1. Data cleaning.
        ```sh
        Rscript ./code/cropburn/01_clean_data.R
        ```

    2. Run bootstrap simulations. 
        ```sh
        Rscript ./code/cropburn/estimate_te.R
        ```

    3. Generate treatment effect table.
        ```sh
        Rscript ./code/cropburn/03_tabulate_results.R
        ```

    4. Generate figures.
        ```sh
        Rscript ./code/cropburn/04_plot_assignment_maps.R
        ```


### RSV Publication Counts

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