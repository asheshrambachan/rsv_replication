# Program evaluation with remotely sensed outcomes: Replication package

## Repository Structure

- `code/`: Python and R scripts for cleaning raw data, running the analysis, and generating tables and figures
  - `poverty/`
  - `cropburn/`
  - `publications/`
- `data/`: Raw and cleaned data files.
  - `poverty/`
    - `raw`: Original replication packages and SHRUG shapefiles.
    - `interim`: Raw rds outputs from simulations
    - `processed/`: Cleaned data, cleaned shapefiles, cleaned simulation results
  - `cropburn/`
    - `raw`: Original replication packages and SHRUG shapefiles
    - `processed/`: Cleaned data, cleaned shapefiles, cleaned simulation results
  - `publications/`
    - `raw`: Original literature file
    - `processed/`: Cleaned file

## Getting Started

### Step 1: Set Up a Conda Environment
Create a Conda environment with the required dependencies for Python and R:

```sh
cd path/to/rsv_replication
conda update conda
conda config --set channel_priority strict
conda env create -f conda_env.yaml
conda activate rsv
```

### Step 2: Downloading SHRUG Data 

This directory contains scripts that uses data from [SHRUG v2.1](https://www.devdatalab.org/shrug_download/) by Development Data Lab.

Note: Due to licensing restrictions (CC BY-NC-SA 4.0), the raw and processed data are not included in this repository. To obtain it, follow these steps:

1. Go to the SHRUG Download page [[SHURUG download page](https://www.devdatalab.org/shrug_download/)](https://www.devdatalab.org/shrug_download/)
2. Navigate to the tab labeled **Open Polygons and Spatial Statistics**.
3. Download the following `SHP` files and place the unzipped folders in `./data/poverty/raw` for:
	- PC11 State Polygons
    - PC11 District Polygons
	- PC11 Village Polygons
	- Shrid Polygons
4. Download the following `SHP` files and place the unzipped folders in `./data/poverty/raw` for:
	- PC11 Village Polygons
	- Shrid Polygons
  
## Replication Options

You can replicate the results using one of the following methods:

1. **Full Replication:** Run the following to execute all steps in sequence:
    ```
    Rscript ./code/poverty/00_run_all.R
    ```


2. **Partial Replication:** Run individual scripts for specific steps. 

    1. Poverty Application
        ```sh
        Rscript ./code/poverty/00_run_all.R
        ```

    2. Cropburn Application 
        ```sh
        Rscript ./code/cropburn/00_run_all.R
        ```

    3. Publication Counts Figure
        ```sh
       Rscript ./code/publications/00_run_all.R
        ```

## Data Usage Disclaimer

This repository contains proprietary data that is intended for internal use only. Redistribution or external sharing of this data is strictly prohibited.

## TODO
- [ ] `conda env export > conda_env.yml`
- [ ] Add `boot.pval` to conda env
- [ ] Add a codebook to data in kelseyjack_et_al using the [google doc file](Crop Burning Application Notes)
- [ ] Change google doc to private
- [ ] repeat the analysis using the new data from shrug