## Eating-Nature
This repository contains the R code used to compute the Biodiversity Risk indicator following the methodology described in:
Lauro et al. (2026), Eating Nature: Revealing the Ecological Cost of Food Trade through a Global Biodiversity-Risk Footprint.

**Repository contents**

Biodiversity_Footprint.R
R script to compute Biodiversity Risk for 176 countries and 184 commodities over the period 2005–2022.


**Input and output data**

Due to file size and licensing constraints, input and output datasets are archived on Zenodo:

Deforestation Footprint data
Zenodo record: https://zenodo.org/records/10674962
Version 1.0.0, DOI: 10.5281/zenodo.10674962 (Feb 26, 2024).

commodities.xlsx
List of commodities and associated crop types used to link to LC-IMPACT characterization factors.

CFs_land_use_average.xlsx
LC-IMPACT v1.3 characterization factors for land-use stress (average values).

BiodiversityRisk_data.xlsx
Output file containing consumption- and production-based Biodiversity Risk at the country level.

Zenodo repository (all data files):
https://zenodo.org/uploads/19824182

## Reproducing the results

To reproduce the Biodiversity Risk estimates reported in Lauro et al. (2026):

1. **Software requirements**
   - R version: 4.4.3 (or later)

2. **Data setup**
   - Download the input and output data from the Zenodo repository:
     https://zenodo.org/uploads/19824182
   - Place the downloaded files in a folder and adjust the paths in `Biodiversity_Footprint.R` accordingly).

3. **Running the script**
   - Open `Biodiversity_Footprint.R` in R or RStudio.
   - Set the working directory to the root of this repository.
   - Run the script; it will:
     - load the deforestation footprint data and characterization factors,
     - compute production- and consumption-based Biodiversity Risk.

## Citation

If you use this code or data, please cite:

Lauro, S. et al. (2026). Eating Nature: Revealing the Ecological Cost of Food Trade through a Global Biodiversity-Risk Footprint. *Ecological Economics* (forthcoming).

and the Zenodo dataset:

Lauro, S. et al. (2026). Eating-Nature data and code. Zenodo. DOI: 10.5281/zenodo.19824182
