# Cover Crop Incubation, Nitrogen Mineralization
# R Code for Manuscript
This repository contains the R scripts and data files used in the analysis presented in the manuscript titled "Nitrogen Mineralization of Cover Crop Residue Depends on Carbon to Nitrogen Ratio and Soil Temperature". The purpose of this repository is to provide transparency and reproducibility for the results reported in the paper.

## Table of Contents
- [Description](#description)
- [Data](#data)
- [R Scripts](#r-scripts)
- [Contact](#contact)

## Description
The project includes R scripts for data processing, analysis, and visualization of the results discussed in the manuscript. The key objectives of the analysis are:
- To inform policy for farmers who are looking to reduce nitrogen pollution by cover cropping with non-legume rye over the fall or winter period in California's Salinas Valley
- To understand how cover crop residue carbon to nitrogen ratio relates to nitrogen mineralization and hence risk for nitrogen loss (primarily nitrate leaching)
- To understand how soil temperature affects (soil + cover crop residue) nitrogen mineralization 

## Data
The data files used in this project are included in the `data` directory. The following datasets are provided:
- `Summer_pH_Final2.csv`: All pH data
- `percent_min_updated.csv`: Initial nitrogen data based on Elemental Analyzer %N data for each cover crop biomass
Plant Available Nitrogen (nitrate-N & ammonium-N) Data:
- `Gomes_Run2_711.csv`: Plant Available Nitrogen data (normalized from ppm values out of our laboratory discrete analyzer instrument to ugN/gram of dry soil; soil mass, gravimetric water content, and 40ml of 2M KCl solution values were used for the calculation).
- `Gomes_Run3_727.csv`: Plant Available Nitrogen data (normalized from ppm values out of our laboratory discrete analyzer instrument to ugN/gram of dry soil; soil mass, gravimetric water content, and 40ml of 2M KCl solution values were used for the calculation).
- `Copy_of_Gomes_Run_8.csv`: Plant Available Nitrogen data (normalized from ppm values out of our laboratory discrete analyzer instrument to ugN/gram of dry soil; soil mass, gravimetric water content, and 40ml of 2M KCl solution values were used for the calculation).
- `Gomes_Run_12_PAN_Gomes_Updated.csv`: Plant Available Nitrogen data (normalized from ppm values out of our laboratory discrete analyzer instrument to ugN/gram of dry soil; soil mass, gravimetric water content, and 40ml of 2M KCl solution values were used for the calculation).
- `Gomes_Run_15_Oct_3.csv`: Plant Available Nitrogen data (normalized from ppm values out of our laboratory discrete analyzer instrument to ugN/gram of dry soil; soil mass, gravimetric water content, and 40ml of 2M KCl solution values were used for the calculation).
- `Gomes_Run_22_Oct_31.xlsx - PAN_Soil.csv`: Plant Available Nitrogen data (normalized from ppm values out of our laboratory discrete analyzer instrument to ugN/gram of dry soil; soil mass, gravimetric water content, and 40ml of 2M KCl solution values were used for the calculation).


## R Scripts
The R scripts are located in the `scripts` directory and are structured as follows:
- `Cover_Crop_2023_Nitrogen.R`: Script for cleaning, processing, and plotting the soil nitrogen data
- `pH_Cover_Crop.R`: Script for cleaning, processing, and plotting the pH data by treatment and sampling date

##Contact
For any questions or issues, please contact:
Name: Anna Gomes
Email: amgomes@stanford.edu
