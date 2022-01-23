
# Sub-national tailoring of malaria interventions in Mainland Tanzania: simulation of the impact of strata-specific intervention combinations using modelling
This repository includes the postprocessing and plotting scripts for the analysis of intervention simulations run for Tanzania in 2018.
The work has been published in Mal. J. 

See related work:Runge et al, Thawer et al, 

## Files
Files are stored on Zenodo.

- NMSPdat: includes Districts and intervention scenario number per NMSP strategy and strategy candidate


## Simulations
- 03032019_OMStrategicPlanning_resimAll_v2_combined

## Input:
- NMSP index per Council csv dataset (NMSPdat_long.csv)
- Scenario index and label csv dataset (NMSPdat_long.csv)
- Simulation outputs after fitting (JAGSresults_wide.RData)


## Requirements
library(tidyverse)
library(data.table)
library(DescTools)
library(matrixStats)

## Scripts
master.R


00_JAGSresults_wide.R

01_prepare_analysis_dat.R

