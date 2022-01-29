##-----------------------------------------------
## Master file, run all analysis for:
## 'Sub-national tailoring of malaria interventions in Mainland Tanzania:
## simulation of the impact of strata-specific intervention combinations using modelling'
##-----------------------------------------------

pckg <- c("tidyverse", "data.table", "DescTools", "matrixStats", "raster", "malariaAtlas",
          "ggplot2", "scales", "cowplot",'RColorBrewer') #read_excel
lapply(pckg, require, character.only = TRUE)

### custom objects and functions for data processing and plotting
source(file.path('rlibrary', 'customObjects.R'))
source(file.path('rlibrary', 'f_AggrDat.R'))
source(file.path('rlibrary', 'f_get_scenario_labels.R'))
source(file.path('rlibrary', 'f_get_MIS_cm_dat.R'))
source(file.path('rlibrary', 'f_load_nmsp_scendat.R'))

theme_set(theme_cowplot())
NMSPdat_long <- f_load_nmsp_scendat()

process_step <- 'run_figure_scripts' # 'prepare_data''assess_fit''run_figure_scripts'
weightedAggr <- T  ## population weighted aggregation of council predicted intervention impact
SAVEpdf = FALSE

## Strategy definitions
selectedStrategies <- c('NMSPcurrent.withCM', 'revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM')
Strata_labels <- c("very low", "low", "urban", "moderate", "high")
Strata_labels_nourban <- c("very low", "low", "moderate", "high")

### Folder of simulation experiment
simout_dir <- file.path('simdat', '03032019_OMStrategicPlanning_resimAll_v2_combined', 'processedExpResults')

### Scripts
if (process_step == 'prepare_data' | process_step == 'all') {
  source(file.path('scripts', '00_JAGSresults_wide.R'))
  source(file.path('scripts', '01_prepare_analysis_dat.R'))
}
if (process_step == 'assess_fit' | process_step == 'all') {
  source(file.path('scripts','02_assess_fit_perStrata.R')) ## access to pfpr_mbg_2018 data
}
if (process_step == 'run_figure_scripts' | process_step == 'all') {
  ## Figure1: flowchart
  source(file.path('scripts', 'Figure2.R')) # post-editing in Adobe illustrator
  source(file.path('scripts', 'Figure3.R'))
  source(file.path('scripts', 'Figure4.R'))
  source(file.path('scripts', 'Figure5.R'))
  source(file.path('scripts', 'Figure6.R'))
  source(file.path('scripts', 'Figure7.R'))
}

