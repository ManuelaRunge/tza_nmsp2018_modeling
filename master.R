library(tidyverse)
library(data.table)
library(DescTools)
library(matrixStats)
library(raster)
library(malariaAtlas) # better shp to spatial dataframe function
library(ggplot2)
library(cowplot)
library(scales)
library(RColorBrewer)

### custom objects and functions for data processing and plotting
source(file.path('rlibrary', 'customObjects.R'))
source(file.path('rlibrary', 'f_AggrDat.R'))
source(file.path('rlibrary', 'f_get_scenario_labels.R'))
source(file.path('rlibrary', 'f_get_MIS_cm_dat.R'))
source(file.path('rlibrary', 'f_load_nmsp_scendat.R'))

theme_set(theme_cowplot())
NMSPdat_long <- f_load_nmsp_scendat()
simout_dir <- file.path('simdat', '03032019_OMStrategicPlanning_resimAll_v2_combined', 'processedExpResults')
selectedStrategies <- c('NMSPcurrent.withCM', 'revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM')

process_step <- 'all' # 'prepare_data''assess_fit''run_figure_scripts'
weightedAggr <- T  ## population weighted aggregation of council predicted intervention impact

### Scripts
if (process_step == 'prepare_data' | process_step == 'all' ) {
  source(file.path('scripts','00_JAGSresults_wide.R'))
  source(file.path('scripts','01_prepare_analysis_dat.R'))
}
if (process_step == 'assess_fit' | process_step == 'all') {
  #Note requires geostatistical model based prevalence estimates for Tanzania in 2018
  #source(file.path('scripts','02_assess_fit_perRegion.R'))
  #source(file.path('scripts','02_assess_fit_perStrata.R'))
}
if (process_step == 'run_figure_scripts'| process_step == 'all') {
  source(file.path('scripts','Figure1.R'))
  source(file.path('scripts','Figure2.R'))
  source(file.path('scripts','Figure3.R'))
  source(file.path('scripts','Figure4.R'))
  source(file.path('scripts','Figure5.R'))
  source(file.path('scripts','Figure6.R'))
}

