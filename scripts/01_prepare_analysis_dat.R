## ===================================================================
## Analysis dat
## Input:
## - NMSP index per Council csv dataset (NMSPdat_long.csv)
## - Scenario index and label csv dataset (NMSPdat_long.csv)
## - Simulation outputs after fitting (JAGSresults_wide.RData)
## ===================================================================
library(tidyverse)
library(data.table)

source(file.path("rlibrary", "customObjects.R"))
source(file.path("rlibrary", "f_mergevars.R"))

f_load_nmsp_scendat <- function() {

  strategy_labels_to_update <- c("revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM",
                                 "revNMSP8b_new_IPTscHighOnly_SMCmoderat_noLSM_withMDAinvlow")

  #load(file.path('dat', "FutureScenarioLabelsDat_ALLInt.RData"))
  #dat <- dat %>% dplyr::mutate(FutScen = paste(CMincrease, futITNcov, futSNPcov, futIRScov, FutureScenarios, IPTcov, sep = '-'))
  scendat <- fread(file.path("dat", 'FutureScenarioLabelsDat.csv')) %>%
    dplyr::mutate(FutScen = paste(CMincrease, futITNcov, futSNPcov, futIRScov, FutureScenarios, IPTcov, sep = '-'))

  #load(file.path('dat', 'NMSP_SMMSP.RData'))
  NMSPdat_long <- fread(file.path('dat', 'NMSPdat_long.csv')) %>%
    dplyr::select(-FutScen_label, -Strata, -Stratification.5b_withoutUrban, -urbanrural) %>%
    mutate(Strategy_FutScen_nr = ifelse(Strategy_FutScen_nr == 79 & Strategy %in% strategy_labels_to_update, 83,
                                        Strategy_FutScen_nr)) %>%
    left_join(scendat[, c("FutScen_nr", "FutScen")], by = c('Strategy_FutScen_nr' = 'FutScen_nr'))

  #### add LSM  to urban
  new_name <- gsub("onlyCMandITN", "addLARV",
                   NMSPdat_long2$FutScen[NMSPdat_long2$Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM" &
                                           NMSPdat_long2$Stratification.5b == "urban"])

  NMSPdat_long$FutScen[NMSPdat_long$Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM" &
                         NMSPdat_long$Stratification.5b == "urban"] <- new_name

  return(NMSPdat_long)
}

load(file.path('dat', '03032019_OMStrategicPlanning_resimAll_v2_combined', 'processedExpResults', 'JAGSresults_wide.RData'))
JAGSresults_wide <- subset(JAGSresults_wide, year >= 2016)
table(JAGSresults_wide$FutScen_nr, JAGSresults_wide$year)

## N districts 184 per FutScen
JAGSresults_wide %>%
  dplyr::filter(year == 2017, statistic == "median") %>%
  dplyr::group_by(FutScen) %>%
  tally()

JAGSresults_wide$StrataLabel <- factor(JAGSresults_wide$Stratification.5b, levels = strata_lbl, labels = strata_lbl)
AnalysisDat <- JAGSresults_wide
save(AnalysisDat, file = file.path("dat", "AnalysisDat.Rdata"))

### Analysisdat2 ?

NMSPdat_long <- f_load_nmsp_scendat()
### Check which ones not matching
sort(unique(NMSPdat_long2$FutScen)) %in% sort(unique(JAGSresults_wide$FutScen))
sort(unique(NMSPdat_long2$Stratification.5b)) %in% sort(unique(JAGSresults_wide$Stratification.5b))


### Merge stratification data to simulation database
AnalysisDat <- inner_join(JAGSresults_wide, NMSPdat_long)
table(AnalysisDat$Strategy, exclude = NULL)
dim(AnalysisDat)

### N = 184 per strategy and year, and median statistic
AnalysisDat %>%
  dplyr::filter(year == 2017, statistic == "median") %>%
  dplyr::group_by(FutScen) %>%
  tally()

## Shorten strategy names
AnalysisDat <- AnalysisDat %>%
  mutate(Strategy_adj = gsub('_', '\n',
                             gsub('current', '', Strategy)),
         StrategyLabel = case_when(
           Strategy == "counterfactual" ~ "Counterfactual",
           Strategy == "NMSPcurrent" ~ "NMSP with maintained CM",
           Strategy == "NMSPcurrent.withCM" ~ "NMSP with improved CM",
           Strategy == "NMSPcurrent.withCM_noITNinLow" ~ "",
           Strategy == "NMSPcurrent.withCM_noITNinLow_PBOproxyinHigh" ~ "",
           Strategy == "revNMSP4b_adj_CMall_withIPTsc" ~ "potential SMMSP",
           Strategy == "revNMSP4d_adj_CMall_withIPTsc_MDA_noLARV_inVeryLow" ~ "potential SMMSP\n with MDA",
           Strategy == "revNMSP8b_new_IPTscHighOnly_SMCmoderat_noLSM_withMDAinvlow" ~ "SMMSP\n with MDA",
           Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM" ~ "SMMSP"
         )) %>%
  as.data.frame()

table(AnalysisDat$Strategy, exclude = NULL)
table(AnalysisDat$StrategyLabel, exclude = NULL)

AnalysisDat$StrategyLabel <- factor(AnalysisDat$StrategyLabel, labels = strategies_lbl, levels = strategies_lbl)
AnalysisDat$StrataLabel <- factor(AnalysisDat$Stratification.5b, labels = strata_lbl, levels = strata_lbl)
table(AnalysisDat$FutScen_label, AnalysisDat$StrataLabel)

save(AnalysisDat, file = file.path("dat", "AnalysisDat.Rdata"))
