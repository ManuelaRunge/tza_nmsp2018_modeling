## ===================================================================
## Analysis dat
## Input:
## - NMSP index per Council csv dataset (NMSPdat_long.csv)
## - Scenario index and label csv dataset (NMSPdat_long.csv)
## - Simulation outputs after fitting (JAGSresults_wide.RData)
## ===================================================================
library(tidyverse)
library(data.table)

SAVE = T
source(file.path("rlibrary", "customObjects.R"))

NMSPdat_long <- fread(file.path('dat', 'NMSPdat_long.csv')) %>%
  dplyr::rename(FutScen_nr = Strategy_FutScen_nr) %>%
  dplyr::select(-FutScen_label)

scendat <- fread(file.path("dat", 'FutureScenarioLabelsDat.csv')) %>%
  dplyr::mutate(FutScen = paste(CMincrease, futITNcov, futSNPcov, futIRScov, FutureScenarios, IPTcov, sep = '-'))

### add Larviciding back for selected final SMMSP
strategy_labels_to_update <- c("revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM",
                               "revNMSP8b_new_IPTscHighOnly_SMCmoderat_noLSM_withMDAinvlow")
NMSPdat_long$FutScen_nr[NMSPdat_long$FutScen_nr == 79 &
                          NMSPdat_long$Strategy %in% strategy_labels_to_update] <- 83

##### merge future scenario variables to NMSP dat
NMSPdat_long <- left_join(NMSPdat_long, scendat[, c("FutScen_nr", "FutScen")], all.x = TRUE)
NMSPdat_long <- NMSPdat_long %>%
  dplyr::select(-FutScen_nr, -Strata, -Stratification.5b_withoutUrban, -urbanrural) %>%
  as.data.frame()

#### add LSM  to urban
new_name <- gsub("onlyCMandITN", "addLARV",
                 NMSPdat_long$FutScen[NMSPdat_long$Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM" &
                                        NMSPdat_long$Stratification.5b == "urban"])

NMSPdat_long$FutScen[NMSPdat_long$Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM" &
                       NMSPdat_long$Stratification.5b == "urban"] <- new_name


## Analysis dat for future
if (get_analysis_dat) {
  load(file.path('dat', '03032019_OMStrategicPlanning_resimAll_v2_combined', 'processedExpResults', 'JAGSresults_wide.RData'))
  JAGSresults_wide <- subset(JAGSresults_wide, year >= 2016)

  ### Merge stratification data to simulation database
  AnalysisDat <- inner_join(JAGSresults_wide, NMSPdat_long)
  table(AnalysisDat$Strategy, exclude = NULL)

  ### N = 184 per strategy and year, and median statistic
  AnalysisDat %>%
    dplyr::filter(year == 2017, statistic == "median") %>%
    dplyr::group_by(Strategy) %>%
    tally()

  ## Shorten strategy names
  AnalysisDat <- AnalysisDat %>%
    mutate(Strategy_adj = gsub('_', '\n',
                               gsub('current', '', Strategy)),
           StrategyLabel = case_when(
             Strategy == "counterfactual" ~ "Counterfactual",
             Strategy == "smallestICER" ~ "Optimised for cost-effectiveness",
             Strategy == "NMSPcurrent" ~ "NMSP with maintained CM",
             Strategy == "NMSPcurrent.withCM" ~ "NMSP with improved CM",
             Strategy == "lowestCost" ~ "Achieving NMSP target at lowest costs",
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
  if (SAVE)save(AnalysisDat, file = file.path("dat", "AnalysisDat.Rdata"))
}