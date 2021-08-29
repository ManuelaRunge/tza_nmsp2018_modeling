library(tidyverse)

incrementalFutScenLabels <- c(
  "increase in CM", "increase in CM - ITN MRC",
  "increase in CM - ITN MRC-IPTsc",
  "increase in CM - ITN MRC continuous",
  "increase in CM - ITN MRC continuous-IPTsc",
  "increase in CM - ITN MRC continuous-IRS",
  "increase in CM - ITN MRC continuous-IRS-IPTsc"
)


load(file.path("dat", "NMSP_SMMSP.RData")) ## including current and revised NMSP
load(file.path("simdat", "JAGSresults_wide.RData"))


## keep in mind current NMSP has LARV in urban and the new one not (in the simulations)
NMSPdat_long <- NMSPdat_long %>% dplyr::select(-FutScen_label)

#### adjustment needed for LSM, add in urban areas
unique(JAGSresults_wide$FutScen[JAGSresults_wide$FutScen_nr %in% c(79, 83)])

### add Larviciding back for selected final SMMSP
NMSPdat_long$Strategy_FutScen_nr[NMSPdat_long$Strategy_FutScen_nr == 79 &
                                   NMSPdat_long$Strategy %in% c("revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM",
                                                                "revNMSP8b_new_IPTscHighOnly_SMCmoderat_noLSM_withMDAinvlow")] <- 83 ##

NMSPdat_long$FutScen_nr <- as.numeric(NMSPdat_long$Strategy_FutScen_nr)

##### merge future scenario variables to NMSP dat
### load main future scenario numbers used
#load(file.path(OMDir, "FutureScenarios", "FutureScenarioLabelsDat_ALLInt.RData"))
load(file.path("simdat", "FutureScenarioLabelsDat_ALLInt.RData"))
dat$FutScen_nr <- as.numeric(dat$FutScen_nr)
dat$FutScen <- paste(dat$CMincrease, dat$futITNcov, dat$futSNPcov, dat$futIRScov, dat$FutureScenarios, dat$IPTcov, sep = "-")

NMSPdat_long2 <- left_join(NMSPdat_long, dat[, c("FutScen_nr", "FutScen")], all.x = TRUE)
dim(NMSPdat_long2)
dim(NMSPdat_long)
NMSPdat_long2 <- NMSPdat_long2 %>%
  #  filter(Strategy=="NMSPcurrent") %>%
  dplyr::select(-FutScen_nr, -Strata, -Stratification.5b_withoutUrban, -urbanrural) %>%
  as.data.frame()

strategynames <- unique(NMSPdat_long$Strategy)

### Check which ones not matching
sort(unique(NMSPdat_long2$FutScen)) %in% sort(unique(JAGSresults_wide$FutScen))
sort(unique(NMSPdat_long2$Stratification.5b)) %in% sort(unique(JAGSresults_wide$Stratification.5b))

#### add LSM  to urban
NMSPdat_long2$FutScen[NMSPdat_long2$Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM" & NMSPdat_long2$Stratification.5b == "urban"] <-
  gsub("onlyCMandITN", "addLARV", NMSPdat_long2$FutScen[NMSPdat_long2$Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM" & NMSPdat_long2$Stratification.5b == "urban"])

## Analysis dat for future
AnalysisDat <- JAGSresults_wide
# AnalysisDat <- subset(AnalysisDat, statistic == "median")
AnalysisDat <- subset(AnalysisDat, year >= 2016)

CMandLLINFutScenDat <- AnalysisDat %>%
  filter(FutScen_label %in% incrementalFutScenLabels &
           futITNcov != 0.5 &
           futSNPcov != 0.4) %>%
  dplyr::select(FutScen_nr, FutScen_label) %>%
  unique()

### Merge stratification data to simulation database
NMSPdat_long2$merged <- 1

### Merge stratification data to simulation database
AnalysisDat2 <- left_join(AnalysisDat, NMSPdat_long2)

## keep only those that are matched
AnalysisDat2 <- subset(AnalysisDat2, merged == 1)

## formatting
AnalysisDat2$Strategy <- as.character(AnalysisDat2$Strategy)
table(AnalysisDat2$Strategy, exclude = NULL)

### look at data
AnalysisDat2 %>%
  dplyr::filter(year == 2017, statistic == "median") %>%
  dplyr::group_by(Strategy) %>%
  tally()


## Strategy names too long
AnalysisDat2$Strategy_adj <- AnalysisDat2$Strategy
AnalysisDat2$Strategy_adj <- gsub("current", "", AnalysisDat2$Strategy_adj)
AnalysisDat2$Strategy_adj <- gsub("_", "\n", AnalysisDat2$Strategy_adj)

AnalysisDat2 <- AnalysisDat2 %>%
  dplyr::mutate(StrategyLabel = case_when(
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

table(AnalysisDat2$Strategy, AnalysisDat2$StrategyLabel, exclude = NULL)

stratlabl <- c(
  "Counterfactual", "NMSP with maintained CM", "NMSP with improved CM", "Optimised for cost-effectiveness",
  "Achieving NMSP target at lowest costs", "potential SMMSP\n with MDA", "potential SMMSP", "SMMSP"
)

AnalysisDat2$StrategyLabel <- factor(AnalysisDat2$StrategyLabel,
                                     labels = stratlabl,
                                     levels = stratlabl
)
save(AnalysisDat2, file = file.path("simdat", "AnalysisDat2.Rdata"))