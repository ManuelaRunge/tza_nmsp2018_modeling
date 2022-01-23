## script to be included in Figure2.R

selectedStrategies <- c("NMSPcurrent.withCM", "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM")
AnalysisDat2 <- AnalysisDat2 %>% filter(Strategy != "revNMSP9a_noCM_new_IPTscHighOnly_SMCmoderat_noLSM")
table(AnalysisDat2$Strategy, AnalysisDat2$CMincrease)

#### Additional interventions
tempname <- c(
  "revNMSP8b_new_IPTscHighOnly_SMCmoderat_noLSM_withMDAinvlow", "revNMSP4d_adj_CMall_withIPTsc_MDA_noLARV_inVeryLow",
  "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM", "revNMSP4c_adj_CMall_withIPTsc_LARVurbanOnly",
  "revNMSP4b_adj_CMall_withIPTsc", "revNMSP4a_adj_CMall",
  "NMSPcurrent.withCM", "NMSPcurrent.withCM_noITNinLow",
  "revNMSP3b_adj_withIPTsc", "revNMSP3a_adj",
  "revNMSP7d_adj_VConly_withIPTsc_MDA_noLARV_inVeryLow", "revNMSP7b_adj_VConly_withIPTsc",
  "revNMSP7c_adj_VConly_withIPTsc_LARVurbanOnly", "revNMSP9a_noCM_new_IPTscHighOnly_SMCmoderat_noLSM",
  "revNMSP7a_adj_VConly", "NMSPcurrent"
)

tempnr <- c(1:length(tempname))

AnalysisDat2$StrategyGrp <- NA
AnalysisDat2$StrategyGrp[grep("current", AnalysisDat2$Strategy)] <- "current"
AnalysisDat2$StrategyGrp[grep("rev", AnalysisDat2$Strategy)] <- "revised"

AnalysisDat2$Strategy_label_noCM <- paste(AnalysisDat2$StrategyGrp, AnalysisDat2$FutScen_label_noCM, sep = "-")
AnalysisDat2$StrategyOrdered_nr <- factor(AnalysisDat2$Strategy, levels = c(tempname), labels = c(tempnr))
unique(AnalysisDat2$Strategy_label_noCM)

AnalysisDat2$StrategyNEW <- NA
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "NMSPcurrent"] <- "1A_VC only"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "NMSPcurrent.withCM"] <- "1A_with CM"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "NMSPcurrent.withCM_noITNinLow"] <- "1B_with CM, no ITN in low"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP3a_adj"] <- "3A_no IPTsc"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP3b_adj_withIPTsc"] <- "3B_IPTsc"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP4a_adj_CMall"] <- "4A_CM all"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP4b_adj_CMall_withIPTsc"] <- "4B_CM al with IPTsc"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP4c_adj_CMall_withIPTsc_LARVurbanOnly"] <- "4C_CM al with IPTsc, LSM urban only"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP4d_adj_CMall_withIPTsc_MDA_noLARV_inVeryLow"] <- "4D_CM al with IPTsc, MDA in very low"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP7a_adj_VConly"] <- "4A_VC only all"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP7b_adj_VConly_withIPTsc"] <- "4B_VC only al with IPTsc"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP7c_adj_VConly_withIPTsc_LARVurbanOnly"] <- "4C_VC only al with IPTsc, LSM urban only"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP7d_adj_VConly_withIPTsc_MDA_noLARV_inVeryLow"] <- "4D_VC only al with IPTsc, MDA in very low"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM"] <- "8A_IPTsc in high only, SMC moderate"
AnalysisDat2$StrategyNEW[AnalysisDat2$Strategy == "revNMSP8b_new_IPTscHighOnly_SMCmoderat_noLSM_withMDAinvlow"] <- "8B_IPTsc in high only, SMC moderate, MDA in very low"
table(AnalysisDat2$StrategyNEW)

AnalysisDat2$StrategyTOSEP <- AnalysisDat2$StrategyNEW
AnalysisDat2 <- AnalysisDat2 %>% separate(StrategyNEW, into = c("StrategyID", "StrategyLBL"), sep = "_")
AnalysisDat2 %>%
  dplyr::select(StrategyGrp, StrategyID, StrategyOrdered_nr) %>%
  unique()


### not all strategies as defined in excel included in NMSP dat - check
length(unique(NMSPdat_long$Strategy))
length(unique(AnalysisDat2$Strategy))
table(AnalysisDat2$Strategy, AnalysisDat2$Strata)

verylowScen <- AnalysisDat2 %>%
  filter(Strata == "very low" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
  dplyr::select(FutScen_nr) %>%
  unique()

lowScen <- AnalysisDat2 %>%
  filter(Strata == "low" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
  dplyr::select(FutScen_nr) %>%
  unique()

urbanScen <- AnalysisDat2 %>%
  filter(Strata == "urban" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
  dplyr::select(FutScen_nr) %>%
  unique()

moderateScen <- AnalysisDat2 %>%
  filter(Strata == "moderate" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
  dplyr::select(FutScen_nr) %>%
  unique()

highScen <- AnalysisDat2 %>%
  filter(Strata == "high" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
  dplyr::select(FutScen_nr) %>%
  unique()

verylowFinal <- AnalysisDat2 %>%
  filter(Strata == "very low" & Strategy == selectedStrategies[2]) %>%
  dplyr::select(FutScen_nr, Strategy, FutScen_nr) %>%
  unique()

lowFinal <- AnalysisDat2 %>%
  filter(Strata == "low" & Strategy == selectedStrategies[2]) %>%
  dplyr::select(FutScen_nr, Strategy, FutScen_nr) %>%
  unique()

urbanFinal <- AnalysisDat2 %>%
  filter(Strata == "urban" & Strategy == selectedStrategies[2]) %>%
  dplyr::select(FutScen_nr, Strategy, FutScen_nr, LARVcov) %>%
  unique()

moderateFinal <- AnalysisDat2 %>%
  filter(Strata == "moderate" & Strategy == selectedStrategies[2]) %>%
  dplyr::select(FutScen_nr, Strategy, FutScen_nr) %>%
  unique()

highFinal <- AnalysisDat2 %>%
  filter(Strata == "high" & Strategy == selectedStrategies[2]) %>%
  dplyr::select(FutScen_nr, Strategy, FutScen_nr) %>%
  unique()


groupVars <- c("Strata", "year", "CMincrease", "FutScen_nr", "FutScen_label_noCM")
verylowScenDat <- AnalysisDat %>%
  filter(Strata == "very low" &
           FutScen_nr %in% verylowScen$FutScen_nr &
           year == 2020) %>%
  aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)

lowScenDat <- AnalysisDat %>%
  filter(Strata == "low" &
           FutScen_nr %in% lowScen$FutScen_nr &
           year == 2020) %>%
  aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)

urbanScenDat <- AnalysisDat %>%
  filter(Strata == "urban" &
           FutScen_nr %in% urbanScen$FutScen_nr &
           year == 2020) %>%
  aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)

moderateScenDat <- AnalysisDat %>%
  filter(Strata == "moderate" &
           FutScen_nr %in% moderateScen$FutScen_nr &
           year == 2020) %>%
  aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)

highScenDat <- AnalysisDat %>%
  filter(Strata == "high" &
           FutScen_nr %in% highScen$FutScen_nr &
           year == 2020) %>%
  aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)


verylowREV <- verylowScenDat %>%
  filter(Strata == "very low" & FutScen_nr %in% verylowFinal$FutScen_nr) %>%
  dplyr::select(Strata, mean.val) %>%
  unique()

lowREV <- lowScenDat %>%
  filter(Strata == "low" & FutScen_nr %in% lowFinal$FutScen_nr) %>%
  dplyr::select(Strata, mean.val) %>%
  unique()

urbanREV <- urbanScenDat %>%
  filter(Strata == "urban" & FutScen_nr %in% urbanFinal$FutScen_nr) %>%
  dplyr::select(Strata, mean.val) %>%
  unique()

moderateREV <- moderateScenDat %>%
  filter(Strata == "moderate" & FutScen_nr %in% moderateFinal$FutScen_nr) %>%
  dplyr::select(Strata, mean.val) %>%
  unique()

highREV <- highScenDat %>%
  filter(Strata == "high" & FutScen_nr %in% highFinal$FutScen_nr) %>%
  dplyr::select(Strata, mean.val) %>%
  unique()

REV <- as.data.frame(rbind(verylowREV, lowREV, urbanREV, moderateREV, highREV))
# REV <-as.data.frame( rbind(verylowREV ,	 lowREV ,		 urbanREV ,	 moderateREV ,   highREV))
ScenDat <- as.data.frame(rbind(verylowScenDat, lowScenDat, urbanScenDat, moderateScenDat, highScenDat))

table(ScenDat$Strata)
ScenDat$StrataLabel2 <- factor(ScenDat$Strata,
                               levels = c("very low", "low", "urban", "moderate", "high"),
                               labels = c("very low (6)", "low (5)", "urban (9)", "moderate (6)", "high (11)")
)

## give new number
ScenDat <- ScenDat %>% mutate(FutScen_label_noCM_num = as.numeric(as.factor(FutScen_label_noCM)))

ScenDat$Intervention_plusID <- paste0(ScenDat$FutScen_label_noCM, " (", ScenDat$FutScen_label_noCM_num, ")")
ScenDat$FutScen_label_noCM <- gsub("LARV", "LSM", ScenDat$FutScen_label_noCM)

