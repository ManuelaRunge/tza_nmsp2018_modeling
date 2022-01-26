## script to be included in Figure2.R

selectedStrategies <- c("NMSPcurrent.withCM", "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM")
AnalysisDat2 <- AnalysisDat2 %>% filter(Strategy != "revNMSP9a_noCM_new_IPTscHighOnly_SMCmoderat_noLSM")
table(AnalysisDat2$Strategy, AnalysisDat2$CMincrease)

#### Additional interventions
tempname <- c(
  "revNMSP8b_new_IPTscHighOnly_SMCmoderat_noLSM_withMDAinvlow", 
  "revNMSP4d_adj_CMall_withIPTsc_MDA_noLARV_inVeryLow",
  "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM", 
  "revNMSP4c_adj_CMall_withIPTsc_LARVurbanOnly",
  "revNMSP4b_adj_CMall_withIPTsc", 
  "revNMSP4a_adj_CMall",
  "NMSPcurrent.withCM", 
  "NMSPcurrent.withCM_noITNinLow",
  "revNMSP3b_adj_withIPTsc", 
  "revNMSP3a_adj",
  "revNMSP7d_adj_VConly_withIPTsc_MDA_noLARV_inVeryLow", 
  "revNMSP7b_adj_VConly_withIPTsc",
  "revNMSP7c_adj_VConly_withIPTsc_LARVurbanOnly", 
  "revNMSP9a_noCM_new_IPTscHighOnly_SMCmoderat_noLSM",
  "revNMSP7a_adj_VConly", 
  "NMSPcurrent"
)

tempnr <- c(1:length(tempname))

AnalysisDat2$StrategyGrp <- NA
AnalysisDat2$StrategyGrp[grep("current", AnalysisDat2$Strategy)] <- "current"
AnalysisDat2$StrategyGrp[grep("rev", AnalysisDat2$Strategy)] <- "revised"

AnalysisDat2$Strategy_label_noCM <- paste(AnalysisDat2$StrategyGrp, AnalysisDat2$FutScen_label_noCM, sep = "-")
AnalysisDat2$StrategyOrdered_nr <- factor(AnalysisDat2$Strategy, levels = c(tempname), labels = c(tempnr))
unique(AnalysisDat2$Strategy_label_noCM)


### not all strategies as defined in excel included in NMSP dat - check
length(unique(NMSPdat_long$Strategy))
length(unique(AnalysisDat2$Strategy))
table(AnalysisDat2$Strategy, AnalysisDat2$Strata)
table(NMSPdat_long$Strategy, NMSPdat_long$Strata)


get_ScenDat <- function(df=data.frame()){
  
  verylowScen_CM_addon=c()
  lowScen_CM_addon=c()
  urbanScen_CM_addon=c()
  moderateScen_CM_addon=c()
  highScen_CM_addon=c()
  
  if(nrow(df)>1){

    AnalysisDat <- AnalysisDat %>%mutate(FutScen_noCM = gsub('@Access2016@-','',FutScen),
                                           FutScen_noCM =  gsub('0.6057272-','',FutScen_noCM))

    df <- df %>% left_join(unique(AnalysisDat[,c('Strata','FutScen_noCM','FutScen_nr')]), all.x=TRUE)
    
    verylowScen_CM_addon = df$FutScen_nr[df$Strata=='very low']
    lowScen_CM_addon = df$FutScen_nr[df$Strata=='low']
    urbanScen_CM_addon = df$FutScen_nr[df$Strata=='urban']
    moderateScen_CM_addon = df$FutScen_nr[df$Strata=='moderate']
    highScen_CM_addon = df$FutScen_nr[df$Strata=='high']
  }
  
  
  
  verylowScen <- AnalysisDat2 %>%
    filter(Strata == "very low" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
    dplyr::select(FutScen_nr) %>%
    unique()
  verylowScen <- unique(c(verylowScen$FutScen_nr,verylowScen_CM_addon))
  
  lowScen <- AnalysisDat2 %>%
    filter(Strata == "low" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
    dplyr::select(FutScen_nr) %>%
    unique()
  lowScen <- unique(c(lowScen$FutScen_nr,lowScen_CM_addon))
  
  
  urbanScen <- AnalysisDat2 %>%
    filter(Strata == "urban" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
    dplyr::select(FutScen_nr) %>%
    unique()
  urbanScen <- unique(c(urbanScen$FutScen_nr,urbanScen_CM_addon))
  
  
  moderateScen <- AnalysisDat2 %>%
    filter(Strata == "moderate" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
    dplyr::select(FutScen_nr) %>%
    unique()
  moderateScen <- unique(c(moderateScen$FutScen_nr,moderateScen_CM_addon))
  
  
  highScen <- AnalysisDat2 %>%
    filter(Strata == "high" & Strategy_FutScen_nr == Strategy_FutScen_nr) %>%
    dplyr::select(FutScen_nr) %>%
    unique()
  highScen <- unique(c(highScen$FutScen_nr,highScen_CM_addon))
  
  groupVars <- c("Strata", "year", "CMincrease", "FutScen","FutScen_nr", "FutScen_label_noCM","futSNPcov")
  verylowScenDat <- AnalysisDat %>%
    filter(Strata == "very low" &
             FutScen_nr %in% verylowScen &
             year == 2020) %>%
    aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)
  
  lowScenDat <- AnalysisDat %>%
    filter(Strata == "low" &
             FutScen_nr %in% lowScen &
             year == 2020) %>%
    aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)
  
  urbanScenDat <- AnalysisDat %>%
    filter(Strata == "urban" &
             FutScen_nr %in% urbanScen &
             year == 2020) %>%
    aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)
  
  moderateScenDat <- AnalysisDat %>%
    filter(Strata == "moderate" &
             FutScen_nr %in% moderateScen &
             year == 2020) %>%
    aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)
  
  highScenDat <- AnalysisDat %>%
    filter(Strata == "high" &
             FutScen_nr %in% highScen &
             year == 2020) %>%
    aggregatDat(groupVars, "PR", "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)
  

  ### ScenDat
  ScenDat <- as.data.frame(rbind(verylowScenDat, lowScenDat, urbanScenDat, moderateScenDat, highScenDat))
  
  return(ScenDat)
  
}


ScenDat <- get_ScenDat()
unique(ScenDat$FutScen_label_noCM)
ScenDat %>% dplyr::select(Strata,FutScen_label_noCM,FutScen_nr,CMincrease) %>% unique() %>%
  group_by(Strata,CMincrease,FutScen_label_noCM) %>% 
  pivot_wider(names_from=CMincrease, values_from=FutScen_nr) %>%
  as.data.frame()


df <- ScenDat %>% dplyr::select(Strata,FutScen,FutScen_label_noCM,FutScen_nr,CMincrease) %>% unique() %>%
  mutate(FutScen_noCM = gsub('@Access2016@-','',FutScen),
         FutScen_noCM =  gsub('0.6057272-','',FutScen_noCM)) %>%
  group_by(Strata,FutScen_noCM) %>%
  tally() %>%
  filter(n==1)  %>%
  dplyr::select(Strata, FutScen_noCM)

if(nrow(df)>1){
  #redo_selection
  ScenDat <- get_ScenDat(df)
}



## Clean up labels for plot

ScenDat$FutScen_label_noCM <- gsub("LARV", "LSM", ScenDat$FutScen_label_noCM)

ScenDat <- ScenDat %>%
  mutate(
    FutScen_label_noCM = gsub("+continuous", "SNP", FutScen_label_noCM),
    FutScen_label_noCM = gsub("ITN continuous", "ITN(SNP)", FutScen_label_noCM),
    FutScen_label_noCM = gsub("ITN MRC", "ITN(MRC)", FutScen_label_noCM),
    FutScen_label_noCM = gsub(" ", "", FutScen_label_noCM),
    FutScen_label_noCM = gsub("noCMonly", "CMonly", FutScen_label_noCM),
    FutScen_label_noCM = gsub("ITNSNP", "ITN(SNP)", FutScen_label_noCM),
    FutScen_label_noCM = gsub("MRC[+]SNP", "MRC,SNP", FutScen_label_noCM),
    FutScen_label_noCM = gsub("[+]", " + ", FutScen_label_noCM)
  ) %>%
  mutate(FutScen_label_noCM = ifelse(futSNPcov == 0.7, gsub('SNP','SNP',FutScen_label_noCM), 
                                     ifelse(futSNPcov==0.4,gsub('SNP','SNP*',FutScen_label_noCM),
                                            FutScen_label_noCM))) 



FutScen_label_noCM_fct <- c("counterfactual"   
                            ,"CMonly"                       
                            ,"ITN(MRC)"               
                            ,"LSM"                    
                            ,"MDA"        
                            ,"ITN(SNP*)"              
                            ,"ITN(SNP)"               
                            ,"ITN(MRC)+LSM"     
                            ,"ITN(SNP*)+LSM"                    
                            ,"ITN(SNP)+LSM"           
                            ,"ITN(SNP)+IPTsc"  
                            ,"ITN(SNP)+IRS"  
                            ,"ITN(MRC+SNP*)"   
                            ,"ITN(MRC+SNP)" 
                            ,"ITN(MRC+SNP)+LSM"   
                            ,"ITN(MRC+SNP)+IRS"       
                            ,"ITN(SNP)+IRS+IPTsc"
                            ,"ITN(MRC+SNP)+LSM+IPTsc" 
                            ,"ITN(MRC+SNP)+IRS+IPTsc")
FutScen_label_noCM_fct <- gsub('MRC[+]SNP','MRC,SNP',FutScen_label_noCM_fct)
FutScen_label_noCM_fct <- gsub('[+]',' + ',FutScen_label_noCM_fct)

ScenDat$FutScen_label_noCM <- factor(ScenDat$FutScen_label_noCM,
                                     levels=rev(FutScen_label_noCM_fct),
                                     labels=rev(FutScen_label_noCM_fct))




#####################
### Selected strategy per strata
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
