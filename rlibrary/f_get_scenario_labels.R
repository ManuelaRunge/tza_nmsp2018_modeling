#### add labels for future scenarios
### run script that loads the data frame used for parameter estimation, which includes all scenarios and labels
f_get_scenario_labels <- function(simout_dir) {
  load(file.path(simout_dir, "DataForParameterEstimationCombined.Rdata"))
  #dummy 'C' and 'e' had been added in inital excel file, due to formatting weirdness
  colnames(SimDat) <- gsub("CfutITNcove", "futITNcov", colnames(SimDat))
  colnames(SimDat) <- gsub("CfutSNPcove", "futSNPcov", colnames(SimDat)) #dummy 'e' had been added in inital excel file

  futVARS <- c("FutScen", "FutScen_nr", "CMincrease", "futITNcov", "futSNPcov", "futIRScov", "IPTcov", "FutureScenarios")
  dat <- SimDat %>%
    dplyr::filter(HistScen_nr == 1) %>%
    mutate(FutScen = gsub('__', '-', FutScen)) %>%
    dplyr::select(futVARS) %>%
    unique() %>%
    as.data.frame()
  rm(SimDat)

  futVARSNum <- c("futIRScov", "futITNcov", "futSNPcov", "IPTcov")
  dat[, futVARSNum] <- lapply(futVARSNum, function(x) as.numeric(as.character(dat[, x])))


  dat <- dat %>%
    mutate(futCMlabel = ifelse(CMincrease == "@Access2016@", 'current case management', '85% treatment of cases'),
           futMDAlabel = ifelse(FutureScenarios == "addMDA", 'MDA', 'no MDA'),
           futLARVlabel = ifelse(FutureScenarios == "addLARV", 'LARV', 'no LARV'),
           futIPTSClabel = ifelse(IPTcov == 0, 'no IPTsc', 'IPTsc'),
           futIRSlabel = ifelse(futIRScov == 0, 'no IRS', 'IRS'),
           futITNlabel = ifelse(futITNcov == 0, 'no ITN mass campaign', 'ITN mass campaign'),
           futSNPlabel = ifelse(futSNPcov == 0, 'no ITN continuous', 'ITN continuous'),
           IRScov = futIRScov / 100,
           MDAcov = ifelse(FutureScenarios == "addMDA", 0.8, 0),
           LARVcov = ifelse(FutureScenarios == "addLARV", 0.8, 0),
           IPTSCcov = IPTcov / 100)

  dat <- dat %>%
    mutate(futITNdep = ifelse(futSNPcov == 0 & futITNcov == 0, 'no ITN',
                              ifelse(futSNPcov != 0 & futITNcov != 0, 'both',
                                     ifelse(futSNPcov == 0 & futITNcov != 0, 'mass-campaign', 'continuous'))),
           CMITN = ifelse(CMincrease == '@Access2016@' & futITNdep == "no ITN", 'no increase in CM, no ITN',
                          ifelse(CMincrease == '@Access2016@' & futITNdep != 'no ITN', 'no increase in CM, ITN',
                                 ifelse(CMincrease != '@Access2016@' & futITNdep == 'no ITN', 'increase in CM, no ITN',
                                        ifelse(CMincrease != '@Access2016@' & futITNdep != 'no ITN', 'increase in CM and ITN', '')))),
           FutScen_label1 = paste(futCMlabel, futITNlabel, futSNPlabel, futIRSlabel, futLARVlabel, futMDAlabel, sep = '-'),
           FutScen_label2 = paste(CMITN, futIRSlabel, futLARVlabel, futMDAlabel, sep = '-'),
           FutScen = paste(CMincrease, futITNcov, futSNPcov, futIRScov, FutureScenarios, sep = '-')
    )

  dat <- dat %>%
    mutate(FutScen_label = gsub("-no MDA", "", FutScen_label2),
           FutScen_label = gsub("-no IRS", "", FutScen_label),
           FutScen_label = gsub("-no LARV", "", FutScen_label),
           FutScen_label = gsub(", no ITN", "", FutScen_label),
           FutScen_label = gsub("no increase in CM, ", "", FutScen_label),
           FutSen_label = gsub("-no IPTsc", "", FutScen_label),
           FutScen_label = gsub("no increase in CM, no ITN-", "", FutScen_label),
           FutScen_label = gsub("and", "-", FutScen_label),
           FutScen_label = ifelse(FutScen_label == "no increase in CM", "baseline", FutScen_label),
           FutScen_label = ifelse(futSNPcov != 0, gsub("ITN", "ITN continuous", FutScen_label), FutScen_label),
           FutScen_label = ifelse(futITNcov != 0, gsub("ITN", "ITN MRC", FutScen_label), FutScen_label)
    )

  dat <- dat %>%
    mutate(FutScen_label_noCM = gsub("no increase in CM-", "", FutScen_label),
           FutScen_label_noCM = gsub("baseline", "counterfactual", FutScen_label_noCM),
           FutScen_label_noCM = gsub("increase in CM - ", "", FutScen_label_noCM),
           FutScen_label_noCM = gsub("increase in CM$", "CM only", FutScen_label_noCM),
           FutScen_label_noCM = gsub("increase in CM-", "", FutScen_label_noCM),
           FutScen_label_noCM = gsub("-", "+", FutScen_label_noCM),
           FutScen_label_noCM = gsub("ITN MRC continuous", "ITN (MRC+continuous)", FutScen_label_noCM),
           FutScen_label_noCM = gsub("ITNcontinuous", "ITN continuous", FutScen_label_noCM),
           FutScen_label_noCM = gsub("ITNMRC", "ITN MRC", FutScen_label_noCM),
    )




  return(dat)

}
