f_load_nmsp_scendat <- function() {
      #' add Larviciding back for selected final SMMSP
      #'
  strategy_labels_to_update <- c("revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM",
                                 "revNMSP8b_new_IPTscHighOnly_SMCmoderat_noLSM_withMDAinvlow")

  scendat <- fread(file.path("dat", 'FutureScenarioLabelsDat.csv')) %>%
    dplyr::mutate(FutScen = paste(CMincrease, futITNcov, futSNPcov, futIRScov, FutureScenarios, IPTcov, sep = '-')) %>%
    dplyr::select(FutScen, FutScen_nr, nInt, CMincrease, futITNcov, futSNPcov, futIRScov, FutureScenarios, IPTcov)

  #load(file.path('dat', 'NMSP_SMMSP.RData')) #TODO
  NMSPdat_long <- fread(file.path('dat', 'NMSPdat_long.csv')) %>%
    dplyr::select(-Strata_withoutUrban, -FutScen_label) %>%
    dplyr::mutate(Strategy_FutScen_nr = ifelse(Strategy_FutScen_nr == 79 & Strategy %in% strategy_labels_to_update, 83,
                                               Strategy_FutScen_nr))

  ### merge future scenario variables to NMSP dat
  NMSPdat_long <- NMSPdat_long %>%
    left_join(scendat[, c("FutScen_nr", "FutScen")], by = c('Strategy_FutScen_nr' = 'FutScen_nr'))

  #### add LSM  to urban
  new_name <- gsub("onlyCMandITN", "addLARV",
                   NMSPdat_long$FutScen[NMSPdat_long$Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM" &
                                          NMSPdat_long$Strata == "urban"])

  NMSPdat_long$FutScen[NMSPdat_long$Strategy == "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM" &
                         NMSPdat_long$Strata == "urban"] <- new_name

  #NMSPdat_long <- NMSPdat_long %>% dplyr::rename(FutScen_nr = Strategy_FutScen_nr)

  return(NMSPdat_long)
}
