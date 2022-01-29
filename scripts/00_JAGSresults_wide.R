#' ===================================================================
#' Simulation outputs to analysis dataframe
#' Input:
#' - DataForParameterEstimationCombined.RData (before fitting)
#' - JAGSresultsCombined.RData (after fitting)
#' Output:
#' - JAGSresults_wide.RData
#' ===================================================================

##------------------------------------------------------------------------------------------
## load(file.path(simout_dir, "DataForParameterEstimationCombined.Rdata")) ## BEFORE fitting using JAGS
## See details on model calibration in:
## Runge, M., Snow, R.W., Molteni, F., Thawer, S., Mohamed, A., Mandike, R., Giorgi, E., Macharia, P.M., Smith, T.A., Lengeler, C., Pothin, E., 2020.
## Simulating the council-specific impact of anti-malaria interventions: A tool to support malaria strategic planning in Tanzania.
## PLoS ONE 15, e0228469. https://doi.org/10.1371/journal.pone.0228469
##------------------------------------------------------------------------------------------

##------------------------------------------------------------------------------------------
## Attach fitting parameters (e.g. EIR and ITN) to districts for all outcome measures
## transform format (statistics in rows and outcomes in columns instead of other way around)
## if year is NA, it corresponds to the estimated parameter EIR and ITN and not to an predicted outcome
##------------------------------------------------------------------------------------------
load(file.path(simout_dir, "JAGSresultsCombined.Rdata")) ## AFTER fitting using JAGS
JAGSresults <- JAGSresultsCombined %>%
  dplyr::mutate(FutScen_nr = as.numeric(FutScen_nr),
                year = ifelse(is.na(year), 0, year)) %>%
  dplyr::select(-NaiveSE, -q75, -q25, -`Time-seriesSE`, -DIS.min_cilu, -DIS.max_ciup,
                -DISFutScen.min_cilu, -DISFutScen.max_ciup) %>%
  tidyr::gather(statistic, value, q2.5, median, q97.5, mean, sd)

### Dataframe subsets:
##  - for predicted outcomes
selectVars <- c("District", "outcome", "FutScen_nr", "year", "statistic", "value")

JAGSresults_pred <- JAGSresults %>%
  dplyr::select(all_of(selectVars)) %>%
  dplyr::filter(outcome != "EIR" & outcome != "ITN") %>%
  tidyr::pivot_wider(names_from = outcome, values_from = value)

##  - for fitting parameter EIR and ITN
JAGSresults_param <- JAGSresults %>%
  dplyr::select(all_of(selectVars)) %>%
  dplyr::filter(outcome == "EIR" | outcome == "ITN") %>%
  tidyr::pivot_wider(names_from = outcome, values_from = value) %>%
  dplyr::select(-year)

JAGSresults_wide <- left_join(JAGSresults_pred, JAGSresults_param)
rm(JAGSresultsCombined, JAGSresults, JAGSresults_pred, JAGSresults_param)


JAGSresults_wide$Cases <- JAGSresults_wide$Severe + JAGSresults_wide$Uncomp


## Add population
TZADistrictDat <- fread(file.path('dat', 'TZADistrictDat.csv')) %>%
  dplyr::select(District, Region, Zone, MIS_UMRC, Strata, Strata_withoutUrban, Population_2016)

JAGSresults_wide <- JAGSresults_wide %>% left_join(TZADistrictDat)
tapply(JAGSresults_wide$Population_2016, JAGSresults_wide$Region, summary)

save(JAGSresults_wide, file = file.path(simout_dir, 'JAGSresults_wide.RData'))
