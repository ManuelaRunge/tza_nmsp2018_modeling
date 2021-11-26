## ===================================================================
## Simulation outputs to analysis dat
## Input:
## - DataForParameterEstimationCombined.RData (before fitting)
## - JAGSresultsCombined.RData (after fitting)
## Output:
## - JAGSresults_wide.RData
## ===================================================================
library(tidyverse)
library(data.table)

source(file.path("rlibrary", "customObjects.R"))
source(file.path("rlibrary", "f_mergevars.R"))
source(file.path("rlibrary", "f_get_scenario_labels.R"))
source(file.path("rlibrary", "f_get_MIS_cm_dat.R"))

simout_dir <- file.path('dat', '03032019_OMStrategicPlanning_resimAll_v2_combined', 'processedExpResults')

## load(file.path(simout_dir, "DataForParameterEstimationCombined.Rdata")) ## BEFORE fitting using JAGS
## See details on model calibration in:
## Runge, M., Snow, R.W., Molteni, F., Thawer, S., Mohamed, A., Mandike, R., Giorgi, E., Macharia, P.M., Smith, T.A., Lengeler, C., Pothin, E., 2020.
## Simulating the council-specific impact of anti-malaria interventions: A tool to support malaria strategic planning in Tanzania.
## PLoS ONE 15, e0228469. https://doi.org/10.1371/journal.pone.0228469

## Attach fitting parameters (e.g. EIR and ITN) to districts for all outcome measures
## transform format (statistics in rows and outcomes in columns instead of other way around)
## if year is NA, it corresponds to the estimated parameter EIR and ITN and not to an predicted outcome
load(file.path(simout_dir, "JAGSresultsCombined.Rdata")) ## AFTER fitting using JAGS
JAGSresults <- JAGSresultsCombined %>%
  mutate(FutScen_nr = as.numeric(FutScen_nr),
         year = ifelse(is.na(year), 0, year)) %>%
  dplyr::select(-NaiveSE, -q75, -q25, -`Time-seriesSE`, -DIS.min_cilu, -DIS.max_ciup, -DISFutScen.min_cilu, -DISFutScen.max_ciup) %>%
  tidyr::gather(statistic, value, q2.5, median, q97.5, mean, sd)

## subset with outcome measures
## for predicted outcomes
selectVars <- c("District", "outcome", "FutScen_nr", "year", "statistic", "value")

JAGSresults_pred <- JAGSresults %>%
  dplyr::select(selectVars) %>%
  dplyr::filter(outcome != "EIR" & outcome != "ITN") %>%
  tidyr::spread(outcome, value)

### for fitting parameter EIR and ITN
JAGSresults_param <- JAGSresults %>%
  dplyr::select(selectVars) %>%
  dplyr::filter(outcome == "EIR" | outcome == "ITN") %>%
  tidyr::spread(outcome, value) %>%
  dplyr::select(-year)

JAGSresults_wide <- f_addVar(JAGSresults_pred, JAGSresults_param)
rm(JAGSresultsCombined, JAGSresults, JAGSresults_pred, JAGSresults_param)

JAGSresults_wide <- JAGSresults_wide %>%
  mutate(Cases = Severe + Uncomp,
         PR = PR * 100)

### add Strata
TZA_dat <- fread(file.path('dat', 'TZA_dat.csv'))
JAGSresults_wide <- f_addVar(JAGSresults_wide, TZA_dat)

JAGSresults_wide$Stratum <- factor(JAGSresults_wide$Stratum,
                                   levels = strata_lbl[1:4], labels = strata_lbl[1:4])
JAGSresults_wide$Stratum_urban <- factor(JAGSresults_wide$Stratum_urban,
                                         levels = strata_lbl, labels = strata_lbl)
table(JAGSresults_wide$Stratum, JAGSresults_wide$Stratum_urban, exclude = NULL)

## Add intervention scenario labels
dat <- f_get_scenario_labels(simout_dir)
JAGSresults_wide <- f_addVar(JAGSresults_wide, dat) %>% as.data.frame()

cmdat <- f_get_MIS_cm_dat()
JAGSresults_wide <- f_addVar(JAGSresults_wide, cmdat) %>%
  mutate(futCMcov=ifelse(CMincrease == "@Access2016@",MISreg_CMeffectiveCoverage_2016/100,0.85))
tapply(JAGSresults_wide$futCMcov,JAGSresults_wide$CMincrease, summary)


## transform to numeric, and divide by 100 if not a proportion
InterventionVars <- colnames(JAGSresults_wide)[grep("cov", colnames(JAGSresults_wide))]
(InterventionVars <- InterventionVars[!(InterventionVars %in% "FutScen_label_cov")])
JAGSresults_wide[InterventionVars] <- apply(JAGSresults_wide[InterventionVars], 2,
                                            function(x) { as.numeric(x) })
JAGSresults_wide[InterventionVars] <- apply(JAGSresults_wide[InterventionVars], 2,
                                            function(x) { ifelse(x > 1, x / 100, x) })
apply(JAGSresults_wide[InterventionVars], 2, unique)

save(JAGSresults_wide, file = file.path('dat', '03032019_OMStrategicPlanning_resimAll_v2_combined', 'processedExpResults', 'JAGSresults_wide.RData'))
