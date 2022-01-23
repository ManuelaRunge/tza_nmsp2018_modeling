cat(paste0('Start running 01_prepare_analysis_dat.R'))

load(file.path(simout_dir, 'JAGSresults_wide.RData'))
str(JAGSresults_wide)

### Subset to future scenarios for AnalysisDat
JAGSresults_wide <- subset(JAGSresults_wide, year >= 2016)

JAGSresults_wide %>%
  filter(statistic == 'median') %>%
  group_by(District, year) %>%
  tally()
summary(JAGSresults_wide$FutScen_nr)
dim(JAGSresults_wide)

### N = 184 per strategy and year, and median statistic
JAGSresults_wide %>%
  filter(statistic == 'median' & year == 2016) %>%
  dplyr::group_by(FutScen_nr) %>%
  tally()


### Add intervention coverage variables
int_cov_dat <- f_get_scenario_labels(simout_dir)
unique(JAGSresults_wide$FutScen_nr)[unique(JAGSresults_wide$FutScen_nr) %in% unique(int_cov_dat$FutScen_nr)]
unique(JAGSresults_wide$FutScen_nr)[!(unique(JAGSresults_wide$FutScen_nr) %in% unique(JAGSresults_wide$FutScen_nr))]
JAGSresults_wide <- left_join(JAGSresults_wide, int_cov_dat)

## Add population
TZADistrictDat <- fread(file.path('dat', 'TZADistrictDat.csv')) %>%
  dplyr::select(-Strata) %>%
  rename(Strata = Stratification.5b,
         Strata_withoutUrban = Stratification.5b_withoutUrban) %>%
  dplyr::select(District, Region, Zone, MIS_UMRC, Strata, Strata_withoutUrban, Population_2016)

JAGSresults_wide <- JAGSresults_wide %>% left_join(TZADistrictDat)
tapply(JAGSresults_wide$Population_2016, JAGSresults_wide$Region, summary)

## Add CM values
cmdat <- f_get_MIS_cm_dat()
JAGSresults_wide <- left_join(JAGSresults_wide, cmdat) %>%
  dplyr::mutate(futCMcov = ifelse(CMincrease == "@Access2016@", MISreg_CMeffectiveCoverage_2016 / 100, 0.85))


# get future scenario number that corresponds to the counterfactual scenario
InterventionVars <- colnames(JAGSresults_wide)[grep("cov", colnames(JAGSresults_wide))]
InterventionVars <- InterventionVars[!(InterventionVars %in% "FutScen_label_cov")]

(FutScen_nrCounterfactual <- JAGSresults_wide %>%
  dplyr::filter(CMincrease == "@Access2016@" & FutureScenarios == "onlyCMandITN") %>%
  dplyr::select(vars = InterventionVars[InterventionVars != "futCMcov"], "FutScen_nr") %>%
  filter_at(vars(-FutScen_nr), all_vars(. == 0)) %>%
  unique() %>%
  dplyr::select(FutScen_nr) %>%
  unique() %>%
  as.numeric())

### add counterfactual variable to dataset used as filter
JAGSresults_wide$counterfactual <- 0
JAGSresults_wide$counterfactual[JAGSresults_wide$FutScen_nr == FutScen_nrCounterfactual] <- 1


## The baseline does not have a 'future scenario number', rather the mean of all future scenarios.
## However for simplification one scenario was picked to represent the baseline in baselineYear (2016).
## Otherwie the mean needs to be calculated and readded to the dataset
JAGSresults_wide$baseline <- 0
JAGSresults_wide$baseline[JAGSresults_wide$FutScen_nr == FutScen_nrCounterfactual & JAGSresults_wide$year == baselineYear] <- 1

### Calculate relative reductions using data.table
### including year as grp ( reduction calculated each year for each council and each statistic)
groupVARS <- c("District", "year", "statistic")
JAGSresults_wide <- as.data.table(JAGSresults_wide, key = groupVARS)

### counterfactual (compared to counterfactual in each year)
JAGSresults_wide[, PRdiff.counterfactual := PR[counterfactual == 1] - PR, by = groupVARS]
JAGSresults_wide[, PRdiff.counterfactual.perc := ((PR[counterfactual == 1] - PR) / PR[counterfactual == 1]) * 100, by = groupVARS]

JAGSresults_wide[, SevereAverted.counterfactual := Severe[counterfactual == 1] - Severe, by = groupVARS]
JAGSresults_wide[, UncompAverted.counterfactual := Uncomp[counterfactual == 1] - Uncomp, by = groupVARS]

JAGSresults_wide[, CasesAverted.counterfactual := Cases[counterfactual == 1] - Cases, by = groupVARS]
JAGSresults_wide[, CasesAverted.counterfactual.perc := ((Cases[counterfactual == 1] - Cases) / Cases[counterfactual == 1]) * 100, by = groupVARS]

if ("allDeaths" %in% colnames(JAGSresults_wide)) {
  JAGSresults_wide[, allDeathsAverted.counterfactual := allDeaths[counterfactual == 1] - allDeaths, by = groupVARS]
  JAGSresults_wide[, allDeathsAverted.counterfactual.perc := ((allDeaths[counterfactual == 1] - allDeaths) / allDeaths[counterfactual == 1]) * 100, by = groupVARS]
}
rm(groupVARS)

### baseline (each year compared to 2016)
groupVARS <- c("District", "statistic")
JAGSresults_wide <- as.data.table(JAGSresults_wide, key = groupVARS)

JAGSresults_wide[, PRdiff.baseline := PR[baseline == 1] - PR, by = groupVARS]
JAGSresults_wide[, PRdiff.baseline.perc := ((PR[baseline == 1] - PR) / PR[baseline == 1]) * 100, by = groupVARS]

JAGSresults_wide[, SevereAverted.baseline := Severe[baseline == 1] - Severe, by = groupVARS]
JAGSresults_wide[, UncompAverted.baseline := Uncomp[baseline == 1] - Uncomp, by = groupVARS]

JAGSresults_wide[, CasesAverted.baseline := Cases[baseline == 1] - Cases, by = groupVARS]
JAGSresults_wide[, CasesAverted.baseline.perc := ((Cases[baseline == 1] - Cases) / Cases[baseline == 1]) * 100, by = groupVARS]

if ("allDeaths" %in% colnames(JAGSresults_wide)) {
  JAGSresults_wide[, allDeathsAverted.baseline := allDeaths[baseline == 1] - allDeaths, by = groupVARS]
  JAGSresults_wide[, allDeathsAverted.baseline.perc := ((allDeaths[baseline == 1] - allDeaths) / allDeaths[baseline == 1]) * 100, by = groupVARS]
}
rm(groupVARS)


#### cases averted per population  - cases averted
print(simPop)
JAGSresults_wide$Cases.Pop <- (JAGSresults_wide$Cases / simPop) * JAGSresults_wide$Population_2016
JAGSresults_wide$CasesAverted.counterfactual.Pop <- (JAGSresults_wide$CasesAverted.counterfactual / simPop) * JAGSresults_wide$Population_2016

## assumption of constant population since baseline year (e.g. 2016)... (To Do, integrate population per year)
JAGSresults_wide$CasesAverted_pP <- JAGSresults_wide$CasesAverted.counterfactual.Pop / JAGSresults_wide$Population_2016

## Cases (not incidence, as not scaled by access to care)
JAGSresults_wide$Cases_p1000 <- (JAGSresults_wide$Cases / simPop) * 1000

### Check calculations
head(JAGSresults_wide)
tapply(JAGSresults_wide$CasesAverted.counterfactual.perc, JAGSresults_wide$baseline, summary)
tapply(JAGSresults_wide$CasesAverted.counterfactual.perc, JAGSresults_wide$FutureScenarios, summary)

tapply(JAGSresults_wide$PRdiff.counterfactual.perc, JAGSresults_wide$FutureScenarios, summary)
tapply(JAGSresults_wide$PRdiff.counterfactual.perc, JAGSresults_wide$counterfactual, summary)
tapply(JAGSresults_wide$PRdiff.counterfactual, JAGSresults_wide$counterfactual, summary)
tapply(JAGSresults_wide$PRdiff.baseline, JAGSresults_wide$year, summary)

##-------------------------------------------
summary(JAGSresults_wide$EIR)
JAGSresults_wide$EIRgrp <- NA
JAGSresults_wide$EIRgrp[JAGSresults_wide$EIR <= 2] <- "<2"
JAGSresults_wide$EIRgrp[JAGSresults_wide$EIR > 2 & JAGSresults_wide$EIR <= 5] <- "<5"
JAGSresults_wide$EIRgrp[JAGSresults_wide$EIR > 5 & JAGSresults_wide$EIR <= 10] <- "5-10"
JAGSresults_wide$EIRgrp[JAGSresults_wide$EIR > 10 & JAGSresults_wide$EIR <= 20] <- "10-20"
JAGSresults_wide$EIRgrp[JAGSresults_wide$EIR > 20 & JAGSresults_wide$EIR <= 60] <- "20-40"
JAGSresults_wide$EIRgrp[JAGSresults_wide$EIR > 60 & JAGSresults_wide$EIR <= 100] <- "40-80"
JAGSresults_wide$EIRgrp[JAGSresults_wide$EIR > 100] <- ">80"
JAGSresults_wide$EIRgrp <- factor(JAGSresults_wide$EIRgrp,
                                  levels = c("<2", "<5", "5-10", "10-20", "20-40", "40-80", ">80"),
                                  labels = c("<2", "<5", "5-10", "10-20", "20-40", "40-80", ">80")
)
table(JAGSresults_wide$EIRgrp)
JAGSresults_wide$EIRgrpLabel <- paste0("EIR ", JAGSresults_wide$EIRgrp)
##-------------------------------------------


AnalysisDat <- JAGSresults_wide
dim(AnalysisDat)
#any(duplicated(AnalysisDat))

save(AnalysisDat, file = file.path("simdat", "AnalysisDat.Rdata"))
rm(JAGSresults_wide, TZADistrictDat, int_cov_dat)

###TODO AnalysisDat2
