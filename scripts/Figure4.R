## -----------------------------------------
#### Figure 4 - IPTsc in high (and moderate) strata
## -----------------------------------------

require(data.table)
require(tidyverse)
require(cowplot)

library(spatstat)
source(file.path("rlibrary", "f_spread.R"))
source(file.path("rlibrary", "f_aggrDat.R"))

theme_set(theme_cowplot())
fig5cols <- c('#00B2EE', '#8DC63F', '#EE7600', '#C53F42', '#628A2C', '#C38B4B', '#614525', '#9D3234')
simPop = 10000
TwoCols<- c("deepskyblue2", "darkorange2")
load(file.path("simdat", "AnalysisDat.RData"))

## IPT scenarios
IPTScinterventions <- AnalysisDat %>%
  dplyr::filter(
    CMincrease == "@Access2016@" &
      futITNcov == 0 &
      futSNPcov == 0 &
      futIRScov != 0 &
      IPTcov == 0 &
      FutureScenarios == "onlyCMandITN" |
      CMincrease == "@Access2016@" &
        futITNcov != 0 &
        futSNPcov == 0 &
        futIRScov == 0 &
        IPTcov == 0 &
        FutureScenarios == "onlyCMandITN" |
      CMincrease == "@Access2016@" &
        futITNcov == 0 &
        futSNPcov != 0 &
        futIRScov == 0 &
        IPTcov == 0 &
        FutureScenarios == "onlyCMandITN" |
      CMincrease == "@Access2016@" &
        futITNcov == 0 &
        futSNPcov == 0 &
        futIRScov != 0 &
        IPTcov != 0 &
        FutureScenarios == "onlyCMandITN" |
      CMincrease == "@Access2016@" &
        futITNcov != 0 &
        futSNPcov == 0 &
        futIRScov == 0 &
        IPTcov != 0 &
        FutureScenarios == "onlyCMandITN" |
      CMincrease == "@Access2016@" &
        futITNcov == 0 &
        futSNPcov != 0 &
        futIRScov == 0 &
        IPTcov != 0 &
        FutureScenarios == "onlyCMandITN" |
      CMincrease == "@Access2016@" &
        futITNcov != 0 &
        futSNPcov != 0 &
        futIRScov == 0 &
        IPTcov != 0 &
        FutureScenarios == "onlyCMandITN" |
      CMincrease == "@Access2016@" &
        futITNcov != 0 &
        futSNPcov != 0 &
        futIRScov == 0 &
        IPTcov == 0 &
        FutureScenarios == "onlyCMandITN"
  ) %>%
  dplyr::select(FutScen) %>%
  unique()
IPTScinterventions <- IPTScinterventions$FutScen

#### Create IPTsc plot (in (moderate and) high stratum only)
### Get relevant future scenarios
FutNrDat <- AnalysisDat %>%
  filter(
    FutureScenarios == "onlyCMandITN" &
      futITNcov != 0.5 &
      futSNPcov != 0.4 &
      (futIRScov == 0 | (futIRScov != 0 & futSNPcov != 0 & futITNcov != 0)) &
      ((CMincrease == "no increase in CM" & (futSNPcov != 0 | futITNcov != 0)) |
        (CMincrease != "no increase in CM" & (futSNPcov != 0 | futITNcov != 0)))
  ) %>%
  dplyr::select(FutScen_nr, FutScen_label) %>%
  unique()

AnalysisDat$Cases.pP <- AnalysisDat$Cases / simPop
AnalysisDat$Incidence <- (AnalysisDat$Cases / simPop) * 1000

### create label
AnalysisDat$IPTscplotlabel <- paste0(AnalysisDat$futITNdep, " & ", AnalysisDat$futIRSlabel)
AnalysisDat$IPTscplotlabel <- gsub(" and ", "\n+",
                                   gsub(" & ", "\n+",
                                        gsub(" & no IRS", "", AnalysisDat$IPTscplotlabel)))

AnalysisDat$IPTscplotlabel2 <- factor(AnalysisDat$IPTscplotlabel,
                                      levels = c("no ITN",
                                                 "no ITN\n+IRS",
                                                 "continuous",
                                                 "mass-campaign",
                                                 "both",
                                                 "continuous\n+IRS",
                                                 "mass-campaign\n+IRS",
                                                 "both\n+IRS"),
                                      labels = c("none",
                                                 "+IRS",
                                                 "ITN continuous",
                                                 "ITN mass-campaign",
                                                 "ITN mass-campaign\n+continuous",
                                                 "ITN continuous\n+IRS",
                                                 "ITN mass-campaign\n+IRS",
                                                 "ITN mass-campaign\n+continuous\n+IRS")
)

table(AnalysisDat$IPTscplotlabel2)

# FutNr <- selectedFutscenNr - !(District %in% missingIPTscDistricts),
subdat <- AnalysisDat %>%
  filter(Strata %in% c("moderate", "high") &
           year >= 2016 &
           year <= 2020 &
           FutScen_nr %in% FutNrDat$FutScen_nr &
           statistic == "median") %>%
  dplyr::select(District, Population_2016, Strata, IPTscplotlabel2, IPTscplotlabel,
                year, FutScen_nr, futCMlabel, IPTcov, CMincrease, PR, Cases.pP) %>%
  unique() %>%
  gather("outcome", "value", -c(District, Population_2016, Strata, IPTscplotlabel2,
                                IPTscplotlabel, year, FutScen_nr, futCMlabel, IPTcov, CMincrease))
### not aggregated but using geom_smooth in plot (loess regression function)

### add n districts!!
subdat$Strata_adj <- as.character(subdat$Strata)
subdat$Strata_adj <- factor(subdat$Strata_adj,
                            levels = c("moderate", "high"),
                            labels = c("moderate\n(n=41)", "high\n(n=69)")
)

subdat$outcomen <- factor(subdat$outcome,
                          levels = c("Incidence", "Cases.Pop", "Cases.pP", "PR"),
                          labels = c(
                            '"Incidence"', '"Total cases"',
                            '"Cases per person"',
                            'italic("PfPR")["2 to 10"]'
                          )
)


### Calculate difference between improved CM and not improved CM across the other interventions using data.table
groupVars <- c("Strata", "District", "IPTscplotlabel", "year", "futCMlabel", "CMincrease", "outcome", "Strata_adj")
subdat <- as.data.table(subdat, key = groupVars)

## IPTsc difference, regardless of CM
subdat[, IPTscdiff := value[IPTcov == 0] - value[IPTcov == 0.8], by = groupVars]
subdat[, IPTscdiff_perc := ((value[IPTcov == 0] - value[IPTcov == 0.8]) / value[IPTcov == 0]) * 100, by = groupVars]


### Calculate difference between IPTsc and no IPTsc across the other interventions using data.table
groupVars <- c("Strata", "District", "IPTscplotlabel", "year", "IPTcov", "outcome", "Strata_adj")
subdat <- as.data.table(subdat, key = groupVars)

## CM difference, regardless of IPTsc
subdat[, CMdiff := value[futCMlabel == "current case management"] - value[futCMlabel != "current case management"], by = groupVars]
subdat[, CMdiff_perc := ((value[futCMlabel == "current case management"] - value[futCMlabel != "current case management"]) / value[futCMlabel == "current case management"]) * 100, by = groupVars]

### View calculated differences
testdat <- subdat %>% filter(year == 2020)
tapply(testdat$CMdiff, testdat$outcome, summary)
tapply(testdat$IPTscdiff, testdat$outcome, summary)
tapply(testdat$CMdiff_perc, testdat$outcome, summary)
tapply(testdat$IPTscdiff_perc, testdat$outcome, summary)

### Wide to long format for plotting
testdat_long <- testdat %>%
  dplyr::select(IPTscplotlabel2, outcome, CMdiff_perc, IPTscdiff_perc) %>%
  melt(id.vars = c("outcome", "IPTscplotlabel2"))

testdat_long$variable <- as.character(testdat_long$variable)
testdat_long$variable[testdat_long$variable == "CMdiff_perc"] <- "improved CM"
testdat_long$variable[testdat_long$variable == "IPTscdiff_perc"] <- "IPTsc"
table(testdat_long$variable)

### Create plot
fig6_new <- ggplot(data = testdat_long) +
  theme_cowplot() +
  geom_hline(yintercept = c(20, 40, 60, 80), color = "grey") +
  geom_hline(yintercept = c(0)) +
  geom_boxplot(aes(x = IPTscplotlabel2, y = value, fill = variable), width = 0.7) +
  scale_fill_manual(values = TwoCols) +
  facet_wrap(~outcome, nrow = 2) +
  theme(
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 16),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 1)
  ) +
  scale_y_continuous(lim = c(-20, 81), breaks = seq(0, 90, 20)) +
  theme(legend.position = "right") +
  labs(y = "reduction (%)", fill = "", x = "", title = "")


  ggsave("Figure4.png", plot = fig6_new, path = file.path("figures"), width = 15, height = 8, device = "png")  ## previously saved Figure6_new
  ggsave("Figure4.pdf", plot = fig6_new, path =  file.path("figures"), width = 15, height = 8, device = "pdf")  ## previously saved Figure6_new

## add plot with statistics
#source("addStats_explore.R")
table(testdat_long$variable)

groupVars <- c("variable","outcome")
testdat_long %>%
  f_aggrDat(groupVars, "value")



### adjust labels
#testdat$futCMlabel[testdat$futCMlabel == "current case management"] <- "+ IPTsc"
#testdat$futCMlabel[testdat$futCMlabel == "85% treatment of cases"] <- "+ IPTsc\n+CM"
### Aggregate data per strata and at national level for easier reporting of average estimates
groupVars <- c("Strata", "IPTscplotlabel", "year", "FutScen_nr", "futCMlabel", "IPTcov", "CMincrease", "outcome", "Strata_adj")
subdatAggrStrat <- subdat %>%
  filter(!(outcome %in% c("Cases.Pop", "Cases.pP"))) %>%
  f_weighted.aggrDat(groupVars, "value", "Population_2016")

groupVars <- c("IPTscplotlabel", "year", "FutScen_nr", "futCMlabel", "IPTcov", "CMincrease", "outcome")
subdatAggr <- subdat %>%
  filter(!(outcome %in% c("Cases.Pop", "Cases.pP"))) %>%
  f_weighted.aggrDat(groupVars, "value", "Population_2016")


groupVars <- c("IPTscplotlabel", "year", "futCMlabel", "CMincrease", "outcome")
subdatAggrIPTsc <- subdat %>%
  filter(!(outcome %in% c("Cases.pP"))) %>%
  f_weighted.aggrDat(groupVars, "IPTscdiff", "Population_2016")


### Save excel files
fwrite(testdat, file = file.path( "figures", paste0("pplotMain_line_2020.csv")))
fwrite(testdat_long, file = file.path( "figures", paste0("pplotMain_box_2020.csv")))
fwrite(subdatAggrStrat, file = file.path( "figures", paste0("pplotMain_aggrStrat.csv")))
fwrite(subdatAggr, file = file.path( "figures", paste0("pplotMain_aggr.csv")))
fwrite(subdatAggrIPTsc, file = file.path( "figures", paste0("pplotMain_IPTdiff.csv")))

