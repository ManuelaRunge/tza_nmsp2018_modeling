cat(paste0('Start running Figure5.R'))

load(file.path("simdat", "AnalysisDat.RData"))

## IPT scenarios
IPTScinterventions <- AnalysisDat %>%
  dplyr::filter(
    (CMincrease == "@Access2016@" &
      futITNcov == 0 &
      futSNPcov == 0 &
      futIRScov != 0 &
      FutureScenarios == "onlyCMandITN") |
      (CMincrease == "@Access2016@" &
        futITNcov != 0 &
        futSNPcov == 0 &
        futIRScov == 0 &
        FutureScenarios == "onlyCMandITN") |
      (CMincrease == "@Access2016@" &
        futITNcov == 0 &
        futSNPcov != 0 &
        futIRScov == 0 &
        FutureScenarios == "onlyCMandITN") |
      (CMincrease == "@Access2016@" &
        futITNcov != 0 &
        futSNPcov != 0 &
        futIRScov == 0 &
        FutureScenarios == "onlyCMandITN")
  ) %>%
  dplyr::select(FutScen) %>%
  unique()
(PTScinterventions <- IPTScinterventions$FutScen)

#### Create IPTsc plot (in (moderate and) high stratum only)
### Get relevant future scenarios
FutNrDat <- AnalysisDat %>%
  filter(FutureScenarios == "onlyCMandITN" &
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
                                        gsub(" & no IRS", "",
                                             AnalysisDat$IPTscplotlabel)))

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

AnalysisDat <- AnalysisDat %>%
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


### Calculate difference between improved CM and not improved CM across the other interventions using data.table
## IPTsc difference, regardless of CM
groupVars <- c("Strata", "District", "IPTscplotlabel", "year", "futCMlabel", "CMincrease", "outcome")
AnalysisDat <- as.data.table(AnalysisDat, key = groupVars)
AnalysisDat[, IPTscdiff := value[IPTcov == 0] - value[IPTcov == 0.8], by = groupVars]
AnalysisDat[, IPTscdiff_perc := ((value[IPTcov == 0] - value[IPTcov == 0.8]) / value[IPTcov == 0]) * 100, by = groupVars]

### Calculate difference between IPTsc and no IPTsc across the other interventions using data.table
## CM difference, regardless of IPTsc
groupVars <- c("Strata", "District", "IPTscplotlabel", "year", "IPTcov", "outcome")
AnalysisDat <- as.data.table(AnalysisDat, key = groupVars)
AnalysisDat[, CMdiff := value[futCMlabel == "current case management"] - value[futCMlabel != "current case management"], by = groupVars]
AnalysisDat[, CMdiff_perc := ((value[futCMlabel == "current case management"] - value[futCMlabel != "current case management"]) / value[futCMlabel == "current case management"]) * 100, by = groupVars]

AnalysisDat <- AnalysisDat %>% filter(year == 2020)

### Wide to long format for plotting
AnalysisDat_long <- AnalysisDat %>%
  dplyr::select(District, Population_2016, year, IPTscplotlabel2, outcome, CMdiff_perc, IPTscdiff_perc) %>%
  pivot_longer(cols = -c(District, Population_2016, year, IPTscplotlabel2, outcome), names_to = 'variable')

AnalysisDat_long$variable[AnalysisDat_long$variable == "CMdiff_perc"] <- "improved CM"
AnalysisDat_long$variable[AnalysisDat_long$variable == "IPTscdiff_perc"] <- "IPTsc"
table(AnalysisDat_long$variable)

### Create plot
pplot <- ggplot(data = AnalysisDat_long) +
  geom_hline(yintercept = c(20, 40, 60, 80), color = "grey") +
  geom_hline(yintercept = c(0)) +
  geom_boxplot(aes(x = IPTscplotlabel2, y = value, fill = variable), width = 0.7) +
  scale_fill_manual(values = TwoCols) +
  facet_wrap(~outcome, nrow = 2) +
  theme(strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 1)) +
  scale_y_continuous(lim = c(-20, 81), breaks = seq(0, 90, 20)) +
  theme(legend.position = "right") +
  labs(y = "reduction (%)", fill = "", x = "", title = "")


ggsave("Fig_5.png", plot = pplot, path = file.path("figures"), width = 15, height = 8, device = "png")  ## previously saved Figure6_new
if (SAVEpdf)ggsave("Fig_5.pdf", plot = pplot, path = file.path("figures"), width = 15, height = 8, device = "pdf")  ## previously saved Figure6_new

## For text
AnalysisDat_long %>%
  aggregatDat(groupVars = c("variable", "outcome"), valueVar = "value", weightVar = "Population_2016", weightedAggr = T) %>%
  fwrite(file.path("figures", 'figuredat', paste0("Fig4_AnalysisDat_long_weighted.csv")))

AnalysisDat_long %>%
  aggregatDat(groupVars = c("variable", "outcome"), valueVar = "value", weightVar = "Population_2016", weightedAggr = F) %>%
  fwrite(file.path("figures", 'figuredat', paste0("Fig4_AnalysisDat_long_unweighted.csv")))


groupVars <- c("Strata", "IPTscplotlabel", "year", "FutScen_nr", "futCMlabel", "IPTcov", "CMincrease", "outcome")
subdatAggrStrat <- AnalysisDat %>%
  filter(!(outcome %in% c("Cases.Pop", "Cases.pP"))) %>%
  aggregatDat(groupVars, "value", "Population_2016", weightedAggr = weightedAggr)

groupVars <- c("IPTscplotlabel", "year", "FutScen_nr", "futCMlabel", "IPTcov", "CMincrease", "outcome")
subdatAggr <- AnalysisDat %>%
  filter(!(outcome %in% c("Cases.Pop", "Cases.pP"))) %>%
  aggregatDat(groupVars, "value", "Population_2016", weightedAggr = weightedAggr)


groupVars <- c("IPTscplotlabel", "year", "futCMlabel", "CMincrease", "outcome")
subdatAggrIPTsc <- AnalysisDat %>%
  filter(!(outcome %in% c("Cases.Pop", "Cases.pP"))) %>%
  aggregatDat(groupVars, "IPTscdiff", "Population_2016", weightedAggr = weightedAggr)


### Save datasetss
fwrite(AnalysisDat_long, file = file.path("figures", 'figuredat', paste0("Fig5.csv")))
fwrite(subdatAggrStrat, file = file.path("figures", 'figuredat', paste0("Fig5_pplotMain_aggrStrat.csv")))
fwrite(subdatAggr, file = file.path("figures", 'figuredat', paste0("Fig5_pplotMain_aggr.csv")))
fwrite(subdatAggrIPTsc, file = file.path("figures", 'figuredat', paste0("Fig5_pplotMain_IPTdiff.csv")))

rm(pplot, AnalysisDat, groupVars, AnalysisDat_long, subdatAggrStrat, subdatAggr, subdatAggrIPTsc)