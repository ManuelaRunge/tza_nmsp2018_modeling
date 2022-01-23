cat(paste0('Start running Figure3.R'))

selectedOutcome <- "PR"  # "Cases.pP"

load(file.path("simdat", "AnalysisDat.Rdata"))
AnalysisDat <- AnalysisDat %>% filter((year == 2022 | year == 2020 | year == 2016))
str(AnalysisDat)

FutNrDat <- AnalysisDat %>%
  filter(FutureScenarios == "onlyCMandITN" &
           IPTcov == 0 &
           futIRScov == 0) %>%
  dplyr::select(FutScen_nr, FutScen_label) %>%
  unique()

### generate variables for nets, regardless of other interventions
tempdat <- AnalysisDat %>%
  ungroup() %>%
  group_by(Strata) %>%
  filter(FutScen_nr %in% FutNrDat$FutScen_nr &
           FutScen_label != "ITN MRC continuous")

table(tempdat$FutScen_label, tempdat$Strata)
tempdat$withWithoutNets <- "CM only"
tempdat$withWithoutNets[grep("ITN", tempdat$FutScen_label)] <- "+ ITNs"
tempdat$withWithoutNets <- factor(tempdat$withWithoutNets,
                                  levels = c("CM only", "+ ITNs"),
                                  labels = c("CM only", "+ ITNs"))

### aggregate LLIN group - take mean --- or generate ranges across all LLIN scenarios?
groupVARS <- c("District", "statistic", "Strata", "Population_2016", "withWithoutNets", "year")
tempdatForDiff <- tempdat %>%
  dplyr::group_by_at(.vars = groupVARS) %>%
  dplyr::summarize(PR = mean(PR))

## calculate difference in CM vs CM+LLIN
groupVARS <- c("District", "statistic", "Strata", "Population_2016", "year")
tempdatForDiff <- data.table(tempdatForDiff, key = groupVARS)
tempdatForDiff[, CMITNdiff := PR[withWithoutNets == "CM only"] - PR[withWithoutNets == "+ ITNs"], by = groupVARS]
tempdatForDiff[, CMITNdiff_perc := ((PR[withWithoutNets == "CM only"] - PR[withWithoutNets == "+ ITNs"]) / PR[withWithoutNets == "CM only"]) * 100, by = groupVARS]

## transform back to data frame and then aggregate from council to strata
groups <- c("statistic", "Strata", "withWithoutNets", "year")
tempdatForDiffAggr <- tempdatForDiff %>%
  filter(Strata %in% c("very low", "low", "urban")) %>%
  f_weighted.aggrDat(groupVars = groups, valueVar = "CMITNdiff", weightVar = "Population_2016", WideToLong = FALSE)

#tempdatForDiffAggr %>% filter(year == 2020)
table(tempdat$District, tempdat$withWithoutNets)

### add n districts
nDisSTrata <- tempdat %>%
  dplyr::select(District, Strata) %>%
  unique() %>%
  dplyr::group_by(Strata) %>%
  tally()
sum(nDisSTrata$n)

### Creae label variable
tempdat$StrataRev_adj <- as.character(tempdat$Strata)
tempdat$StrataRev_adj <- factor(tempdat$StrataRev_adj,
                                levels = c("very low", "low", "urban"),
                                labels = c(
                                  paste0("very low\n(n=", nDisSTrata$n[nDisSTrata$Strata == "very low"], ")"),
                                  paste0("low\n(n=", nDisSTrata$n[nDisSTrata$Strata == "low"], ")"),
                                  paste0("urban\n(n=", nDisSTrata$n[nDisSTrata$Strata == "urban"], ")")
                                )
)

#### Aggregate
if (selectedOutcome == "PR") selectedOutcomeLabel <- expression(italic("PfPR")["2 to 10"] * " (%)")
if (selectedOutcome == "Cases.pP") selectedOutcomeLabel <- "Cases pP"

groups <- c("statistic", "Strata", "Strata_withoutUrban", "StrataRev_adj", "withWithoutNets", "year")

plotdat <- tempdat %>%
  filter(statistic == "median" & Strata %in% c("very low", "low", "urban")) %>%
  aggregatDat(groupVars = groups, valueVar = selectedOutcome, weightVar = "Population_2016", weightedAggr = weightedAggr)


plotdat$withWithoutNets_year <- paste0(plotdat$withWithoutNets, ' ', plotdat$year)
withWithoutNets_year_cols <- c("CM only 2016" = "#7FD8F6",
                               "CM only 2020" = "#00B2EE",
                               "CM only 2022" = "#007CA6",
                               "+ ITNs 2016" = "#F4AC66",
                               "+ ITNs 2020" = "#EE7600",
                               "+ ITNs 2022" = "#BE5E00")

pplotSmall <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  geom_bar(data = subset(plotdat, statistic == "median"),
           aes(x = withWithoutNets, y = mean.val, fill = withWithoutNets_year, group = year),
           stat = "identity", position = position_dodge(width = 0.5), width = 0.5, size = 0.7) +
  geom_errorbar(data = subset(plotdat, statistic == "median"),
                aes(x = withWithoutNets, ymin = lower.ci.val, ymax = upper.ci.val, group = year),
                stat = "identity", position = position_dodge(width = 0.5), width = 0.5, size = 0.7, col = "black") +
  scale_fill_manual(values = withWithoutNets_year_cols) +
  labs(y = selectedOutcomeLabel, colour = "", fill = "", x = "") +
  labs(title = "A)", subtitle = "") +
  theme(legend.position = "right") +
  facet_wrap(~StrataRev_adj, scales = "free") +
  customTheme_noAngle

### add n districts
### very low risk strata by EIR
tempdat$EIRgrp <- cut(tempdat$EIR, c(-Inf, 2, 5, 10, 20, 40, 80, Inf),
                      labels = c("EIR <2", "EIR 2-5", "EIR 5-10", "EIR 10-20", "EIR 20-40", "EIR 40-80", "EIR >80"))

tempdat$EIRgrp_adj <- cut(tempdat$EIR, c(-Inf, 2, 5, 10, Inf),
                          labels = c("EIR <2", "EIR 2-5", "EIR 5-10", "EIR > 10"))

counterfactualInterventions <- tempdat %>%
  ungroup() %>%
  dplyr::filter(CMincrease == "@Access2016@" &
                  futITNcov == 0 &
                  futSNPcov == 0 &
                  futIRScov == 0 &
                  IPTcov == 0 &
                  FutureScenarios == "onlyCMandITN") %>%
  dplyr::select(FutScen_nr) %>%
  unique()
(counterfactualInterventions <- counterfactualInterventions$FutScen_nr)

d <- tempdat %>%
  ungroup() %>%
  filter(year == 2016 &
           FutScen_nr == counterfactualInterventions &
           Strata_withoutUrban == "Very low") %>%
  dplyr::select(FutScen_nr, District, year, Strata_withoutUrban, EIRgrp_adj) %>%
  unique() %>%
  group_by(FutScen_nr, Strata_withoutUrban, year, EIRgrp_adj) %>%
  arrange(FutScen_nr, Strata_withoutUrban, EIRgrp_adj) %>%
  tally()

tempdat$EIRgrp_adj <- factor(tempdat$EIRgrp_adj,
                             levels = c("EIR <2", "EIR 2-5", "EIR 5-10", "EIR > 10"),
                             labels = c(
                               paste0("EIR <2\n(n=", d$n[d$EIRgrp_adj == "EIR <2"], ")"),
                               paste0("EIR 2-5\n(n=", d$n[d$EIRgrp_adj == "EIR 2-5"], ")"),
                               paste0("EIR 5-10\n(n=", d$n[d$EIRgrp_adj == "EIR 5-10"], ")"),
                               paste0("EIR > 10\n(n=", d$n[d$EIRgrp_adj == "EIR > 10"], ")")
                             )
)

table(tempdat$EIRgrp_adj, tempdat$EIRgrp, exclude = NULL)


#### Prepare main plot - show very low only
table(tempdat$withWithoutNets, tempdat$Strata_withoutUrban)
groups <- c("Strata_withoutUrban", "EIRgrp_adj", "withWithoutNets", "year")

### spread statistic
tempdat_stat <- tempdat %>%
  ungroup() %>%
  dplyr::select(EIR, year, Strata_withoutUrban, FutScen_nr, District, withWithoutNets, statistic, PR) %>%
  pivot_wider(names_from = statistic, values_from = c("PR", "EIR"))

pplotMain <- ggplot(
  data = subset(tempdat_stat, Strata_withoutUrban == "Very low"),
  aes(x = EIR_median, y = PR_median,
      ymin = PR_q2.5, ymax = PR_q97.5,
      xmin = EIR_q2.5, xmax = EIR_q97.5,
      col = as.factor(withWithoutNets),
      fill = as.factor(withWithoutNets))) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  geom_errorbar(size = 0.7, alpha = 0.5) +
  geom_errorbarh(size = 0.7, alpha = 0.5) +
  geom_point(shape = 21, size = 2, col = "black", alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3, raw = TRUE), show.legend = FALSE, se = FALSE) +
  facet_wrap(~year, nrow = 1, scales = "free_y") +
  scale_x_continuous(trans = "log", breaks = c(1:5, 10, 20), labels = c(1:5, 10, 20)) +
  scale_y_continuous(breaks = seq(0, 20, 2), labels = seq(0, 20, 2)) +
  scale_color_manual(values = TwoCols) +
  scale_fill_manual(values = TwoCols) +
  labs(y = selectedOutcomeLabel, colour = "", fill = "", x = "",
       title = "B)", subtitle = "", x = "pre-intervention EIR (log-scale)") +
  theme(legend.position = "right") +
  customTheme_noAngle

(Figure3 <- plot_grid(pplotSmall, pplotMain, nrow = 2))

ggsave(paste0("Fig_3.png"), plot = Figure3, path = file.path("figures"), width = 13, height = 8, dpi = 200, device = "png")
#ggsave(paste0("Fig_3.pdf"), plot = Figure3, path = file.path("figures"), width = 13, height = 8, dpi = 200, device = "pdf")


### Save datasetss
fwrite(plotdat, file = file.path("figures", 'figuredat', paste0("Fig3_pplotSmall_", selectedOutcome, ".csv")))
fwrite(tempdat, file = file.path("figures", 'figuredat', paste0("Fig3_tempdat_", selectedOutcome, ".csv")))
fwrite(tempdat_stat, file = file.path("figures", 'figuredat', paste0("Fig3_tempdat_stat", selectedOutcome, ".csv")))


#### For text
dat <- plotdat %>%
  ungroup() %>%
  dplyr::filter(statistic == "median") %>%
  dplyr::select(Strata, Strata_withoutUrban, withWithoutNets, year, mean.val, lower.ci.val, upper.ci.val, weighted)

dat_2016 <- dat %>% filter(year == 2016) %>% dplyr::select(-year)
dat_2020 <- dat %>% filter(year == 2020) %>% dplyr::select(-year)
dat_2022 <- dat %>% filter(year == 2022) %>% dplyr::select(-year)

colnames(dat_2016)[c(3:5)] <- paste0(colnames(dat_2016)[c(3:5)], "_2016")
colnames(dat_2020)[c(3:5)] <- paste0(colnames(dat_2020)[c(3:5)], "_2020")
colnames(dat_2022)[c(3:5)] <- paste0(colnames(dat_2022)[c(3:5)], "_2022")

dat <- dat_2016 %>%
  left_join(dat_2020) %>%
  left_join(dat_2022)

fwrite(dat, file.path('figures', 'figuredat', 'Fig3A_fortext.csv'))


