## ===================================================================
## Figure 3
## ===================================================================
library(data.table)
library(tidyverse)
library(spatstat) #weighted.median

source(file.path("rlibrary", "customObjects.R"))
source(file.path("rlibrary", "f_AggrDat.R"))
source(file.path("rlibrary", "f_spread.R"))

selectedOutcome <- "PR"  # "Cases.pP"

load(file.path("dat", "AnalysisDat.Rdata"))
AnalysisDat <- AnalysisDat %>% filter((year == 2022 | year == 2020 | year == 2016))

FutNrDat <- AnalysisDat %>%
  filter(FutureScenarios == "onlyCMandITN" &
           IPTcov == 0 &
           futIRScov == 0) %>%
  dplyr::select(FutScen_nr, FutScen_label) %>%
  unique()

### generate variables for nets, regardless of other interventions
tempdat <- AnalysisDat %>%
  ungroup() %>%
  group_by(StrataLabel) %>%
  filter(FutScen_nr %in% FutNrDat$FutScen_nr &
           FutScen_label != "ITN MRC continuous")

table(tempdat$FutScen_label, tempdat$StrataLabel)
### add label variable
tempdat$withWithoutNets <- "CM only"
tempdat$withWithoutNets[grep("ITN", tempdat$FutScen_label)] <- "+ ITNs"
tempdat$withWithoutNets <- factor(tempdat$withWithoutNets,
                                  levels = c("CM only", "+ ITNs"),
                                  labels = c("CM only", "+ ITNs"))

### aggregate LLIN group - take mean --- or generate ranges across all LLIN scenarios?
groupVARS <- c("District", "statistic", "Stratification.5b", "Population_2016", "StrataLabel", "withWithoutNets", "year")
tempdatForDiff <- tempdat %>%
  dplyr::group_by_at(.vars = groupVARS) %>%
  dplyr::summarize(PR = mean(PR))

## calculate difference in CM vs CM+LLIN
groupVARS <- c("District", "statistic", "Stratification.5b", "Population_2016", "StrataLabel", "year")
tempdatForDiff <- data.table(tempdatForDiff, key = groupVARS)
tempdatForDiff[, CMITNdiff := PR[withWithoutNets == "CM only"] - PR[withWithoutNets == "+ ITNs"], by = groupVARS]
tempdatForDiff[, CMITNdiff_perc := ((PR[withWithoutNets == "CM only"] - PR[withWithoutNets == "+ ITNs"]) / PR[withWithoutNets == "CM only"]) * 100, by = groupVARS]

## transform back to data frame and then aggregate from council to strata
groups <- c("statistic", "Stratification.5b", "StrataLabel", "withWithoutNets", "year")
tempdatForDiffAggr <- tempdatForDiff %>%
  filter(StrataLabel %in% c("very low", "low", "urban")) %>%
  f_weighted.aggrDat(groupVars = groups, valueVar = "CMITNdiff", weightVar = "Population_2016", WideToLong = FALSE)

tempdatForDiffAggr %>% filter(year == 2020)
table(tempdat$District, tempdat$withWithoutNets)

### add n districts
nDisSTrata <- tempdat %>%
  dplyr::select(District, StrataLabel) %>%
  unique() %>%
  dplyr::group_by(StrataLabel) %>%
  tally()
sum(nDisSTrata$n)

### Creae label variable
tempdat$StrataRev_adj <- as.character(tempdat$Stratification.5b)
tempdat$StrataRev_adj <- factor(tempdat$StrataRev_adj,
                                levels = c("very low", "low", "urban"),
                                labels = c(
                                  paste0("very low\n(n=", nDisSTrata$n[nDisSTrata$StrataLabel == "very low"], ")"),
                                  paste0("low\n(n=", nDisSTrata$n[nDisSTrata$StrataLabel == "low"], ")"),
                                  paste0("urban\n(n=", nDisSTrata$n[nDisSTrata$StrataLabel == "urban"], ")")
                                )
)

#### Aggregate
if (selectedOutcome == "PR") selectedOutcomeLabel <- expression(italic("PfPR")["2 to 10"] * " (%)")
if (selectedOutcome == "Cases.pP") selectedOutcomeLabel <- "Cases pP"

groups <- c("statistic", "Stratification.5b", "StrataRev_adj", "withWithoutNets", "year")
plotdat <- tempdat %>%
  filter(statistic == "median" & StrataLabel %in% c("very low", "low", "urban")) %>%
  f_weighted.aggrDat(groups, selectedOutcome, "Population_2016", FALSE)

plotdat_unw <- tempdat %>%
  filter(statistic == "median" &
           StrataLabel %in% c("very low", "low", "urban")) %>%
  f_aggrDat(groups, selectedOutcome, FALSE)

plotdat_unw$withWithoutNets_year <- paste0(plotdat_unw$withWithoutNets, ' ', plotdat_unw$year)

withWithoutNets_year_cols <- c("CM only 2016" = "#7FD8F6",
                               "CM only 2020" = "#00B2EE",
                               "CM only 2022" = "#007CA6",
                               "+ ITNs 2016" = "#F4AC66",
                               "+ ITNs 2020" = "#EE7600",
                               "+ ITNs 2022" = "#BE5E00")

pplotSmall <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  geom_bar(data = subset(plotdat_unw, statistic == "median"),
           aes(x = withWithoutNets, y = mean.val, fill = withWithoutNets_year, group = year),
           stat = "identity", position = position_dodge(width = 0.5), width = 0.5, size = 0.7) +
  geom_errorbar(data = subset(plotdat_unw, statistic == "median"),
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
  filter(year == 2016 &
           FutScen_nr == counterfactualInterventions &
           Stratification.5b_withoutUrban == "Very low") %>%
  dplyr::select(FutScen_nr, District, year, Stratification.5b_withoutUrban, EIRgrp_adj) %>%
  unique() %>%
  group_by(FutScen_nr, Stratification.5b_withoutUrban, year, EIRgrp_adj) %>%
  arrange(FutScen_nr, Stratification.5b_withoutUrban, EIRgrp_adj) %>%
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
table(tempdat$withWithoutNets, tempdat$Stratification.5b_withoutUrban)
groups <- c("Stratification.5b_withoutUrban", "EIRgrp_adj", "withWithoutNets", "year")
plotdat2 <- tempdat %>%
  f_weighted.aggrDat(groups, selectedOutcome, "Population_2016", FALSE) %>%
  filter(Stratification.5b_withoutUrban == "Very low")
summary(plotdat2$n.val)

## aggregate ITN grp
groups <- c("District", "Stratification.5b_withoutUrban", "EIRgrp_adj", "withWithoutNets", "year")
plotdat2a <- tempdat %>%
  f_weighted.aggrDat(groupVars = groups, valueVar = selectedOutcome, weightVar = "Population_2016", WideToLong = FALSE) %>%
  filter(Stratification.5b_withoutUrban == "Very low")
summary(plotdat2a$n.val)

### spread statistic
tempdat_stat <- tempdat %>%
  ungroup() %>%
  dplyr::select(EIR, year, Stratification.5b_withoutUrban, FutScen_nr, District, withWithoutNets, statistic, PR) %>%
  pivot_wider(names_from=statistic,values_from= c("PR", "EIR"))

pplotMain <- ggplot(
  data = subset(tempdat_stat, Stratification.5b_withoutUrban == "Very low"),
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
fwrite(plotdat2, file = file.path("figures", 'figuredat', paste0("Fig3_pplotMain_", selectedOutcome, ".csv")))
fwrite(plotdat, file = file.path("figures", 'figuredat', paste0("Fig3_pplotSmall_", selectedOutcome, ".csv")))
fwrite(tempdat, file = file.path("figures", 'figuredat', paste0("Fig3_tempdat_", selectedOutcome, ".csv")))


#### For text
dat <- plotdat_unw %>%
  ungroup() %>%
  dplyr::filter(statistic == "median") %>%
  dplyr::select(Stratification.5b, withWithoutNets, year, mean.val, lower.ci.val, upper.ci.val)

dat_2016 <- dat %>% filter(year == 2016) %>% select(-year)
dat_2020 <- dat %>% filter(year == 2020) %>% select(-year)
dat_2022 <- dat %>% filter(year == 2022) %>% select(-year)

colnames(dat_2016)[c(3:5)] <- paste0(colnames(dat_2016)[c(3:5)], "_2016")
colnames(dat_2020)[c(3:5)] <- paste0(colnames(dat_2020)[c(3:5)], "_2020")
colnames(dat_2022)[c(3:5)] <- paste0(colnames(dat_2022)[c(3:5)], "_2022")

dat <- dat_2016 %>%
  left_join(dat_2020) %>%
  left_join(dat_2022)

fwrite(dat, file.path('figures', 'figuredat', 'Fig3A_fortext.csv'))


