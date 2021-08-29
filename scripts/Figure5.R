## -----------------------------------------
#### Figure 5 - Incremental impact in high strata
## -----------------------------------------
require(data.table)
require(tidyverse)
require(cowplot)

library(spatstat)
source(file.path("rlibrary", "f_spread.R"))
source(file.path("rlibrary", "f_aggrDat.R"))

incrementalFutScenLabels <- c(
  "increase in CM", "increase in CM - ITN MRC",
  "increase in CM - ITN MRC-IPTsc",
  "increase in CM - ITN MRC continuous",
  "increase in CM - ITN MRC continuous-IPTsc",
  "increase in CM - ITN MRC continuous-IRS",
  "increase in CM - ITN MRC continuous-IRS-IPTsc"
)


incrementalFutScenLabels <- c(
  "increase in CM", "increase in CM - ITN MRC",
  "increase in CM - ITN MRC-IRS",
  "increase in CM - ITN MRC-IPTsc",
  "increase in CM - ITN MRC continuous",
  "increase in CM - ITN MRC continuous-IPTsc",
  "increase in CM - ITN MRC continuous-IRS",
  "increase in CM - ITN MRC continuous-IRS-IPTsc"
)


theme_set(theme_cowplot())
fig5cols <- c('#00B2EE', '#8DC63F', '#EE7600', '#C53F42', '#628A2C', '#C38B4B', '#614525', '#9D3234')

load(file.path("simdat", "AnalysisDat.RData"))

CMandLLINFutScenDat <- AnalysisDat %>%
  filter(FutScen_label %in% incrementalFutScenLabels &
           futITNcov != 0.5 &
           futSNPcov != 0.4) %>%
  dplyr::select(FutScen_nr, FutScen_label) %>%
  unique()

## add table on bottom of plot ?
AnalysisDat$FutScen_labelPlot <- gsub("increase in ", "",
                                      gsub("MRC continuous", "MRC\n+SNP",
                                           gsub("-", "\n+", AnalysisDat$FutScen_label)))


### add table  (Additional table 1 ? )  ## To DO correct table!!
tempdat <- subset(AnalysisDat, Strata == "high" &
  statistic == "median" &
  year == 2020 &
  FutScen_nr %in% CMandLLINFutScenDat$FutScen_nr)

tapply(tempdat$PRdiff.baseline.perc, tempdat$FutScen_labelPlot, summary)


# PR, PRdiff.baseline.perc
Fig5A <- ggplot(data = tempdat, aes(x = FutScen_labelPlot, y = PRdiff.baseline.perc, fill = FutScen_labelPlot)) +
  geom_hline(yintercept = c(-1 * c(0, 10, 50, 80, 100), c(0, 10, 50, 80, 100)), color = 'grey', size = 0.5) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed") +
  geom_jitter(data = tempdat, aes(x = FutScen_labelPlot, y = PRdiff.baseline.perc),
              fill = "lightgrey", width = 0.1, height = 0.1, shape = 21) +
  scale_fill_manual(values = fig5cols) +
  theme(legend.position = 'None', axis.text.x = element_blank()) +
  scale_y_continuous(breaks = c(-100, -80, -50, -10, 0, 10, 50, 80, 100),
                     labels = c(-100, -80, -50, -10, 0, 10, 50, 80, 100),
                     limits = c(-140, 105)) +
  labs(x = "", y = "PfPR2to10 reduction\n2016-2020")


tempdat %>% f_aggrDat("FutScen_labelPlot", "PRdiff.baseline.perc")


tempdat$PRdiff.baseline.perc_grp <- ""
tempdat$PRdiff.baseline.perc_grp[tempdat$PRdiff.baseline.perc <= 0] <- "increase in PfPR"
tempdat$PRdiff.baseline.perc_grp[tempdat$PRdiff.baseline.perc > 0 & tempdat$PRdiff.baseline.perc < 10] <- "<10% reduction in PfPR"
tempdat$PRdiff.baseline.perc_grp[tempdat$PRdiff.baseline.perc >= 10 & tempdat$PRdiff.baseline.perc < 50] <- ">10% reduction in PfPR"
tempdat$PRdiff.baseline.perc_grp[tempdat$PRdiff.baseline.perc >= 50 & tempdat$PRdiff.baseline.perc < 80] <- ">50% reduction in PfPR"
tempdat$PRdiff.baseline.perc_grp[tempdat$PRdiff.baseline.perc >= 80] <- ">80% reduction in PfPR"
tempdat$PRdiff.baseline.perc_grp <- factor(tempdat$PRdiff.baseline.perc_grp,
                                           levels = c("increase in PfPR",
                                                      "<10% reduction in PfPR",
                                                      ">10% reduction in PfPR",
                                                      ">50% reduction in PfPR",
                                                      ">80% reduction in PfPR"),
                                           labels = c("no reduction",
                                                      ">0-10%",
                                                      "10-50%",
                                                      "50-80%",
                                                      "80-100%")
)

table(tempdat$PRdiff.baseline.perc_grp, exclude = NULL)

tempdat %>%
  filter(PRdiff.baseline.perc > 50) %>%
  dplyr::group_by(FutScen_labelPlot, District) %>%
  unique() %>%
  dplyr::group_by(FutScen_labelPlot) %>%
  dplyr::tally()

(tab <- tempdat %>%
  group_by(FutScen_labelPlot, PRdiff.baseline.perc_grp) %>%
  tally() %>%
  spread(PRdiff.baseline.perc_grp, n) %>%
  as.data.frame())

(tab2 <- tempdat %>%
  group_by(FutScen_label, District, PRdiff.baseline.perc_grp) %>%
  dplyr::summarize(perc = mean(PRdiff.baseline.perc)) %>%
  spread(PRdiff.baseline.perc_grp, perc) %>%
  as.data.frame())

table(tempdat$PRdiff.baseline.perc_grp)

table(gsub("\n", "+", tempdat$FutScen_labelPlot), tempdat$PRdiff.baseline.perc_grp)
rowSums(table(gsub("\n", "+", tempdat$FutScen_labelPlot), tempdat$PRdiff.baseline.perc_grp))

tabDat <- as.data.frame(tab)
tabDat <- melt(tabDat)
tabDat$value <- as.numeric(as.character(tabDat$value))

Fig5B <- ggplot(data = tabDat, aes(x = variable, y = value, label = value)) +
  geom_bar(aes(fill = FutScen_labelPlot), stat = "identity", width = 0.5, col = "black") +
  geom_label() +
  facet_wrap(~FutScen_labelPlot, nrow = 1) +
  theme(
    legend.position = 'None',
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, color = "black", face = "plain"),
    strip.text.y = element_text(size = 12, color = "black", face = "plain")
  ) +
  coord_flip() +
  scale_fill_manual(values = fig5cols) +
  labs(x = "PfPR2to10 reduction\n2016-2020", y = "Number of councils") +
  scale_y_continuous(breaks = c(), labels = c(), limits = c(-2, 63)) +
  geom_vline(xintercept = c(-Inf, Inf))


pplot <- plot_grid(Fig5A, Fig5B, nrow = 2, rel_heights = c(1, 1), align = "v", axis = "l")
ggsave("Fig_5.png", plot = pplot, path = 'figures', width = 14, height = 10, device = "png")
ggsave("Fig_5.pdf", plot = pplot, path = 'figures', width = 14, height = 10, device = "pdf")


fwrite(tempdat[, c('District', 'year', 'Strata', 'FutScen_nr', 'FutScen_label', 'FutScen_label_noCM',
                   'EIRgrp', 'PR', 'PRdiff.baseline.perc', 'PRdiff.baseline.perc_grp')], file = file.path('figures', "Fig_5A.csv"))
fwrite(tabDat, file = file.path('figures', "Fig_5B.csv"))


####################### INCIDENCE


# _incidence
Fig5A <- ggplot() +
  geom_hline(yintercept = c(-1 * c(0, 10, 50, 80, 100), c(0, 10, 50, 80, 100)), color = 'grey', size = 0.5) +
  geom_hline(yintercept = 0) +
  geom_boxplot(data = tempdat, aes(x = FutScen_labelPlot, y = CasesAverted.baseline.perc, fill = FutScen_labelPlot)) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed") +
  geom_jitter(data = tempdat, aes(x = FutScen_labelPlot, y = CasesAverted.baseline.perc),
              fill = "lightgrey", width = 0.1, height = 0.1, shape = 21) +
  scale_fill_manual(values = fig5cols) +
  theme(legend.position = 'None', axis.text.x = element_blank()) +
  scale_y_continuous(breaks = c(-100, -80, -50, -10, 0, 10, 50, 80, 100),
                     labels = c(-100, -80, -50, -10, 0, 10, 50, 80, 100),
                     limits = c(-140, 105)) +
  labs(x = "", y = "Cases averted (%)\n2016-2020")


tempdat$CasesAverted.baseline.perc_grp <- 0
tempdat$CasesAverted.baseline.perc_grp[tempdat$CasesAverted.baseline.perc <= 0] <- "increase in Cases"
tempdat$CasesAverted.baseline.perc_grp[tempdat$CasesAverted.baseline.perc > 0 & tempdat$CasesAverted.baseline.perc < 10] <- "<10% reduction in Cases"
tempdat$CasesAverted.baseline.perc_grp[tempdat$CasesAverted.baseline.perc >= 10 & tempdat$CasesAverted.baseline.perc < 50] <- ">10% reduction in Cases"
tempdat$CasesAverted.baseline.perc_grp[tempdat$CasesAverted.baseline.perc >= 50 & tempdat$CasesAverted.baseline.perc < 80] <- ">50% reduction in Cases"
tempdat$CasesAverted.baseline.perc_grp[tempdat$CasesAverted.baseline.perc >= 80] <- ">80% reduction in Cases"
tempdat$CasesAverted.baseline.perc_grp <- factor(tempdat$CasesAverted.baseline.perc_grp,
                                                 levels = c("increase in Cases",
                                                            "<10% reduction in Cases",
                                                            ">10% reduction in Cases",
                                                            ">50% reduction in Cases",
                                                            ">80% reduction in Cases"),
                                                 labels = c("no reduction",
                                                            ">0-10%",
                                                            "10-50%",
                                                            "50-80%",
                                                            "80-100%")
)

table(tempdat$CasesAverted.baseline.perc_grp, exclude = NULL)

tempdat %>%
  filter(CasesAverted.baseline.perc_grp > 50) %>%
  dplyr::group_by(FutScen_labelPlot, District) %>%
  unique() %>%
  dplyr::group_by(FutScen_labelPlot) %>%
  dplyr::tally()

(tab <- tempdat %>%
  group_by(FutScen_labelPlot, CasesAverted.baseline.perc_grp) %>%
  tally() %>%
  spread(CasesAverted.baseline.perc_grp, n) %>%
  as.data.frame())

(tab2 <- tempdat %>%
  group_by(FutScen_label, District, CasesAverted.baseline.perc_grp) %>%
  dplyr::summarize(perc = mean(CasesAverted.baseline.perc_grp)) %>%
  spread(CasesAverted.baseline.perc_grp, perc) %>%
  as.data.frame())

table(tempdat$CasesAverted.baseline.perc_grp)

table(gsub("\n", "+", tempdat$FutScen_labelPlot), tempdat$CasesAverted.baseline.perc_grp)
rowSums(table(gsub("\n", "+", tempdat$FutScen_labelPlot), tempdat$CasesAverted.baseline.perc_grp))

tabDat <- as.data.frame(tab)
tabDat <- melt(tabDat)
tabDat$value <- as.numeric(as.character(tabDat$value))

Fig5B <- ggplot(data = tabDat, aes(x = variable, y = value, label = value)) +
  geom_bar(aes(fill = FutScen_labelPlot), stat = "identity", width = 0.5, col = "black") +
  geom_label() +
  facet_wrap(~FutScen_labelPlot, nrow = 1) +
  theme(
    legend.position = 'None',
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, color = "black", face = "plain"),
    strip.text.y = element_text(size = 12, color = "black", face = "plain")
  ) +
  coord_flip() +
  scale_fill_manual(values = fig5cols) +
  labs(x = "Cases averted\n2016-2020", y = "Number of councils") +
  scale_y_continuous(breaks = c(), labels = c(), limits = c(-2, 63)) +
  geom_vline(xintercept = c(-Inf, Inf))


pplot <- plot_grid(Fig5A, Fig5B, nrow = 2, rel_heights = c(1, 1), align = "v", axis = "l")
ggsave("Fig_5_inc.png", plot = pplot, path = 'figures', width = 14, height = 10, device = "png")
ggsave("Fig_5_inc.pdf", plot = pplot, path = 'figures', width = 14, height = 10, device = "pdf")


fwrite(tempdat[, c('District', 'year', 'Strata', 'FutScen_nr', 'FutScen_label', 'FutScen_label_noCM',
                   'EIRgrp', 'PR', 'PRdiff.baseline.perc', 'PRdiff.baseline.perc_grp')], file = file.path('figures', "Fig_5A_inc.csv"))
fwrite(tabDat, file = file.path('figures', "Fig_5B_inc.csv"))


