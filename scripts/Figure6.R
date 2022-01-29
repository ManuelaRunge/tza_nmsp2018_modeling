cat(paste0('Start running Figure6.R'))

load(file.path("simdat", "AnalysisDat.RData"))

incrementalFutScenLabels <- c(
  "increase in CM",
  "increase in CM - ITN MRC",
  "increase in CM - ITN MRC-IPTsc",
  "increase in CM - ITN MRC-IRS",
  "increase in CM - ITN MRC continuous",
  "increase in CM - ITN MRC continuous-IPTsc",
  "increase in CM - ITN MRC continuous-IRS",
  "increase in CM - ITN MRC continuous-IRS-IPTsc"
)

incrementalFutScenLabels[!(incrementalFutScenLabels %in% unique(AnalysisDat$FutScen_label))]
CMandLLINFutScenDat <- AnalysisDat %>%
  filter(FutScen_label %in% incrementalFutScenLabels &
           futITNcov != 0.5 &
           futSNPcov != 0.4) %>%
  dplyr::select(FutScen_nr, FutScen_label) %>%
  unique()

## add table on bottom of plot ?
AnalysisDat$FutScen_labelPlot <- gsub("increase in ", "",
                                      gsub("MRC continuous", "MRC\n+SNP",
                                           gsub("-", "\n+",
                                                AnalysisDat$FutScen_label)))

AnalysisDat <- AnalysisDat %>%
  filter(Strata_withoutUrban == "high" &
           statistic == "median" &
           year == 2020 &
           FutScen_nr %in% CMandLLINFutScenDat$FutScen_nr)

dim(AnalysisDat)
tapply(AnalysisDat$PRdiff.baseline.perc, AnalysisDat$FutScen_labelPlot, summary)


# PR, PRdiff.baseline.perc
Fig6A <- ggplot(data = AnalysisDat, aes(x = FutScen_labelPlot, y = PRdiff.baseline.perc, fill = FutScen_labelPlot)) +
  geom_hline(yintercept = c(-1 * c(0, 10, 50, 80, 100), c(0, 10, 50, 80, 100)), color = 'grey', size = 0.5) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed") +
  geom_jitter(data = AnalysisDat, aes(x = FutScen_labelPlot, y = PRdiff.baseline.perc),
              fill = "lightgrey", width = 0.1, height = 0.1, shape = 21) +
  scale_fill_manual(values = fig5cols) +
  theme(legend.position = 'None', axis.text.x = element_blank()) +
  scale_y_continuous(breaks = c(-100, -80, -50, -10, 0, 10, 50, 80, 100),
                     labels = c(-100, -80, -50, -10, 0, 10, 50, 80, 100),
                     limits = c(-140, 105)) +
  labs(x = "", y = "PfPR2to10 reduction\n2016-2020")

### Summary table
AnalysisDat %>%
  aggregatDat(c("FutScen_labelPlot"), "PRdiff.baseline.perc",
              "Population_2016", weightedAggr = T) %>%
  fwrite(file.path('figures', 'figuredat', 'Fig5_summary_weighted.csv'))

AnalysisDat %>%
  aggregatDat(c("FutScen_labelPlot"),
              "PRdiff.baseline.perc",
              "Population_2016", weightedAggr = F) %>%
  fwrite(file.path('figures', 'figuredat', 'Fig6_summary_unweighted.csv'))


summary(AnalysisDat$PRdiff.baseline.perc)
pfprgrp_lbl <- c("no reduction", ">0-10%", "10-50%", "50-80%", "80-100%")
AnalysisDat$PRdiff.baseline.perc_grp <- cut(AnalysisDat$PRdiff.baseline.perc, c(-Inf, 0, 10, 50, 80, Inf), labels = pfprgrp_lbl)
AnalysisDat$PRdiff.baseline.perc_grp <- factor(AnalysisDat$PRdiff.baseline.perc_grp, levels = pfprgrp_lbl, labels = pfprgrp_lbl)
tapply(AnalysisDat$PRdiff.baseline.perc, AnalysisDat$PRdiff.baseline.perc_grp, summary)

AnalysisDat %>%
  filter(PRdiff.baseline.perc > 50) %>%
  dplyr::group_by(FutScen_labelPlot, District) %>%
  unique() %>%
  dplyr::group_by(FutScen_labelPlot) %>%
  dplyr::tally()

(tab <- AnalysisDat %>%
  group_by(FutScen_labelPlot, PRdiff.baseline.perc_grp) %>%
  tally() %>%
  pivot_wider(names_from = PRdiff.baseline.perc_grp, values_from = n) %>%
  as.data.frame())

(tab2 <- AnalysisDat %>%
  group_by(FutScen_label, District, PRdiff.baseline.perc_grp) %>%
  dplyr::summarize(perc = mean(PRdiff.baseline.perc)) %>%
  pivot_wider(names_from = PRdiff.baseline.perc_grp, values_from = perc) %>%
  as.data.frame())

table(AnalysisDat$PRdiff.baseline.perc_grp)
table(gsub("\n", "+", AnalysisDat$FutScen_labelPlot), AnalysisDat$PRdiff.baseline.perc_grp)
rowSums(table(gsub("\n", "+", AnalysisDat$FutScen_labelPlot), AnalysisDat$PRdiff.baseline.perc_grp))

tabDat <- tab %>% pivot_longer(cols = -FutScen_labelPlot, names_to = 'variable')
tabDat$variable <- factor(tabDat$variable, levels = pfprgrp_lbl, labels = pfprgrp_lbl)

Fig6B <- ggplot(data = tabDat, aes(x = variable, y = value, label = value)) +
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


pplot <- plot_grid(Fig6A, Fig6B, nrow = 2, rel_heights = c(1, 1), align = "v", axis = "l")
ggsave("Fig_6.png", plot = pplot, path = 'figures', width = 14, height = 10, device = "png")
if (SAVEpdf)ggsave("Fig_6.pdf", plot = pplot, path = 'figures', width = 14, height = 10, device = "pdf")

AnalysisDat %>%
  dplyr::select(District, year, Strata, Strata_withoutUrban, FutScen_nr, FutScen_label, EIRgrp,
                PR, PRdiff.baseline.perc, PRdiff.baseline.perc_grp) %>%
  fwrite(file = file.path('figures', 'figuredat', "Fig_5A.csv"))

fwrite(tabDat, file = file.path('figures', 'figuredat', "Fig_5B.csv"))

rm(Fig6A, Fig6B, pplot, tabDat)


##---------------------------------------
#### INCIDENCE
##---------------------------------------

Fig6A <- ggplot() +
  geom_hline(yintercept = c(-1 * c(0, 10, 50, 80, 100), c(0, 10, 50, 80, 100)), color = 'grey', size = 0.5) +
  geom_hline(yintercept = 0) +
  geom_boxplot(data = AnalysisDat, aes(x = FutScen_labelPlot, y = CasesAverted.baseline.perc, fill = FutScen_labelPlot)) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed") +
  geom_jitter(data = AnalysisDat, aes(x = FutScen_labelPlot, y = CasesAverted.baseline.perc),
              fill = "lightgrey", width = 0.1, height = 0.1, shape = 21) +
  scale_fill_manual(values = fig5cols) +
  theme(legend.position = 'None', axis.text.x = element_blank()) +
  scale_y_continuous(breaks = c(-100, -80, -50, -10, 0, 10, 50, 80, 100),
                     labels = c(-100, -80, -50, -10, 0, 10, 50, 80, 100),
                     limits = c(-140, 105)) +
  labs(x = "", y = "Cases averted (%)\n2016-2020")


CasesAverted_lbl <- c("no reduction", ">0-10%", "10-50%", "50-80%", "80-100%")
AnalysisDat$CasesAverted.baseline.perc_grp <- cut(AnalysisDat$CasesAverted.baseline.perc, c(-Inf, 0, 10, 50, 80, Inf),
                                                  labels = CasesAverted_lbl)
AnalysisDat$CasesAverted.baseline.perc_grp <- factor(AnalysisDat$CasesAverted.baseline.perc_grp,
                                                     levels = CasesAverted_lbl, labels = CasesAverted_lbl)
tapply(AnalysisDat$CasesAverted.baseline.perc, AnalysisDat$CasesAverted.baseline.perc_grp, summary)


AnalysisDat %>%
  filter(CasesAverted.baseline.perc > 50) %>%
  dplyr::group_by(FutScen_labelPlot, District) %>%
  unique() %>%
  dplyr::group_by(FutScen_labelPlot) %>%
  dplyr::tally()

(tab <- AnalysisDat %>%
  group_by(FutScen_labelPlot, CasesAverted.baseline.perc_grp) %>%
  tally() %>%
  spread(CasesAverted.baseline.perc_grp, n) %>%
  as.data.frame())

(tab2 <- AnalysisDat %>%
  group_by(FutScen_label, District, CasesAverted.baseline.perc_grp) %>%
  dplyr::summarize(perc = mean(CasesAverted.baseline.perc_grp)) %>%
  spread(CasesAverted.baseline.perc_grp, perc) %>%
  as.data.frame())

table(AnalysisDat$CasesAverted.baseline.perc_grp)
table(gsub("\n", "+", AnalysisDat$FutScen_labelPlot), AnalysisDat$CasesAverted.baseline.perc_grp)
rowSums(table(gsub("\n", "+", AnalysisDat$FutScen_labelPlot), AnalysisDat$CasesAverted.baseline.perc_grp))

tabDat <- tab %>% pivot_longer(cols = -FutScen_labelPlot, names_to = 'variable')

Fig6B <- ggplot(data = tabDat, aes(x = variable, y = value, label = value)) +
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


pplot <- plot_grid(Fig6A, Fig6B, nrow = 2, rel_heights = c(1, 1), align = "v", axis = "l")
ggsave("Fig_6_inc.png", plot = pplot, path = 'figures', width = 14, height = 10, device = "png")
if (SAVEpdf)ggsave("Fig_6_inc.pdf", plot = pplot, path = 'figures', width = 14, height = 10, device = "pdf")

AnalysisDat %>%
  dplyr::select(District, year, Strata, Strata_withoutUrban, FutScen_nr, FutScen_label, EIRgrp,
                PR, PRdiff.baseline.perc, PRdiff.baseline.perc_grp) %>%
  fwrite(file = file.path('figures', 'figuredat', "Fig_6A_inc.csv"))

fwrite(tabDat, file = file.path('figures', 'figuredat', "Fig_6B_inc.csv"))

rm(tabDat, AnalysisDat, Fig6A, Fig6B, pplot, tab, tab2)

