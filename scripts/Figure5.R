cat(paste0('Start running Figure5.R'))

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

#unique(AnalysisDat$FutScen_label)
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
  filter(Strata == "high" &
           statistic == "median" &
           year == 2020 &
           FutScen_nr %in% CMandLLINFutScenDat$FutScen_nr)

tapply(AnalysisDat$PRdiff.baseline.perc, AnalysisDat$FutScen_labelPlot, summary)


# PR, PRdiff.baseline.perc
Fig5A <- ggplot(data = AnalysisDat, aes(x = FutScen_labelPlot, y = PRdiff.baseline.perc, fill = FutScen_labelPlot)) +
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

print(Fig5A)

### Summary table
AnalysisDat %>%
  aggregatDat(c("FutScen_labelPlot"), "PRdiff.baseline.perc",
              "Population_2016", weightedAggr = T) %>%
  fwrite(file.path('figures', 'figuredat', 'Fig5_summary_weighted'))

AnalysisDat %>%
  aggregatDat(c("FutScen_labelPlot"),
              "PRdiff.baseline.perc",
              "Population_2016", weightedAggr = F) %>%
  fwrite(file.path('figures', 'figuredat', 'Fig5_summary_unweighted'))


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
pplot
ggsave("Fig_5.png", plot = pplot, path = 'figures', width = 14, height = 10, device = "png")
ggsave("Fig_5.pdf", plot = pplot, path = 'figures', width = 14, height = 10, device = "pdf")

AnalysisDat %>%
  dplyr::select(District, year, Strata, FutScen_nr, FutScen_label, EIRgrp,
                PR, PRdiff.baseline.perc, PRdiff.baseline.perc_grp) %>%
  fwrite(file = file.path('figures', 'figuredat', "Fig_5A.csv"))
fwrite(tabDat, file = file.path('figures', 'figuredat', "Fig_5B.csv"))

rm(Fig5A, Fig5B, pplot, tabDat)


##---------------------------------------
#### INCIDENCE
##---------------------------------------
Fig5A <- ggplot() +
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
pplot
ggsave("Fig_5_inc.png", plot = pplot, path = 'figures', width = 14, height = 10, device = "png")
#ggsave("Fig_5_inc.pdf", plot = pplot, path = 'figures', width = 14, height = 10, device = "pdf")

AnalysisDat %>%
  dplyr::select(District, year, Strata, FutScen_nr, FutScen_label, EIRgrp,
                PR, PRdiff.baseline.perc, PRdiff.baseline.perc_grp) %>%
  fwrite(file = file.path('figures', 'figuredat', "Fig_5A_inc.csv"))

fwrite(tabDat, file = file.path('figures', 'figuredat', "Fig_5B_inc.csv"))


###-------------------------
describeDetails = FALSE
if (describeDetails) {
  load(file.path("simdat", "AnalysisDat.RData"))
  testdat <- AnalysisDat %>%
    filter(Strata == "high" &
             statistic == "median" &
             year <= 2020 &
             futSNPcov == 0.7 &
             CMincrease == '0.6057272' &
             FutScen_label_noCM == "ITN (MRC+continuous)+IRS+IPTsc") %>%
    group_by(District) %>%
    mutate(prdiff50 = ifelse(max(PRdiff.baseline.perc) < 50, 'LT50', 'GT50')) %>%
    as.data.table()

  ggplot(data = subset(testdat, Strata == 'high')) +
    geom_line(aes(x = year, y = PR, group = District)) +
    facet_wrap(~prdiff50)

  unique(AnalysisDat$FutScen_label_noCM)

  testdat <- AnalysisDat %>%
    filter(Strata == "high" &
             statistic == "median" &
             year <= 2020 &
             ((futSNPcov == 0.7 &
               CMincrease == '0.6057272' &
               FutScen_label_noCM == "ITN (MRC+continuous)+IRS+IPTsc") | (
               FutScen_label_noCM == "counterfactual"
             ))) %>%
    group_by(District, Strata) %>%
    mutate(prdiff50 = ifelse(max(PRdiff.baseline.perc) < 0.5, 'LT50', 'GT50')) %>%
    as.data.table()

  ggplot(data = subset(testdat, Strata == 'high')) +
    geom_line(aes(x = year, y = PR, col = FutScen_label_noCM,
                  group = interaction(District, FutScen_label_noCM))) +
    facet_wrap(~prdiff50)


  tempdat <- testdat %>%
    group_by(District, prdiff50) %>%
    #filter(max(PRdiff.baseline.perc) < 0.5) %>%
    dplyr::select(District, Region, MIS_UMRC, Strata, prdiff50, EIR, PR, PRdiff.baseline.perc, FutScen_label, FutScen_nr)

  tapply(tempdat$EIR, tempdat$prdiff50, summary)

  tempdat <- tempdat %>%
    group_by(District) %>%
    filter(max(PRdiff.baseline.perc) < 50)
  unique(tempdat[, c('District', 'MIS_UMRC')])
  unique(tempdat[, c('District', 'Region')])
  unique(tempdat[, c('District', 'EIR')])

  tempdat <- AnalysisDat %>%
    group_by(District) %>%
    filter(statistic == 'median') %>%
    mutate(prdiff50 = ifelse(max(PRdiff.baseline.perc) < 50, 'LT50', 'GT50')) %>%
    group_by(District, Region, Zone, MIS_UMRC, prdiff50, Strata) %>%
    summarize(EIR = mean(EIR)) %>%
    as.data.table()

  tapply(tempdat$EIR, tempdat$prdiff50, summary)

  ggplot(data = tempdat, aes(x = prdiff50, y = EIR)) +
    geom_violin(draw_quantiles = c(0.5), width = 0.7)

}


