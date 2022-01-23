cat(paste0('Start running Figure6.R'))

load(file.path("simdat", "AnalysisDat.RData"))
AnalysisDat <- AnalysisDat %>% mutate(PR = PR * 100)
NMSPdat_long <- f_load_nmsp_scendat()

AnalysisDat2 <- inner_join(AnalysisDat, NMSPdat_long[, c('District', 'FutScen', 'Strategy', 'Strategy_FutScen_nr')])
dim(AnalysisDat2)
table(AnalysisDat2$Strategy, AnalysisDat2$CMincrease, exclude = NULL)
length(unique(NMSPdat_long$Strategy))
length(unique(AnalysisDat2$Strategy))

AnalysisDat2$Cases.pP <- AnalysisDat2$Cases / simPop
AnalysisDat2$incidence <- (AnalysisDat2$Cases / simPop) * 1000


## PRdiff.baseline.perc
Disdat1 <- AnalysisDat2 %>%
  dplyr::filter(year == 2020 &
                  Strategy %in% selectedStrategies) %>%
  dplyr::select(District, statistic, Population_2016, Strata, Strategy, PR) %>%
  pivot_wider(names_from = Strategy, values_from = PR) %>%
  mutate(outcome = "PR",
         relNMSPoldnew = (NMSPcurrent.withCM / revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM)
  ) %>%
  as.data.frame()
dim(Disdat1)
table(Disdat1$District, Disdat1$statistic)

Disdat2 <- AnalysisDat2 %>%
  dplyr::filter(year == 2020 &
                  Strategy %in% selectedStrategies) %>%
  dplyr::select(District, statistic, Population_2016, Strata, Strategy, Cases.pP) %>%
  spread(Strategy, Cases.pP) %>%
  mutate(
    outcome = "Cases.pP",
    relNMSPoldnew = (NMSPcurrent.withCM / revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM)
  ) %>%
  as.data.frame()

Disdat2b <- AnalysisDat2 %>%
  dplyr::filter(year == 2020 &
                  Strategy %in% selectedStrategies) %>%
  dplyr::select(District, statistic, Population_2016, Strata, Strategy, incidence) %>%
  spread(Strategy, incidence) %>%
  mutate(
    outcome = "incidence",
    relNMSPoldnew = (NMSPcurrent.withCM / revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM)
  ) %>%
  as.data.frame()


Disdat <- rbind(Disdat1, Disdat2, Disdat2b)
Disdat$newBetterThanOld <- "old better"
Disdat$newBetterThanOld[Disdat$revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM < Disdat$NMSPcurrent.withCM] <- "new better"

table(Disdat$outcome[Disdat$statistic == "median"], Disdat$newBetterThanOld[Disdat$statistic == "median"], exclude = NULL)

Disdat <- Disdat %>%
  dplyr::group_by(District, statistic, Population_2016) %>%
  dplyr::arrange(District, statistic, Population_2016) %>%
  dplyr::mutate(Levels = ifelse(nlevels(as.factor(newBetterThanOld)) > 1, "No", "Yes"))

table(Disdat$outcome[Disdat$statistic == "median"], Disdat$Levels[Disdat$statistic == "median"])

Disdat$outcome_lbl <- factor(Disdat$outcome,
                             levels = c("Cases.pP", "PR", "incidence"),
                             labels = c('italic("") * "Cases pP"',
                                        'italic("PfPR")["2 to 10"]',
                                        '"Incidence (all ages)"'))

### calculate mean per strata to be added in plot
DisdatAggr <- Disdat %>%
  dplyr::group_by(Strata, statistic, outcome, outcome_lbl) %>%
  dplyr::summarize(
    relNMSPoldnewAggr = mean(relNMSPoldnew),
    relNMSPoldnewAggr_w = weighted.mean(relNMSPoldnew, w = Population_2016)
  )

Disdat <- left_join(Disdat, DisdatAggr)
Disdat$StrataLabel <- factor(Disdat$Strata,
                             levels = c("very low", "low", "urban", "moderate", "high"),
                             labels = c("very low", "low", "urban", "moderate", "high"))

### add arrow indicated that the plot was truncated at -2 (goes up to -6)
### to do add errorbars from posterior for each district?
table(Disdat$outcome)
pplot <- ggplot(data = subset(Disdat, outcome != 'Cases.pP' & statistic == "median")) +
  theme_cowplot() +
  geom_errorbarh(aes(
    x = (NMSPcurrent.withCM / NMSPcurrent.withCM),
    xmin = (NMSPcurrent.withCM / NMSPcurrent.withCM),
    xmax = (relNMSPoldnew), col = StrataLabel, y = reorder(District, as.numeric(StrataLabel))
  ), alpha = 0.75, size = 1.0, height = 0, show.legend = F) +
  geom_line(aes(
    x = relNMSPoldnewAggr, col = StrataLabel, group = StrataLabel, y = reorder(District, as.numeric(StrataLabel))
  ), size = 1.2, show.legend = F) + #, col = "black"
  geom_line(aes(
    x = relNMSPoldnewAggr_w, col = StrataLabel, group = StrataLabel, y = reorder(District, as.numeric(StrataLabel))
  ), size = 1.0, linetype = "dotdash", show.legend = F) + #, col = "grey"
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = 1) +
  scale_x_continuous(lim = c(-0.2, 3.5), expand = c(0, 0)) +
  scale_y_discrete(labels = c()) +
  theme(legend.position = "bottom", axis.ticks.y = element_blank()) +
  scale_fill_manual(values = c("black", "white")) +
  scale_color_manual(values = StrataCols) +
  facet_wrap(~outcome_lbl, scales = "free_x",
             strip.position = "top",
             labeller = labeller(.cols = label_parsed)) +
  theme(
    strip.text.x = element_text(size = 18),
    panel.spacing.x = unit(0.5, "line"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text(hjust = 0, face = "bold", size = 18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.subtitle = element_text(hjust = -0.25, face = "bold", size = 16),
    legend.position = "bottom"
  ) +
  labs(col = "Stratification", subtitle = "",
       x = 'Ratio of predicted impact for 2020\n(2015-2020 NMSP / 2018-2020 NMSP)',
       y = "Councils sorted by strata (n=184)",
       fill = "Agreement between outcomes")


ggsave(paste0("Fig_6.png"), plot = pplot, path = file.path("figures"), width = 10, height = 12, device = "png")
ggsave(paste0("Fig_6.pdf"), plot = pplot, path = file.path("figures"), width = 10, height = 12, device = "pdf")

fwrite(Disdat, file.path('figures', 'figuredat', 'Fig6_Disdat.csv'))