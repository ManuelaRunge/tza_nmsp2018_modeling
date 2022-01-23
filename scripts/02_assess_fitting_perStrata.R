### combined plot with history - additional files
### load historical prevalence data
## KEMRI pfpr
load(file.path('dat', 'pfpr_kemri', 'TZA_KEMRI_pfpr_161018.RData'))
head(KEMRIpfpr_long)
KEMRIpfpr_long <- subset(KEMRIpfpr_long, year >= 2003)

### load aggregated stats per strata
numVars <- c("mean", "sd", "quant_0", "quant_25", "quant_50", "quant_75", "quant_100")
KEMRI_PfPRAggrStrata <- read.csv(file.path('dat', 'pfpr_kemri', 'PfPR_aggregatedStats_perStrata.csv')) %>%
  rename_with(~gsub("[.]", "", .x)) %>%
  mutate_at(.vars = numVars, .funs = function(x) x * 100)

load(file.path(simout_dir, "JAGSresults_wide.RData"))
tempdat <- JAGSresults_wide %>%
  filter(year <= 2017) %>%
  group_by(District, Strata, Strata_withoutUrban, Population_2016, year, statistic) %>%
  summarise(PR = mean(PR))
table(tempdat$District, tempdat$year)

combDat <- KEMRIpfpr_long %>%
  filter(year == 2016) %>%
  dplyr::select(District, year, PfPR) %>%
  left_join(tempdat) %>%
  gather(var, value, -c(District, Strata, Strata_withoutUrban, Population_2016, year, statistic)) %>%
  mutate(var = ifelse(var == "PfPR", "MBG", "OM"))

tempdat2 <- KEMRIpfpr_long %>%
  filter(year >= 2003 & year <= 2016) %>%
  dplyr::select(District, year, PfPR) %>%
  left_join(tempdat)

tempdat3 <- tempdat %>%
  filter(statistic == "median") %>%
  dplyr::select(District, Strata, Strata_withoutUrban, Population_2016, year, PR)

tempdat3b <- tempdat %>%
  dplyr::select(District, Strata, Strata_withoutUrban, Population_2016, year, statistic, PR) %>%
  pivot_wider(names_from = statistic, values_from = PR)
tapply(tempdat3b$mean, tempdat3b$year, summary)

tempdat4 <- KEMRIpfpr_long %>%
  filter(year >= 2003 & year <= 2016) %>%
  dplyr::select(District, year, PfPR) %>%
  left_join(tempdat)

combDat2 <- KEMRIpfpr_long %>%
  filter(year >= 2003 & year <= 2020) %>%
  dplyr::select(District, year, PfPR) %>%
  left_join(tempdat3) %>%
  pivot_longer(cols = -c(District, Strata, Strata_withoutUrban, Population_2016, year)) %>%
  mutate(name = ifelse(name == "PfPR", "MBG", "OM"),
         value = as.numeric(value)) %>%
  aggregatDat(c("Strata", "year", "name"), "value", "Population_2016", weightedAggr = weightedAggr)

tapply(combDat2$mean, combDat2$year, summary)

combDat3 <- KEMRIpfpr_long %>%
  dplyr::select(District, year, PfPR) %>%
  left_join(tempdat3) %>%
  pivot_longer(cols = -c(District, Strata, Strata_withoutUrban, Population_2016, year)) %>%
  mutate(name = ifelse(name == "PfPR", "MBG", "OM"),
         value = as.numeric(value)) %>%
  aggregatDat(c("Strata", "year", "name"), "value", "Population_2016", weightedAggr = weightedAggr)

tapply(combDat3$mean, combDat3$year, summary)

combDat3_council <- KEMRIpfpr_long %>%
  dplyr::select(District, year, PfPR) %>%
  left_join(tempdat3b) %>%
  mutate(MBG = PfPR, OMmean = mean, OMmean = mean, OMmedian = median, OMq2.5 = q2.5, OMq97.5 = q97.5) # %>%
# gather(var, value, -c(District,  Strata,  year)) %>%
# mutate(var = ifelse(var=="PfPR","MBG", "OM"))
tapply(combDat3_council$mean, combDat3_council$year, summary)


#### for special timeline
#### counterfactual
load(file.path("simdat", "AnalysisDat.RData"))
tempdat5 <- AnalysisDat %>%
  filter(year %in% c(2016, 2017) &
           counterfactual == 1 &
           statistic == "median") %>%
  mutate(Strategy = "counterfactual") %>%
  dplyr::select(District, Strata, Population_2016, year, PR, Strategy)

tempdat5 <- tempdat %>%
  ungroup() %>%
  filter(year < 2016) %>%
  mutate(Strategy = "counterfactual") %>%
  dplyr::select(District, Strata, Population_2016, year, PR, Strategy) %>%
  bind_rows(tempdat5)

combDat5 <- KEMRIpfpr_long %>%
  filter(year >= 2003 & year <= 2020) %>%
  dplyr::select(District, year, PfPR) %>%
  merge(tempdat5) %>%
  pivot_longer(cols = -c(District, Strata, Strategy, Population_2016, year)) %>%
  mutate(name = ifelse(name == "PfPR", "MBG", "OM"),
         value = as.numeric(value)) %>%
  aggregatDat(c("Strategy", "Strata", "year", "name"), "value", "Population_2016", weightedAggr = weightedAggr)


tapply(combDat5$mean.val, combDat5$year, summary)
table(combDat5$year, combDat5$Strategy, exclude = NULL)


## -----------------------------------------
#### Figure A2.2 - timeline in  additional file
## -----------------------------------------

p1 <- ggplot(data = subset(tempdat, statistic == 'median' & year <= 2016)) +
  geom_line(aes(x = year, y = PR, group = District, col = Strata, span = 0.3), size = 0.8, alpha = 0.3) +
  geom_line(data = subset(combDat3, name == 'OM' & year <= 2016), aes(x = year, y = mean.val, group = Strata), col = "grey25", size = 1.3) +
  scale_color_manual(values = StrataCols) +
  scale_fill_manual(values = StrataCols) +
  facet_wrap(~Strata, scales = "free", nrow = 1) +
  labs(x = "", y = expression("OM " * italic("PfPR")["2 to 10"] * " (%)")) +
  theme_classic() +
  theme(legend.position = "none")

p2 <- ggplot(data = subset(tempdat2, year <= 2016)) +
  geom_line(aes(x = year, y = PfPR, group = District, col = Strata, span = 0.3), size = 0.8, alpha = 0.3, se = FALSE) +
  geom_line(data = subset(combDat3, name == 'MBG' & year <= 2016), aes(x = year, y = mean.val, group = Strata), col = "grey25", size = 1.3) +
  scale_color_manual(values = StrataCols) +
  scale_fill_manual(values = StrataCols) +
  facet_wrap(~Strata, scales = "free", nrow = 1) +
  labs(x = "", y = expression("MBG-" * italic("PfPR")["2 to 10"] * " (%)")) +
  theme_classic() +
  theme(legend.position = "none")

p12 <- plot_grid(p1, p2, nrow = 2)
ggsave("combined_timeline_perDis.png", plot = p12,
       path = file.path("figures"), width = 13, height = 7, device = "png")


## -----------------------------------------
#### Boxplots PfPR for additional file
## -----------------------------------------
#########
### calculate relative reduction since 2003
sink(file.path(PaperCSVDir, "timeline prevalence.txt"))
combDat5 %>%
  dplyr::filter(year %in% c(2003, 2016) & Strategy == "counterfactual") %>%
  dplyr::select(Strata, year, var, median.val) %>%
  mutate(year = paste0("y", year)) %>%
  unique() %>%
  spread(year, median.val) %>%
  mutate(PRhist03_16 = y2003 - y2016, PRhist03_16.perc = ((y2003 - y2016) / y2003) * 100)


combDat5 %>%
  dplyr::filter(var == "OM" & year %in% c(2020)) %>%
  dplyr::select(Strata, Strategy, year, var, median.val) %>%
  mutate(year = paste0("y", year)) %>%
  unique() %>%
  spread(year, median.val)
sink()

combDat5$tempStrat <- factor(combDat5$StrategyLabel,
                             levels = c("counterfactual", "NMSP with maintained CM", "NMSP with improved CM", "SMMSP"),
                             label = c("counter\nfactual", "NMSP", "NMSP\nCM+", "SMMSP\nCM+")
)


## -----------------------------------------
#### Timeline plot and boxplots for main text
## -----------------------------------------
labsy <- c("2003", "'04", "'05", "'06", "'07", "'08", "'09", "'10", "'11", "'12", "'13", "'14", "'15", "2016", "'17", "'18", "'19", "2020", "'21", "'22")
labsy1 <- c("2003", "'04", "'05", "'06", "'07", "'08", "'09", "'10", "'11", "'12", "'13", "'14", "'15", "2016", "'17", "'18", "'19", "2020")
labsy2 <- c("2003", "", "'05", "", "'07", "", "'09", "", "'11", "", "'13", "", "'15", "'16")
labsy3 <- c("2003", "'04", "'05", "'06", "'07", "'08", "'09", "'10", "'11", "'12", "'13", "'14", "'15", "2016", "'17")


lineLegend <- get_legend(ggplot() +
                           theme_cowplot() +
                           geom_line(
                             data = subset(combDat5),
                             aes(x = year, y = mean.val, group = interaction(Strata, name, Strategy),
                                 linetype = interaction(Strategy)), size = 3
                           ) +
                           theme(legend.position = "right") +
                           scale_linetype_manual(values = c("dotdash", "solid", "dashed", "dotted")) +
                           theme_classic() +
                           labs(linetype = "") +
                           theme(legend.key.width = unit(4, "lines"), legend.key.height = unit(0.5, "lines"), legend.key.size = unit(2, "lines"), legend.text = element_text(size = 15)) +
                           guides(linetype = guide_legend(nrow = 1)))
plot(lineLegend)


EndYear <- 2017
s_labsy <- labsy3

count <- 0
for (strata in unique(combDat5$Strata)) {
  count <- count + 1
  print(count)
  # strata="urban"

  temp <- ggplot() +
    annotate("rect", xmin = 2016, xmax = 2017, ymin = -Inf, ymax = Inf,
             fill = "lightgrey", alpha = 0.5, size = 1.5) +
    geom_vline(xintercept = c(2003, 2016)) +
    # geom_vline(xintercept = c( 2020), linetype="dashed")+
    #  geom_hline(yintercept = c(1,5,10,30), linetype="dashed", color="gray60")+
    geom_ribbon(
      data = subset(combDat5, Strata %in% strata & name == "OM"),
      aes(x = year, ymax = upper.ci.val, ymin = lower.ci.val, group = interaction(Strata, name, Strategy), fill = Strata), alpha = 0.1
    ) +
    geom_line(
      data = subset(combDat5, Strata %in% strata & (year <= 2016 | (year <= 2017 & name != "OM"))),
      aes(x = year, y = mean.val, col = Strata, group = interaction(Strata, name, Strategy)), linetype = "solid", size = 1.7
    ) +
    geom_line(
      data = subset(combDat5, Strata %in% strata & Strategy == "counterfactual"),
      aes(x = year, y = mean.val, col = Strata, group = interaction(Strata, name)), size = 1.7
    ) +
    geom_point(
      data = subset(combDat5, Strata %in% strata &
        name != "OM" &
        year %in% c(2003:2010, 2016)),
      aes(x = year, y = mean.val, fill = Strata, group = interaction(Strata, name)), shape = 21, size = 3.5
    ) +
    geom_point(
      data = subset(combDat5, Strata %in% strata &
        name != "OM" &
        !(year %in% c(2003:2010, 2016))),
      aes(x = year, y = mean.val, group = interaction(Strata, name, Strategy)), shape = 21, size = 3.5, fill = "white"
    ) +
    geom_ribbon(data = subset(combDat5, Strata %in% strata &
      name != "OM" &
      year %in% c(2003:2016)), aes(x = year, ymin = lower.ci.val, ymax = upper.ci.val, fill = Strata), alpha = 0.1) +
    theme(legend.position = "none") +
    scale_color_manual(values = StrataCols) +
    scale_fill_manual(values = StrataCols) +
    scale_linetype_manual(values = c("dotdash", "solid", "dashed", "dotted")) +
    scale_x_continuous(limits = c(2003, EndYear), labels = s_labsy, breaks = c(2003:EndYear)) +
    labs(title = "", x = "", y = "", col = "Strata", linetype = "") +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic() +
    theme(legend.position = "none") +
    facet_wrap(~Strata) +
    theme(
      panel.spacing.x = unit(0, "line"),
      strip.text.x = element_text(size = 18, face = "bold", hjust = 0, vjust = 0),
      strip.text.y = element_text(size = 18, face = "bold"),
      strip.placement = "outside",
      strip.background = element_rect(colour = "white", fill = "white"),
      plot.subtitle = element_text(hjust = -0.25, face = "bold", size = 18),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18)
    )


  if (count == 3) temp <- temp + labs(y = expression(italic("PfPR")["2 to 10"] * " (%)"))
  if (count %in% c(1, 2, 3, 4)) {
    temp <- temp + theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }


  if (count == 1) temp1 <- temp
  if (count == 2) temp2 <- temp
  if (count == 3) temp3 <- temp
  if (count == 4) temp4 <- temp
  if (count == 5) temp5 <- temp

  rm(temp)
}

temptest2 <- plot_grid(temp1, temp2, temp3, temp4, temp5, ncol = 1, align = "v", rel_heights = c(1, 1, 1, 1, 1.5))
intMaps <- plot_grid(temp1, temp2, temp3, temp4, temp5, nrow = 1)


PR.simobs <- ggplot() +
  geom_hline(yintercept = c(1, 5, 10, 30), linetype = "dashed", color = "gray60") +
  geom_boxplot(
    data = combDat,
    aes(x = var, y = value, fill = Strata, group = interaction(var, Strata))
  ) +
  scale_fill_manual(values = StrataCols) +
  labs(title = "", x = "", y = expression(italic("PfPR")["2 to 10"] * " (%)"), fill = "Strata") +
  facet_grid(~Strata, scale = "free", space = "free", switch = "x") +
  scale_y_continuous(breaks = c(1, 5, seq(0, 50, 10)), labels = c(1, 5, seq(0, 50, 10)), expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none", plot.subtitle = element_text(size = 18, hjust = 0)) +
  customThemeNoFacet
ggsave("combined_PfPR_simobs_boxplot.png", plot = PR.simobs,
       path = file.path("figures"), width = 8, height = 5, device = "png")


PRbox <- ggplot() +
  geom_hline(yintercept = c(1, 5, 10, 30), linetype = "dashed", color = "gray60") +
  geom_boxplot(
    data = subset(JAGSresults_wide, !is.na(Strata) &
      FutScen_nr == 3 &
      year == 2016 &
      statistic == "median"),
    aes(x = fct_rev(Strata), y = PR, fill = Strata), width = 0.5
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = StrataCols) +
  labs(title = "", x = "", y = expression(italic("PfPR")["2 to 10"] * " (%)")) +
  scale_y_continuous(breaks = c(1, 5, 10, 30, 100, 500), labels = c(1, 5, 10, 30, 100, 500), expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none", plot.subtitle = element_text(size = 18, hjust = 0)) +
  labs(x = "\nbaseline prevalence", title = "", y = "") +
  customThemeNoFacet_angle +
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 14))


EIRbox <- ggplot() +
  geom_hline(yintercept = c(1, 5, 10, 30), linetype = "dashed", color = "gray60") +
  geom_boxplot(
    data = subset(JAGSresults_wide, !is.na(Strata) &
      FutScen_nr == 3 &
      year == 2016 &
      statistic == "median"),
    aes(x = Strata, y = EIR, fill = Strata), width = 0.5
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = StrataCols) +
  labs(title = "", subtitle = "", x = "", y = "annual EIR on log scale") +
  scale_y_continuous(trans = "log10", breaks = c(1, 5, 10, 30, 100, 500), labels = c(1, 5, 10, 30, 100, 500), expand = c(0, 0)) +
  theme(legend.position = "none", plot.subtitle = element_text(size = 18, hjust = 0)) +
  theme(
    plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 14),
    axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()
  )

ppEIRPR <- plot_grid(EIRbox, PR.simobs, nrow = 2, rel_widths = c(1, 1), labels = c("B)\n", "C)\n"), label_size = 22, hjust = 0, vjust = 1)
pall <- plot_grid(temptest2, ppEIRPR, nrow = 1, rel_widths = c(1, 0.7), labels = c("A)\n", ""), label_size = 22, hjust = 0, vjust = 1)

ggsave("timeline_simobs_boxplot.png", plot = pall,
       path = file.path('figures'), width = 16, height = 12, device = "png")

