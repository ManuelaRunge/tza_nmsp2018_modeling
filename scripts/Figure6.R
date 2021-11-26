## ===================================================================
## Figure 6
## ===================================================================
library(data.table)
library(tidyverse)
library(spatstat) #weighted.median

source(file.path("rlibrary", "customObjects.R"))
source(file.path("rlibrary", "f_AggrDat.R"))

selectedStrategies <- c("NMSPcurrent.withCM", "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM")

load(file.path("dat", "AnalysisDat2.RData"))
AnalysisDat$Cases.pP <- AnalysisDat$Cases / simPop
AnalysisDat$incidence <- (AnalysisDat$Cases / simPop) * 1000


## PRdiff.baseline.perc
Disdat1 <- AnalysisDat %>%
  dplyr::filter(year == 2020 &
                  FutScen_nr == Strategy_FutScen_nr &
                  Strategy %in% selectedStrategies) %>%
  dplyr::select(District, statistic, Population_2016, StrataLabel, Strategy, PR) %>%
  pivot_wider(names_from=Strategy,values_from=PR) %>%
  mutate(outcome = "PR",
    relNMSPoldnew = (NMSPcurrent.withCM / revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM)
  ) %>%
  as.data.frame()


Disdat2 <- AnalysisDat2 %>%
  dplyr::filter(year == 2020 &
                  FutScen_nr == Strategy_FutScen_nr &
                  Strategy %in% selectedStrategies) %>%
  dplyr::select(District, statistic, Population_2016, StrataLabel, Strategy, Cases.pP) %>%
  spread(Strategy, Cases.pP) %>%
  mutate(
    outcome = "Cases.pP",
    relNMSPoldnew = (NMSPcurrent.withCM / revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM)
  ) %>%
  as.data.frame()

Disdat2b <- AnalysisDat2 %>%
  dplyr::filter(year == 2020 &
                  Strategy_FutScen_nr == Strategy_FutScen_nr &
                  Strategy %in% selectedStrategies) %>%
  dplyr::select(District, statistic, Population_2016, StrataLabel, Strategy, incidence) %>%
  spread(Strategy, incidence) %>%
  mutate(
    outcome = "incidence",
    relNMSPoldnew = (NMSPcurrent.withCM / revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM)
  ) %>%
  as.data.frame()


Disdat <- rbind(Disdat1, Disdat2, Disdat2b)
Disdat$newBetterThanOld <- "old better"
Disdat$newBetterThanOld[Disdat$revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM < Disdat$NMSPcurrent.withCM] <- "new better"

# ggplot(data=Disdat) + geom_boxplot(aes(x=Strata, y=))
table(Disdat$outcome[Disdat$statistic == "median"], Disdat$newBetterThanOld[Disdat$statistic == "median"])

Disdat <- Disdat %>%
  dplyr::group_by(District, statistic, Population_2016) %>%
  dplyr::arrange(District, statistic, Population_2016) %>%
  dplyr::mutate(Levels = ifelse(nlevels(as.factor(newBetterThanOld)) > 1, "No", "Yes"))

table(Disdat$outcome[Disdat$statistic == "median"], Disdat$Levels[Disdat$statistic == "median"])

Disdat$outcomen <- factor(Disdat$outcome,
                          levels = c("Cases.pP", "PR", "incidence"),
                          labels = c('italic("") * "Cases pP"', 'italic("PfPR")["2 to 10"] * " (%)"', 'incidence')
)

### calculate mean per strata to be added in plot
DisdatAggr <- Disdat %>%
  dplyr::group_by(StrataLabel, statistic, outcome, outcomen) %>%
  dplyr::summarize(
    relNMSPoldnewAggr = mean(relNMSPoldnew),
    relNMSPoldnewAggr_w = weighted.mean(relNMSPoldnew, w = Population_2016)
  )

Disdat <- left_join(Disdat, DisdatAggr)

DisdatScatter <- Disdat %>%
  dplyr::select(-c(relNMSPoldnew, newBetterThanOld, Levels, relNMSPoldnewAggr, relNMSPoldnewAggr_w)) %>%
  f_spread("statistic", c("NMSPcurrent.withCM", "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM"))

### add arrow indicated that the plot was truncated at -2 (goes up to -6)
### to do add errorbars from posterior for each district?


table(Disdat$outcome)
pplot <- ggplot(data = subset(Disdat, outcome != 'Cases.pP' & statistic == "median")) +
  theme_cowplot() +
  geom_errorbarh(aes(
    x = (NMSPcurrent.withCM / NMSPcurrent.withCM),
    xmin = (NMSPcurrent.withCM / NMSPcurrent.withCM),
    xmax = (relNMSPoldnew), col = StrataLabel, y = reorder(District, as.numeric(StrataLabel))
  ), alpha=0.75, size = 1.0, height = 0, show.legend = F) +
    geom_line(aes(
    x = relNMSPoldnewAggr,col=StrataLabel, group = StrataLabel, y = reorder(District, as.numeric(StrataLabel))
  ), size = 1.2, show.legend = F) + #, col = "black"
  geom_line(aes(
    x = relNMSPoldnewAggr_w,col=StrataLabel,  group = StrataLabel, y = reorder(District, as.numeric(StrataLabel))
  ), size = 1.0, linetype="dotdash", show.legend = F) + #, col = "grey"
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = 1) +
  scale_x_continuous(lim=c(-0.2,3.5),expand=c(0,0))+
  scale_y_discrete(labels = c()) +
  theme(legend.position = "bottom", axis.ticks.y = element_blank()) +
  scale_fill_manual(values = c("black", "white")) +
  scale_color_manual(values = StrataCols) +
  facet_wrap(~outcomen, scales = "free_x", strip.position = "bottom", labeller = labeller(.cols = label_parsed)) +
  theme(
    strip.text.x = element_text(size = 18),
    panel.spacing.x = unit(0, "line"),
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
       y = "Councils sorted by strata (n=184)",
       fill = "Agreement between outcomes")


ggsave(paste0("Fig_6.png"), plot = pplot, path = file.path("figures"), width = 10, height = 12, device = "png")
ggsave(paste0("Fig_6.pdf"), plot = pplot, path = file.path("figures"), width = 10, height = 12, device = "pdf")



