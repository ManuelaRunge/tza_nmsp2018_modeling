cat(paste0('Start running Figure3.R'))

selectedOutcome <- "PR"  # "Cases.pP"

load(file.path("simdat", "AnalysisDat.Rdata"))
AnalysisDat <- AnalysisDat %>% 
  filter(Strata %in% c('very low','low','urban')) %>%
  filter((year == 2022 | year == 2020 | year == 2016)) %>%
  mutate(PR=PR*100)
str(AnalysisDat)

FutNrDat <- AnalysisDat %>%
  filter(FutureScenarios == "onlyCMandITN" &
           IPTcov == 0 &
           futIRScov == 0) %>%
  dplyr::select(FutScen_nr, FutScen_label) %>%
  unique()

FutNrDat <- FutNrDat %>% filter(FutScen_label %in% c('increase in CM - ITN MRC','increase in CM')) 
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

table(tempdat$FutScen_label, tempdat$withWithoutNets, exclude=NULL)

### aggregate LLIN group - take mean --- or generate ranges across all LLIN scenarios?
groupVARS <- c("District", "statistic", "Strata", "Population_2016", "withWithoutNets", "year")
tempdatForDiff <- tempdat %>%
  dplyr::group_by_at(.vars = groupVARS) %>%
  dplyr::summarize(PR = mean(PR), EIR=mean(EIR))

## calculate difference in CM vs CM+LLIN
groupVARS <- c("District", "statistic", "Strata","Population_2016", "year")
tempdatForDiff <- data.table(tempdatForDiff, key = groupVARS)
tempdatForDiff[, CMITNdiff := PR[withWithoutNets == "CM only"] - PR[withWithoutNets == "+ ITNs"], by = groupVARS]
tempdatForDiff[, CMITNdiff_perc := ((PR[withWithoutNets == "CM only"] - PR[withWithoutNets == "+ ITNs"]) / PR[withWithoutNets == "CM only"]) * 100, by = groupVARS]

## transform back to data frame and then aggregate from council to strata
groups <- c("statistic", "Strata", "withWithoutNets", "year")
tempdatForDiffAggr <- tempdatForDiff %>%
  filter(statistic=='median') %>%
  aggregatDat(groups, "CMITNdiff", "Population_2016", weightedAggr = weightedAggr)

#tempdatForDiffAggr %>% filter(year == 2020)
table(tempdatForDiff$District, tempdatForDiff$withWithoutNets)

### add n districts
nDisSTrata <- tempdatForDiff %>%
  dplyr::select(District, Strata) %>%
  unique() %>%
  dplyr::group_by(Strata) %>%
  tally()
sum(nDisSTrata$n)

### Creae label variable
tempdatForDiff$StrataRev_adj <- as.character(tempdatForDiff$Strata)
tempdatForDiff$StrataRev_adj <- factor(tempdatForDiff$StrataRev_adj,
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

groups <- c("statistic", "Strata",  "StrataRev_adj", "withWithoutNets", "year")
plotdat <- tempdatForDiff %>%
  filter(statistic == "median") %>%
  aggregatDat(groupVars = groups, valueVar = selectedOutcome, weightVar = "Population_2016", weightedAggr = weightedAggr)


plotdat$withWithoutNets_year <- paste0(plotdat$withWithoutNets, ' ', plotdat$year)
withWithoutNets_year_cols <- c("CM only 2016" = "#7FD8F6",
                               "CM only 2020" = "#00B2EE",
                               "CM only 2022" = "#007CA6",
                               "+ ITNs 2016" = "#F4AC66",
                               "+ ITNs 2020" = "#EE7600",
                               "+ ITNs 2022" = "#BE5E00")


get_pplotSmall <- function(df){
pplot <- ggplot(data=df) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_cowplot() +
  geom_bar( aes(x = withWithoutNets, y = mean.val, fill = withWithoutNets_year, group = year),
           stat = "identity", position = position_dodge(width = 0.5), width = 0.5, size = 0.7) +
  geom_errorbar(aes(x = withWithoutNets, ymin = lower.ci.val, ymax = upper.ci.val, group = year),
                stat = "identity", position = position_dodge(width = 0.5), width = 0.5, size = 0.7, col = "black") +
  scale_fill_manual(values = withWithoutNets_year_cols) +
  labs(y = selectedOutcomeLabel, colour = "", fill = "", x = "") +
  labs( subtitle = "") +
  theme(legend.position = "right") +
  facet_wrap(~StrataRev_adj, scales = "free") +
  customTheme_noAngle
 return(pplot)
}

p1 <- get_pplotSmall(subset(plotdat, Strata == "very low")) + scale_y_continuous(lim=c(0,3.5))
p2 <- get_pplotSmall(subset(plotdat, Strata == "low")) + scale_y_continuous(lim=c(0,12), breaks=seq(0,12,1), labels=seq(0,12,1)) +theme(legend.position = 'None')
p3 <- get_pplotSmall(subset(plotdat, Strata == "urban")) + scale_y_continuous(lim=c(0,16))+theme(legend.position = 'None')
plegend <- get_legend(p1)
p1 <- p1 +theme(legend.position = 'None')
pplot <- plot_grid(p1,p2,p3, nrow=1, align='hv')
pplotSmall <- plot_grid(pplot,plegend, rel_widths = c(1,0.25))


###-----------------------------------------
### very low risk strata by EIR

groupVARS <- c("District", "statistic", "Strata_withoutUrban", "Population_2016", "withWithoutNets", "year")
tempdatForDiff <- tempdat %>%
  filter(Strata_withoutUrban == "Very low") %>%
  dplyr::group_by_at(.vars = groupVARS) %>%
  dplyr::summarize(PR = mean(PR), EIR=mean(EIR))


tempdatForEIR <- tempdatForDiff %>%  
  ungroup( )%>%
  filter(withWithoutNets=='CM only' & year==2016) %>%
  dplyr::select(District,statistic, EIR)

tempdatForEIR$EIRgrp <- cut(tempdatForEIR$EIR, c(-Inf, 2, 5, 10, 20, 40, 80, Inf),
                      labels = c("EIR <2", "EIR 2-5", "EIR 5-10", "EIR 10-20", "EIR 20-40", "EIR 40-80", "EIR >80"))
tempdatForEIR$EIRgrp_adj <- cut(tempdatForEIR$EIR, c(-Inf, 2, 5, 10, Inf),
                          labels = c("EIR <2", "EIR 2-5", "EIR 5-10", "EIR > 10"))


d <- tempdatForEIR %>%
  ungroup() %>%
  dplyr::select(  District,  EIRgrp_adj) %>%
  unique() %>%
  group_by(  EIRgrp_adj) %>%
  arrange( EIRgrp_adj) %>%
  tally()

tempdatForEIR$EIRgrp_adj <- factor(tempdatForEIR$EIRgrp_adj,
                             levels = c("EIR <2", "EIR 2-5", "EIR 5-10", "EIR > 10"),
                             labels = c(
                               paste0("EIR <2\n(n=", d$n[d$EIRgrp_adj == "EIR <2"], ")"),
                               paste0("EIR 2-5\n(n=", d$n[d$EIRgrp_adj == "EIR 2-5"], ")"),
                               paste0("EIR 5-10\n(n=", d$n[d$EIRgrp_adj == "EIR 5-10"], ")"),
                               paste0("EIR > 10\n(n=", d$n[d$EIRgrp_adj == "EIR > 10"], ")")
                             )
)

table(tempdatForEIR$EIRgrp_adj, tempdatForEIR$EIRgrp, exclude = NULL)


tempdatForDiff <- tempdatForDiff %>% dplyr::select(-EIR) %>% left_join(tempdatForEIR)

#### Prepare main plot - show very low only
table(tempdatForDiff$withWithoutNets, tempdatForDiff$Strata_withoutUrban)
groups <- c("Strata_withoutUrban", "EIRgrp_adj", "withWithoutNets", "year")

### spread statistic
tempdat_stat <- tempdatForDiff %>%
  ungroup() %>%
  dplyr::select(EIR, year, Strata_withoutUrban,  District, withWithoutNets, statistic, PR) %>%
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
  geom_smooth( show.legend = FALSE, se = FALSE, span=1) +
  facet_wrap(~year, nrow = 1, scales = "free_y") +
  scale_x_continuous(trans = "log", breaks = c(1:5, 10, 20), labels = c(1:5, 10, 20)) +
  scale_y_continuous(breaks = seq(0, 20, 2), labels = seq(0, 20, 2)) +
  scale_color_manual(values = TwoCols) +
  scale_fill_manual(values = TwoCols) +
  labs(y = selectedOutcomeLabel, colour = "", fill = "", x = "",
       subtitle = "", x = "pre-intervention EIR (log-scale)") +
  theme(legend.position = "right") +
  customTheme_noAngle

#pplotSmall <- pplotSmall + theme(legend.position = 'None')
#pplotMain <- pplotMain + theme(legend.position = 'None')
(Figure3 <- plot_grid(pplotSmall, pplotMain, nrow = 2, align='hv', labels=c('A)','B)')))

ggsave(paste0("Fig_3.png"), plot = Figure3, path = file.path("figures"), width = 13, height = 8, dpi = 200, device = "png")
#ggsave(paste0("Fig_3.pdf"), plot = Figure3, path = file.path("figures"), width = 13, height = 8, dpi = 200, device = "pdf")


### Save datasetss
fwrite(plotdat, file = file.path("figures", 'figuredat', paste0("Fig3_pplotSmall_", selectedOutcome, ".csv")))
fwrite(tempdat, file = file.path("figures", 'figuredat', paste0("Fig3_tempdat_", selectedOutcome, ".csv")))
fwrite(tempdat_stat, file = file.path("figures", 'figuredat', paste0("Fig3_tempdat_stat", selectedOutcome, ".csv")))


#### For text
plotdat %>%
  ungroup() %>%
  dplyr::filter(statistic == "median") %>%
  dplyr::select(Strata,  withWithoutNets, year, mean.val, lower.ci.val, upper.ci.val, weighted) %>%
  fwrite(file.path('figures', 'figuredat', 'Fig3A_fortext.csv'))


