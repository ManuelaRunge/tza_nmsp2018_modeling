## -----------------------------------------
#### Figure 4 - IPTsc in high (and moderate) strata
## -----------------------------------------

require(data.table)
require(tidyverse)
require(cowplot)

library(spatstat)
source(file.path("rlibrary", "f_spread.R"))
source(file.path("rlibrary", "f_aggrDat.R"))

theme_set(theme_cowplot())
fig5cols <- c('#00B2EE', '#8DC63F', '#EE7600', '#C53F42', '#628A2C', '#C38B4B', '#614525', '#9D3234')
simPop = 10000
TwoCols<- c("deepskyblue2", "darkorange2")
load(file.path("simdat", "AnalysisDat.RData"))



## -----------------------------------------
### Figure 3  - impact in low strata - discontinuation of nets
## -----------------------------------------
### show plots next to each other

#### additonal impact  of ITN per strata
FutNrDat <- AnalysisDat %>%
  filter(
    FutureScenarios == "onlyCMandITN" &
      IPTcov == 0 &
      futIRScov == 0
  ) %>%
  dplyr::select(FutScen_nr, FutScen_label) %>%
  unique()

### generate variables for nets, regardless of other interventions (done before)
## -> only CM  or CM+ITN not additional "background interventions!"  #&futSNPcov==0
##    and no combination of both nets
tempdat <- subset(AnalysisDat, (year == 2022 | year == 2020 | year == 2016) &
                    FutScen_nr %in% FutNrDat$FutScen_nr &
                    FutScen_label != "ITN MRC continuous")

### add label variable
tempdat$withWithoutNets <- "CM only"
tempdat$withWithoutNets[grep("ITN", tempdat$FutScen_label)] <- "+ ITNs"
tempdat$withWithoutNets <- factor(tempdat$withWithoutNets, levels = c("CM only", "+ ITNs"), labels = c("CM only", "+ ITNs")) ## + any nets

### aggregate LLIN group - take mean --- or generate ranges across all LLIN scenarios?
groupVARS <- c("District", "statistic", "Stratification.5b", "Population_2016", "StrataLabel", "withWithoutNets", "year")
tempdatForDiff <- tempdat %>%
  dplyr::group_by_at(groupVARS) %>%
  dplyr::summarize(PR = mean(PR))

## calculate difference in CM vs CM+LLIN
groupVARS <- c("District", "statistic", "Stratification.5b", "Population_2016", "StrataLabel", "year")
tempdatForDiff <- data.table(tempdatForDiff, key = groupVARS)

tempdatForDiff[, CMITNdiff := PR[withWithoutNets == "CM only"] - PR[withWithoutNets == "+ ITNs"], by = groupVARS]
tempdatForDiff[, CMITNdiff_perc := ((PR[withWithoutNets == "CM only"] - PR[withWithoutNets == "+ ITNs"]) / PR[withWithoutNets == "CM only"]) * 100, by = groupVARS]


## transform back to data frame and then aggregate from council to strata
tempdatForDiff <- as.data.frame(tempdatForDiff)

groups <- c("statistic", "Stratification.5b", "StrataLabel", "withWithoutNets", "year")

tempdatForDiffAggr <- tempdatForDiff %>%
  filter(StrataLabel %in% c("very low", "low", "urban")) %>%
  f_weighted.aggrDat(groups, "CMITNdiff", "Population_2016", FALSE)


tempdatForDiffAggr %>% filter(year == 2020)
table(tempdat$District, tempdat$withWithoutNets)

### add n districts!!
nDisSTrata <- tempdat %>%
  dplyr::select(District, StrataLabel) %>%
  unique() %>%
  group_by(StrataLabel) %>%
  tally()
sum(nDisSTrata$n)

### Creae label variable
tempdat$StrataRev_adj <- as.character(tempdat$Stratification.5b)
tempdat$StrataRev_adj <- factor(tempdat$StrataRev_adj,
                                levels = c("very low", "low", "urban"),
                                labels = c(
                                  paste0("very low\n(n=", nDisSTrata$n[nDisSTrata$StrataLabel == "very low"], ")")    ,                            
                                  paste0("low\n(n=", nDisSTrata$n[nDisSTrata$StrataLabel == "low"], ")"),
                                  paste0("urban\n(n=", nDisSTrata$n[nDisSTrata$StrataLabel == "urban"], ")")
                                )
)

table(tempdat$StrataRev_adj, tempdat$Stratification.5b, exclude = NULL)


#### Aggregate
### "StrataLabel","Strata","Stratification.5b_withoutUrban","Stratification.5b",
selectedOutcome <- "PR" # "Cases.pP"  ## "PR","PRdiff.baseline" , "PRdiff.baseline.perc"
if (selectedOutcome == "PR") selectedOutcomeLabel <- expression(italic("PfPR")["2 to 10"] * " (%)")
if (selectedOutcome == "Cases.pP") selectedOutcomeLabel <- "Cases pP"

groups <- c("statistic", "Stratification.5b", "StrataRev_adj", "withWithoutNets", "year")
plotdat <- tempdat %>%
  filter( statistic == "median" & 
            StrataLabel %in% c("very low", "low", "urban")) %>%
  f_weighted.aggrDat(groups, selectedOutcome, "Population_2016", FALSE)

plotdat_unw <- tempdat %>%
  filter( statistic == "median" & 
            StrataLabel %in% c("very low", "low", "urban")) %>%
  f_aggrDat(groups, selectedOutcome,  FALSE)

groups <- c("District", "statistic", "Stratification.5b", "StrataRev_adj", "withWithoutNets", "year")
plotdata <- tempdat %>%
  filter(StrataLabel %in% c("very low", "low", "urban")) %>%
  f_weighted.aggrDat(groups, selectedOutcome, "Population_2016", FALSE)



pplotSmall <- ggplot() +
  geom_hline(yintercept = c(1), linetype = "dashed") +
  theme_cowplot() +
  geom_bar(
    data = subset(plotdat_unw, statistic == "median"),
    aes(x =as.factor(withWithoutNets) , y = mean.val, fill = as.factor(year)  , group = year),
    stat = "identity", position = position_dodge(width = 0.5), width = 0.5, size = 0.7
  ) +
  geom_errorbar(
    data = subset(plotdat_unw, statistic == "median"),
    aes(x = as.factor(withWithoutNets), ymin = lower.ci.val, ymax = upper.ci.val, group = year),
    stat = "identity", position = position_dodge(width = 0.5), width = 0.5, size = 0.7, col = "black"
  ) +
  scale_fill_manual(values =  ThreeCols_seq) +
  labs(y = selectedOutcomeLabel, colour = "", fill = "", x = "") +
  labs(title = "A)", subtitle = "") +
  theme(
    legend.position =
      "right"
  ) +
  facet_wrap(~StrataRev_adj, scales="free") +
  customTheme_noAngle


### add n districts
### very low  by EIR
tempdat$EIRgrpLabel_adj <- as.character(tempdat$EIRgrpLabel)
tempdat$EIRgrpLabel_adj[tempdat$EIRgrpLabel_adj %in% c("EIR 10-20", "EIR 20-40", "EIR 40-80", "EIR >80")] <- "EIR > 10"

d <- tempdat %>%
  filter(year == 2016 &
           FutScen_nr == counterfactualInterventions &
           Stratification.5b_withoutUrban == "Very low") %>%
  dplyr::select(FutScen_nr, District, year, Stratification.5b_withoutUrban, EIRgrpLabel_adj) %>%
  unique() %>%
  group_by(FutScen_nr, Stratification.5b_withoutUrban, year, EIRgrpLabel_adj) %>%
  arrange(FutScen_nr, Stratification.5b_withoutUrban, EIRgrpLabel_adj) %>%
  tally()

tempdat$EIRgrpLabel_adj <- factor(tempdat$EIRgrpLabel_adj,
                                  levels = c("EIR <2", "EIR <5", "EIR 5-10", "EIR > 10"),
                                  labels = c(
                                    paste0("EIR <2\n(n=", d$n[d$EIRgrpLabel_adj == "EIR <2"], ")"),
                                    paste0("EIR 2-5\n(n=", d$n[d$EIRgrpLabel_adj == "EIR <5"], ")"),
                                    paste0("EIR 5-10\n(n=", d$n[d$EIRgrpLabel_adj == "EIR 5-10"], ")"),
                                    paste0("EIR > 10\n(n=", d$n[d$EIRgrpLabel_adj == "EIR > 10"], ")")
                                  )
)

table(tempdat$EIRgrpLabel_adj, tempdat$EIRgrpLabel, exclude = NULL)


#### Prepare main plot - show very low only
table(tempdat$withWithoutNets ,tempdat$Stratification.5b_withoutUrban     )
groups <- c("Stratification.5b_withoutUrban", "EIRgrpLabel_adj", "withWithoutNets", "year")
plotdat2 <- tempdat %>%
  f_weighted.aggrDat(groups, selectedOutcome, "Population_2016", FALSE) %>%
  filter(Stratification.5b_withoutUrban == "Very low")
summary(plotdat2$n.val)

groups <- c("District", "Stratification.5b_withoutUrban", "EIRgrpLabel_adj", "withWithoutNets", "year") ## aggregate ITN grp
plotdat2a <- tempdat %>%
  f_weighted.aggrDat(groups, selectedOutcome, "Population_2016", FALSE) %>%
  filter(Stratification.5b_withoutUrban == "Very low")
summary(plotdat2a$n.val)

### spread statistic
tempdat_stat <- tempdat %>%
  dplyr::select(EIR, year, Stratification.5b_withoutUrban, FutScen_nr, District, withWithoutNets, statistic, PR) %>%
  f_spread(statistic, c("PR", "EIR"))

pplotMain <- ggplot(
  data = subset(tempdat_stat, Stratification.5b_withoutUrban == "Very low"),
  aes(
    x = median_EIR, y = median_PR,
    ymin = q2.5_PR, ymax = q97.5_PR,
    xmin = q2.5_EIR, xmax = q97.5_EIR,
    col = as.factor(withWithoutNets),
    fill = as.factor(withWithoutNets)
  )
) +
  geom_hline(yintercept = c(1), linetype = "dashed") +
  theme_cowplot() +
  geom_errorbar(size = 0.7, alpha = 0.5) +
  geom_errorbarh(size = 0.7, alpha = 0.5) +
  geom_point(shape = 21, size = 2, col = "black", alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3, raw = TRUE), show.legend = FALSE, se = FALSE) +
  customTheme_noAngle +
  scale_color_manual(values = TwoCols) +
  scale_fill_manual(values = TwoCols) +
  labs(y = selectedOutcomeLabel, colour = "", fill = "", x = "") +
  labs(title = "B)", subtitle = "", x = "pre-intervention EIR (log-scale)") +
  theme(legend.position = "right") +
  facet_wrap(~year, nrow = 1, scales = "free_y") +
  scale_x_continuous(trans = "log", breaks = c(1:5, 10, 20), labels = c(1:5, 10, 20)) +
  scale_y_continuous(breaks = seq(0, 20, 2), labels = seq(0, 20, 2))




Figure3 <- plot_grid(pplotSmall, pplotMain,nrow=2)
Figure3

if (SAVE) {
  ggsave(paste0("Figure3_v2.png"), plot = Figure3, path = file.path( "figures"), width = 13, height = 8, dpi = 200, device = "png")  
  ggsave(paste0("Figure3_v2.pdf"), plot = Figure3, path = file.path( "figures"), width = 13, height = 8, dpi = 200, device = "pdf")  
}




### Save dataset
#fwrite(plotdat2, file = file.path( "figures", paste0("Figure3_pplotMain_", selectedOutcome, ".csv")))
#fwrite(plotdat, file = file.path( "figures", paste0("Figure3_pplotSmall_", selectedOutcome, ".csv")))
#fwrite(tempdat, file = file.path( "figures", paste0("Figure3_tempdat_", selectedOutcome, ".csv")))


#### Increase for text
dat <- subset(plotdat_unw, statistic == "median") %>% select( Stratification.5b, withWithoutNets,year,mean.val,lower.ci.val,upper.ci.val )

dat_2016 <- dat %>% filter(year==2016) %>% select(-year)
dat_2020 <- dat %>% filter(year==2020) %>% select(-year)
dat_2022 <- dat %>% filter(year==2022) %>% select(-year)

colnames(dat_2016)[c(3:5)] <- paste0(colnames(dat_2016)[c(3:5)],"_2016")
colnames(dat_2020)[c(3:5)] <- paste0(colnames(dat_2020)[c(3:5)],"_2020")
colnames(dat_2022)[c(3:5)] <- paste0(colnames(dat_2022)[c(3:5)],"_2022")

dat <- dat_2016 %>% left_join(dat_2020)%>% left_join(dat_2022) %>%
fwrite(dat, file.path("figures","Figure3A_fortext.csv"))


