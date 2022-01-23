cat(paste0('Start running Figure2.R'))

load(file.path("simdat", "AnalysisDat.RData"))
AnalysisDat <- AnalysisDat  %>% mutate(PR=PR*100)
NMSPdat_long <- f_load_nmsp_scendat()

## Either select counterfactual or take mean of future scenarios (that ran repeatedly for the past)
baselineDat <- AnalysisDat %>%
  dplyr::filter(statistic == 'median' & year == 2016) %>%
  #dplyr::filter(counterfactual==1) %>%
  group_by(Strata, District) %>%
  summarize(PR = mean(PR),
            Population_2016 = mean(Population_2016)) %>%
  aggregatDat(groupVars = "Strata", valueVar = "PR",
              weightVar = "Population_2016", WideToLong = FALSE, weightedAggr = F)

baselineDat$StrataLabel2 <- factor(baselineDat$Strata,
                                   levels = c("very low", "low", "urban", "moderate", "high"),
                                   labels = c("very low (6)", "low (5)", "urban (9)", "moderate (6)", "high (11)")
)

baselineDat$StrataLabel <- factor(baselineDat$Strata, levels = strata_lbl, labels = strata_lbl)

tempdat <- AnalysisDat %>%
  dplyr::select(FutScen, futSNPcov, FutScen_nr) %>%
  unique()


AnalysisDat2 <- inner_join(AnalysisDat, NMSPdat_long[, c('District', 'FutScen', 'Strategy', 'Strategy_FutScen_nr')])
dim(AnalysisDat2)
table(AnalysisDat2$Strategy, AnalysisDat2$CMincrease, exclude = NULL)
length(unique(NMSPdat_long$Strategy))
length(unique(AnalysisDat2$Strategy))

#ScenDat <- fread(file.path("simdat", "Figure2_strata.csv"))
source(file.path('rlibrary', 'get_explorative_strategies.R'))

ScenDat <- left_join(ScenDat, tempdat) %>% as.data.frame()
ScenDat$StrataLabel <- factor(ScenDat$Strata, levels = strata_lbl, labels = strata_lbl)
table(ScenDat$FutScen_label_noCM, exclude = NULL)


ScenDat <- ScenDat %>%
  mutate(futSNPcov = ifelse(futSNPcov == 0, '', paste0('(', futSNPcov * 100, '%)'))) %>%
  mutate(
    FutScen_label_noCM = gsub("+continuous", "SNP", FutScen_label_noCM),
    FutScen_label_noCM = gsub("ITN continuous", "ITN(SNP)", FutScen_label_noCM),
    FutScen_label_noCM = gsub("ITN MRC", "ITN(MRC)", FutScen_label_noCM),
    FutScen_label_noCM = gsub(" ", "", FutScen_label_noCM),
    FutScen_label_noCM = gsub("noCMonly", "CMonly", FutScen_label_noCM),
    FutScen_label_noCM = gsub("ITNSNP", "ITN(SNP)", FutScen_label_noCM),
    FutScen_label_noCM = paste0(FutScen_label_noCM, futSNPcov)
  )
unique(ScenDat$FutScen_label_noCM)


verylowFinal <- 77 # revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
lowFinal <- 125 # revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
urbanFinal <- c(73, 85, 133, 134) # revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
moderateFinal <- 137 #revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
highFinal <- c(102, 108) #revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM

pplot <- ggplot(data = baselineDat) +
  theme_cowplot() +
  geom_hline(yintercept = 0, color = "white", linetype = "dashed", size = 0.7) +
  geom_hline(yintercept = 0.01, color = "black", linetype = "solid", size = 0.7) +
  geom_rect(data = baselineDat, aes(ymin = lower.ci.val, ymax = upper.ci.val, xmin = -Inf, xmax = Inf), alpha = 0.4, fill = 'lightgrey') +
  geom_hline(data = baselineDat, aes(yintercept = mean.val), linetype = "dashed", size = 0.7) +
  geom_pointrange(data = ScenDat, aes(x = as.factor(FutScen_label_noCM),
                                      y = mean.val,
                                      ymin = lower.ci.val,
                                      ymax = upper.ci.val, alpha = CMincrease)) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "very low" & FutScen_nr %in% verylowFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val),
                  col = prevcolsAdj[2]) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "low" & FutScen_nr %in% lowFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val), col = prevcolsAdj[3]) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "urban" & FutScen_nr %in% urbanFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val), col = prevcolsAdj[1]) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "moderate" & FutScen_nr %in% moderateFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val), col = prevcolsAdj[4]) +
  geom_pointrange(data = subset(ScenDat, StrataLabel == "high" & FutScen_nr %in% highFinal),
                  aes(x = FutScen_label_noCM,
                      y = mean.val,
                      ymin = lower.ci.val,
                      ymax = upper.ci.val), col = prevcolsAdj[5]) +

  facet_wrap(~StrataLabel, scales = "free", ncol = 2) +
  coord_flip() +
  geom_hline(yintercept = c(Inf)) +
  theme(legend.position = "none") +
  geom_vline(xintercept = c(Inf, -Inf)) +
  scale_y_continuous(breaks=seq(0,25,0.5),labels=seq(0,25,0.5))+
  labs(y = expression(italic("PfPR")["2 to 10"] * ""),
       x = "Intervention packages per strata (unique number)") +
  theme(
    panel.spacing.x = unit(0, "line"),
    strip.text.x = element_text(size = 22, face = "bold"),
    strip.text.y = element_text(size = 22, face = "bold"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "black", fill = "white"),
    plot.subtitle = element_text(hjust = -0.25, face = "bold", size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22)
  )

pplot
ggsave("Fig_2.png", plot = pplot, path = 'figures', width = 14, height = 10, device = "png")
ggsave("Fig_2.pdf", plot = pplot, path = 'figures', width = 14, height = 10, device = "pdf")


### For text
fwrite(ScenDat, file.path('figures', 'figuredat', 'figure2_dat.csv'))
fwrite(baselineDat, file.path('figures', 'figuredat', 'baselineDat.csv'))


FutScenLow = c('0.6057272-0-0-0-addMDA-0', '@Access2016@-0-0-0-addMDA-0', '0.6057272-80-0-0-onlyCMandITN-0',
               '0.6057272-0-0-0-addLARV-0', '@Access2016@-80-0-0-onlyCMandITN-0',
               '@Access2016@-0-0-0-addLARV-0', '0.6057272-0-0-0-onlyCMandITN-0', '@Access2016@-0-0-0-onlyCMandITN-0'
)

AnalysisDat %>%
  dplyr::filter(year == 2020) %>%
  filter(Strata == 'very low') %>%
  filter(FutScen %in% FutScenLow) %>%
  aggregatDat(groupVars = "Strata", valueVar = "PR", weightVar = "Population_2016", WideToLong = FALSE,weightedAggr=weightedAggr)


AnalysisDat %>%
  dplyr::filter(year == 2020) %>%
  filter(Strata == 'very low') %>%
  filter(FutScen %in% FutScenLow) %>%
  aggregatDat(groupVars = c("Strata", "FutScen"), valueVar = "PR", weightVar = "Population_2016", WideToLong = FALSE,weightedAggr=weightedAggr)

