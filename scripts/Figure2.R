cat(paste0('Start running Figure2.R'))

load(file.path("simdat", "AnalysisDat.RData"))
AnalysisDat <- AnalysisDat %>% 
  filter(statistic == 'median') %>%
  mutate(PR=PR*100)

NMSPdat_long <- f_load_nmsp_scendat()

baselineDat <- AnalysisDat %>%
  dplyr::filter(baseline==1) %>%
  aggregatDat(groupVars = "Strata", valueVar = "PR",
              weightVar = "Population_2016", WideToLong = FALSE, weightedAggr = weightedAggr)

str(baselineDat)
baselineDat$StrataLabel2 <- factor(baselineDat$Strata,
                                   levels = c("very low", "low", "urban", "moderate", "high"),
                                   labels = c("very low (6)", "low (5)", "urban (9)", "moderate (6)", "high (11)")
)

baselineDat$StrataLabel <- factor(baselineDat$Strata, levels = strata_lbl, labels = strata_lbl)




### Results in multiple strategy rows per district and scenario
AnalysisDat <- AnalysisDat %>% filter(year==2020)
AnalysisDat$FutScen_label_noCM[AnalysisDat$FutScen_label_noCM=='no CM only'] <- 'counterfactual'

AnalysisDat2 <- full_join(AnalysisDat, NMSPdat_long[, c('District', 'FutScen', 'Strategy', 'Strategy_FutScen_nr')]) %>%
  filter(!(is.na(Strategy)))

any(duplicated(AnalysisDat2[,c('District','FutScen_nr','year','statistic','PR','FutScen','FutScen_label','Strategy')]))
dim(AnalysisDat2)
table(AnalysisDat2$Strategy, AnalysisDat2$CMincrease, exclude = NULL)
length(unique(NMSPdat_long$Strategy))
length(unique(AnalysisDat2$Strategy))


tempdat <- AnalysisDat2 %>%
  dplyr::select(FutScen, futSNPcov, FutScen_nr) %>%
  unique()

source(file.path('rlibrary', 'get_explorative_strategies.R'))

ScenDat <- left_join(ScenDat, tempdat) %>% as.data.frame()
ScenDat$StrataLabel <- factor(ScenDat$Strata, levels = strata_lbl, labels = strata_lbl)

table(ScenDat$FutScen_label_noCM, exclude = NULL)
ScenDat$FutScen_label_noCM_old <- ScenDat$FutScen_label_noCM
#ScenDat$FutScen_label_noCM <- ScenDat$FutScen_label_noCM_old

ScenDat %>% dplyr::select(Strata,FutScen_label_noCM, CMincrease) %>% 
  unique() %>%
  group_by(Strata,CMincrease) %>% 
  tally() %>%
  pivot_wider(names_from=CMincrease, values_from=n)


## selected intervention mixes per strata
verylowFinal <- 77 # revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
lowFinal <- 125 # revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
urbanFinal <- c(73, 85, 133, 134) # revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
moderateFinal <- 137 #revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM
highFinal <- c(102, 108) #revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM

custom_plot <- function(base_df, scen_df,selected_FutScen_nr, col_index,facet_colors=TRUE){
  pplot <- ggplot(data = base_df) +
    theme_cowplot() +
    geom_hline(yintercept = 0, color = "white", linetype = "dashed", size = 0.7) +
    geom_hline(yintercept = 1, color = "black", linetype = "solid", size = 0.7) +
    geom_rect(aes(ymin = lower.ci.val, ymax = upper.ci.val, xmin = -Inf, xmax = Inf), alpha = 0.45, fill = 'lightgrey') +
    geom_hline(aes(yintercept = mean.val), linetype = "dashed", size = 0.7) +
    geom_pointrange(data = scen_df, aes(x = as.factor(FutScen_label_noCM),
                                        y = mean.val,
                                        ymin = lower.ci.val,
                                        ymax = upper.ci.val, col = CMincrease)) +
    geom_pointrange(data = subset(scen_df, FutScen_nr %in% selected_FutScen_nr),
                    aes(x = FutScen_label_noCM,
                        y = mean.val,
                        ymin = lower.ci.val,
                        ymax = upper.ci.val),
                    col = prevcolsAdj[col_index]) +
    facet_wrap(~Strata, scales = "free", ncol = 2) +
    scale_color_manual(values=c('grey','black'))+
    coord_flip() +
    geom_hline(yintercept = c(Inf)) +
    theme(legend.position = "none") +
    geom_vline(xintercept = c(Inf, -Inf)) +
    labs(x='',y = expression(italic("PfPR")["2 to 10"] * "")) +
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
  
  return(pplot)
}


facet_color <- function(p,col_index){
  g <- ggplot_gtable(ggplot_build(p))
  strip_both <- which(grepl("strip-", g$layout$name))
  fills <- prevcolsAdj[col_index]
  k <- 1
  for (i in strip_both) {
    j <- which(grepl("rect", g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  #grid.draw(g)
 return(g)
  
}

pp1 <- custom_plot(base_df=subset(baselineDat,Strata=='very low'),
                   scen_df=subset(ScenDat,Strata=='very low'),
                   selected_FutScen_nr=verylowFinal, 
                   col_index=2) +labs(y='')


pp2 <- custom_plot(base_df=subset(baselineDat,Strata=='low'),
                   scen_df=subset(ScenDat,Strata=='low'),
                   selected_FutScen_nr=lowFinal, 
                   col_index=3) + scale_y_continuous(breaks=seq(0,7,1),
                                                    labels=seq(0,7,1))+
  labs(y='',x = "Intervention packages per strata (unique number)")

pp3 <- custom_plot(base_df=subset(baselineDat,Strata=='urban'),
            scen_df=subset(ScenDat,Strata=='urban'),
            selected_FutScen_nr=urbanFinal, 
            col_index=1) + scale_y_continuous(breaks=seq(0,13,2),
                                             labels=seq(0,13,2)) 
  
pp4 <- custom_plot(base_df=subset(baselineDat,Strata=='moderate'),
                   scen_df=subset(ScenDat,Strata=='moderate'),
                   selected_FutScen_nr=moderateFinal, 
                   col_index=4) +labs(y='')+ scale_y_continuous(breaks=seq(0,17,2),
                                                    labels=seq(0,17,2))

pp5 <- custom_plot(base_df=subset(baselineDat,Strata=='high'),
                   scen_df=subset(ScenDat,Strata=='high'),
                   selected_FutScen_nr=highFinal, 
                   col_index=5) + scale_y_continuous(breaks=seq(0,27,5),
                                                    labels=seq(0,27,5))
## add colors
pp1 <- facet_color(pp1,2)
pp2 <- facet_color(pp2,3)
pp3 <- facet_color(pp3,1)
pp4 <- facet_color(pp4,4)
pp5 <- facet_color(pp5,5)

pplot <- plot_grid(pp1,pp4,pp2,pp5, pp3, ncol=2, align='hv')
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

