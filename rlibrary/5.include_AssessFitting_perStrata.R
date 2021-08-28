## To be included in main analysis script  (~Projects\OM_TZA\Scripts\ExperimentAnalysis\5_analysis.Rmd)
## source(paste0(ScriptDir, "5.include_FittingPlots.R"))

## prepare data 
## combine observed and simulated data to one dataframe
prevCordat1 <- ds.obs_long %>%
  dplyr::filter(AgeGroup == "2to10" & year >= 2003 & year <= 2017) %>%
  dplyr::mutate(estimatesource = "obs", PR = value * 100, PRlo = NA, PRup = NA) %>%
  dplyr::select(District, year, PR, PRlo, PRup, estimatesource) %>%
  as.data.frame

### take mean of future scenarios for baselne , instead of just one future scenario		
#use median and credible intervals
prevCordat2 <- JAGSresults %>%
  dplyr::filter(outcome == "PR") %>%
  dplyr::group_by(District, year) %>%
  dplyr::summarize(PR = mean(median) * 100, PRlo = mean(q2.5) * 100, PRup = mean(q97.5) * 100) %>%
  dplyr::mutate(estimatesource = "sim") %>%
  dplyr::select(District, year, PR, PRlo, PRup, estimatesource) %>%
  as.data.frame()

prevCordat <- rbind(prevCordat1, prevCordat2)
prevCordat$District <- as.character(prevCordat$District)
prevCordat <- merge(prevCordat, TZADistrictDat[, c("Region", "District", "urbanrural", "MIS_UMRC")], by = "District", all.x = TRUE)
rm(prevCordat1, prevCordat2)

## long to wide format
prevCordat_wide <- reshape(prevCordat, idvar = c("Region", "District", "urbanrural", "MIS_UMRC", "year"), timevar = "estimatesource", direction = "wide")

### Add label variables 
prevCordat_wide$timeperiod <- "Historical trend"
prevCordat_wide$timeperiod[prevCordat_wide$year == baselineYear] <- "Baseline"
prevCordat_wide$timeperiod[prevCordat_wide$year == MonitoringStart] <- "Pre-intervention year"

prevCordat_wide$timeperiod2 <- "Historical trend"
prevCordat_wide$timeperiod2[prevCordat_wide$year == baselineYear] <- "Baseline year (2016)"
prevCordat_wide$timeperiod2[prevCordat_wide$year == MonitoringStart] <- "Pre-intervention year (2003)"

prevCordat_wide$timeperiod2 <- factor(prevCordat_wide$timeperiod2,
                                      levels = c("Pre-intervention year (2003)", "Historical trend", "Baseline year (2016)"),
                                      labels = c("Pre-intervention year (2003)", "Historical trend", "Baseline year (2016)"))


###================================
### National 
###================================

### Change in prevalence between pre-intervention and baseline year
## Description of historical trend, and additional variables
groupVARS <- c("District", "Region", "urbanrural", "MIS_UMRC")
prevCordat_wide <- as.data.table(prevCordat_wide, key = groupVARS)

### 2003 to 2016
prevCordat_wide[, PR.sim.diff.history := PR.sim[year == 2003] - PR.sim[year == 2016], by = groupVARS]
prevCordat_wide[, PR.sim.diff.history.perc := ((PR.sim[year == 2003] - PR.sim[year == 2016]) / PR.sim[year == 2003]) * 100, by = groupVARS]

prevCordat_wide[, PR.obs.diff.history := PR.obs[year == 2003] - PR.obs[year == 2016], by = groupVARS]
prevCordat_wide[, PR.obs.diff.history.perc := ((PR.obs[year == 2003] - PR.obs[year == 2016]) / PR.obs[year == 2003]) * 100, by = groupVARS]

summary(prevCordat_wide$PR.sim.diff.history)
summary(prevCordat_wide$PR.obs.diff.history)

summary(prevCordat_wide$PR.sim.diff.history.perc)
summary(prevCordat_wide$PR.obs.diff.history.perc)

### correlation, Concordance and bland altman plot
#prevCordat_wide <- na.omit(prevCordat_wide)
cor(prevCordat_wide[!is.na(prevCordat_wide$PR.sim) & !is.na(prevCordat_wide$PR.obs), "PR.sim"], prevCordat_wide[!is.na(prevCordat_wide$PR.sim) & !is.na(prevCordat_wide$PR.obs), "PR.obs"])
CCC(prevCordat_wide$PR.sim, prevCordat_wide$PR.obs, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)$rho.c

## baseline year only 
tempdat <- prevCordat_wide %>% filter(year == 2016 & !is.na(PR.sim) & !is.na(PR.obs))
cor(tempdat$PR.sim, tempdat$PR.obs)
CCC(tempdat$PR.sim, tempdat$PR.obs, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)$rho.c

pplot <- bland.altman.plot(tempdat$PR.sim, tempdat$PR.obs, graph.sys = "ggplot2", conf.int = .95, pch = 19)
ggsave(paste0("BAplot_2016.png"), plot = pplot, path = ExperimentFigureDir, width = 8, height = 6, device = "png")
rm(pplot)
rm(tempdat)

### all years - bland altman
tempdat <- prevCordat_wide %>% filter(!is.na(PR.sim) & !is.na(PR.obs))

pplot <- bland.altman.plot(tempdat$PR.sim, tempdat$PR.obs, graph.sys = "ggplot2", conf.int = .95, pch = 19)
ggsave(paste0("BAplot_2003-2016.png"), plot = pplot, path = ExperimentFigureDir, width = 8, height = 6, device = "png")
rm(pplot)

corALLYEARS <- cor(tempdat[, "PR.obs"], tempdat[, "PR.sim"], method = c("pearson"))
cccALLYEARS <- round(CCC(tempdat[, "PR.obs"], tempdat[, "PR.sim"], ci = "z-transform", conf.level = 0.95, na.rm = TRUE)$rho.c, 2)

corcaptiontext <- paste0("Pearson's r 2003-2016 = ", round(corALLYEARS, 3))
CCCcaptiontext <- paste0("CCC 2003-2016 = ", cccALLYEARS[1], " (95%CI: ", cccALLYEARS[2], ", ", cccALLYEARS[3], ")")


### Fitting plot - used for publication
tempdat <- as.data.frame(subset(prevCordat_wide, timeperiod2 != "Historical trend"))
lm2003 <- lm(PR.obs ~ PR.sim, data = subset(tempdat, year == 2003))
cor2003 <- cor(tempdat[tempdat$year == 2003, "PR.obs"], tempdat[tempdat$year == 2003, "PR.sim"], method = c("pearson"))
cor2016 <- cor(tempdat[tempdat$year == 2016, "PR.obs"], tempdat[tempdat$year == 2016, "PR.sim"], method = c("pearson"))

ccc2003 <- round(CCC(tempdat[tempdat$year == 2003, "PR.obs"], tempdat[tempdat$year == 2003, "PR.sim"], ci = "z-transform", conf.level = 0.95, na.rm = TRUE)$rho.c, 2)
ccc2016 <- round(CCC(tempdat[tempdat$year == 2016, "PR.obs"], tempdat[tempdat$year == 2016, "PR.sim"], ci = "z-transform", conf.level = 0.95, na.rm = TRUE)$rho.c, 2)

tempdat$rvalue = cor2003
tempdat$rvalue[tempdat$year == 2016] = cor2016

tempdat$cccvalue = paste0("CCC= ", ccc2003[1], " (95%CI: ", ccc2003[2], ", ", ccc2003[3], ")")
tempdat$cccvalue[tempdat$year == 2016] = paste0("CCC= ", ccc2016[1], " (95%CI: ", ccc2016[2], ", ", ccc2016[3], ")")

pplot <- ggplot(data = tempdat, aes(x = PR.obs, y = PR.sim, ymin = PRlo.sim, ymax = PRup.sim, label = paste0("Pearson's r = ", round(rvalue, 2)))) +
  theme_cowplot() +
  geom_errorbar(size = 1, alpha = 0.5, col = "lightblue") +
  geom_point(shape = 21, size = 1.5, fill = "blue") +
  geom_smooth(size = 1, method = "lm", se = FALSE, col = "indianred") +
  geom_abline(intercept = 0, slope = 1, size = 0.7) +
  geom_text(x = 11, y = 68, size = 5) +
  labs(title = "",
       subtitle = "",
       x = expression("Geospatial predicted " *
                        italic("Pf") *
                        "PR"["2 to 10"] *
                        " (%)"),
       y = expression("OpenMalaria predicted " *
                        italic("Pf") *
                        "PR"["2 to 10"] *
                        " (%)")) +
  facet_wrap(~timeperiod2, nrow = 2, scales = "free") +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10), labels = seq(0, 70, 10)) +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, 10), labels = seq(0, 70, 10)) +
  customTheme_noAngle +
  theme(legend.position = "right")
ggsave(paste0("fittingPlot_baseline_preIntervention.png"), plot = pplot, path = ExperimentFigureDir, width = 6, height = 10, device = "png")

pplot <- ggplot(data = tempdat, aes(x = PR.obs, y = PR.sim, ymin = PRlo.sim, ymax = PRup.sim, label = cccvalue)) +
  theme_cowplot() +
  geom_errorbar(size = 1, alpha = 0.5, col = "lightblue") +
  geom_point(shape = 21, size = 1.5, fill = "blue") +
  geom_smooth(size = 1, method = "lm", se = FALSE, col = "indianred") +
  geom_abline(intercept = 0, slope = 1, size = 0.7) +
  geom_text(x = 19, y = 68, size = 5) +
  labs(title = "",
       subtitle = "",
       x = expression("Geospatial predicted " *
                        italic("Pf") *
                        "PR"["2 to 10"] *
                        " (%)"),
       y = expression("OpenMalaria predicted " *
                        italic("Pf") *
                        "PR"["2 to 10"] *
                        " (%)"),
       caption = CCCcaptiontext) +
  facet_wrap(~timeperiod2, nrow = 2, scales = "free") +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10), labels = seq(0, 70, 10)) +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, 10), labels = seq(0, 70, 10)) +
  customTheme_noAngle +
  theme(legend.position = "right")
ggsave(paste0("fittingPlot_baseline_preIntervention_CCC.png"), plot = pplot, path = ExperimentFigureDir, width = 6, height = 10, device = "png")

##===============================
## Subnational - region and district
##===============================
#### Comparison per region
tapply(prevCordat_wide$PR.sim.diff.history, prevCordat_wide$Region, summary)
tapply(prevCordat_wide$PR.obs.diff.history, prevCordat_wide$Region, summary)

tapply(prevCordat_wide$PR.sim.diff.history.perc, prevCordat_wide$Region, summary)
tapply(prevCordat_wide$PR.obs.diff.history.perc, prevCordat_wide$Region, summary)

prevCordat_wide$PRobssim.ratio <- prevCordat_wide$PR.sim / prevCordat_wide$PR.obs
prevCordat_wide$PRobssim.diff <- prevCordat_wide$PR.obs - prevCordat_wide$PR.sim
summary(prevCordat_wide$PRobssim.diff)

###############################################
tempdat <- as.data.frame(subset(prevCordat_wide, timeperiod2 != "Historical trend"))
tempdat16 <- subset(tempdat, year == 2016)
#tempdat16 	<- subset(tempdat, year==2003)

###Write out minimum and maximum PR per region for positionining text lavels in plot (not used anymore, instead double facets used)
DataCov2016 <- do.call(rbind, lapply(split(tempdat16, tempdat16$Region),
                                     function(x) data.frame(group = x$Region[1], mCov = CCC(x$PR.obs, x$PR.sim, ci = "z-transform", conf.level = 0.95, na.rm = TRUE))))

DataCov2016 %>%
  dplyr::select(group, mCov.rho.c.est, mCov.rho.c.lwr.ci, mCov.rho.c.upr.ci) %>%
  unique()

DataCov2016 <- DataCov2016 %>%
  dplyr::mutate(Region = group) %>%
  dplyr::group_by(Region) %>%
  dplyr::select(-group) %>%
  dplyr::summarise_all(funs(mean)) %>%
  left_join(tempdat16, by = "Region") %>%
  mutate(cccvalue = paste0("CCC= ", round(mCov.rho.c.est, 2), "\n(95%CI: ", round(mCov.rho.c.lwr.ci, 2), ", ", round(mCov.rho.c.upr.ci, 2), ")")) %>%
  as.data.frame()

#### Create region labels used for facetting ,  including number of districts
ndis <- DataCov2016 %>%
  dplyr::select(Region, District) %>%
  unique() %>%
  group_by(Region) %>%
  tally() %>%
  as.data.frame()

regsorted <- DataCov2016 %>%
  left_join(ndis, by = "Region") %>%
  mutate(RegionLabel = paste0(Region, " (", n, ")")) %>%
  dplyr::arrange(mCov.rho.c.est) %>%
  dplyr::select(Region, RegionLabel) %>%
  unique()

DataCov2016$RegionLabel <- factor(DataCov2016$Region,
                                  levels = regsorted[, 1][c(length(regsorted[, 1]):1)],
                                  labels = regsorted[, 2][c(length(regsorted[, 2]):1)])

table(DataCov2016$RegionLabel, DataCov2016$Region, exclude = NULL)

pplot <- ggplot(data = DataCov2016, aes(x = PR.obs, y = PR.sim, ymin = PRlo.sim, ymax = PRup.sim)) +
  theme_cowplot() +
  geom_errorbar(size = 1, alpha = 0.8, col = "lightblue") +
  geom_point(shape = 21, size = 1.5, fill = "blue") +
  geom_smooth(size = 1, method = "lm", se = FALSE, col = "indianred") +
  geom_abline(intercept = 0, slope = 1, size = 0.7) +
  labs(title = "",
       subtitle = "",
       x = expression("Geospatial predicted " *
                        italic("Pf") *
                        "PR"["2 to 10"] *
                        " (%)"),
       y = expression("OpenMalaria predicted " *
                        italic("Pf") *
                        "PR"["2 to 10"] *
                        " (%)")) +
  facet_wrap(~timeperiod2, nrow = 2, scales = "free") +
  customTheme_noAngle +
  theme(legend.position = "right") +
  facet_wrap(RegionLabel ~ cccvalue, ncol = 5, scales = "free")
if (SAVE)ggsave(paste0("fittingPlot_perRegion_2016.v3.png"), plot = pplot, path = ExperimentFigureDir, width = 20, height = 20, device = "png")
#ggsave(paste0("fittingPlot_perRegion_2003.v2.png"), plot= pplot, path= ExperimentFigureDir ,width =20, height =20, device = "png")	


#### Historical trend 
prevCordat$estimatesourceLabel <- "OpenMalaria pedicted PfPR"
prevCordat$estimatesourceLabel[prevCordat$estimatesource != "sim"] <- "Geospatial predicted PfPR"
table(prevCordat$estimatesourceLabel, prevCordat$estimatesource, exclude = NULL)

prevCordat <- left_join(prevCordat, DataCov2016[, c("Region", "RegionLabel", "mCov.rho.c.est")], by = "Region")

pplot <- ggplot(data = subset(prevCordat, year <= 2016)) +
  theme_cowplot() +
  geom_smooth(aes(x = year, y = PR, col = estimatesourceLabel, fill = estimatesourceLabel), size = 1.3, span = 1) +
  labs(title = "",
       subtitle = "",
       x = "Year",
       y = expression("Geospatial" *
                        italic("PfPR") *
                        " - OpenMalaria" *
                        italic("PfPR")),
       col = "Predicton method",
       fill = "Predicton method") +
  theme(legend.position = "none") +
  #scale_x_continuous(breaks=c(2003:2016), labels=c(2003:2016))+
  facet_wrap(~RegionLabel, scales = "free") +
  scale_colour_manual(values = TwoCols) +
  scale_fill_manual(values = TwoCols) +
  customTheme_noAngle +
  theme(legend.position = "top")
if (SAVE)ggsave(paste0("fittingPlot_perRegion_historicalTrend.png"), plot = pplot, path = ExperimentFigureDir, width = 22, height = 12, device = "png")

####or instead of using smooth, calculate mean and confidence intervals, or use median and 95% interquartile ranfe
prevCordatReg <- aggrDat(prevCordat, c("RegionLabel", "Region", "year", "estimatesourceLabel"), "PR", WideToLong = FALSE)
pplot <- ggplot(data = subset(prevCordatReg, year <= 2016), aes(x = year, y = median.val, ymin = lower.ci.val, ymax = upper.ci.val, col = estimatesourceLabel, fill = estimatesourceLabel)) +
  theme_bw() +
  geom_ribbon() +
  geom_line() +
  labs(title = "",
       subtitle = "",
       x = "Year",
       y = expression("Geospatial" *
                        italic("PfPR") *
                        " - OpenMalaria" *
                        italic("PfPR"))) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(2003:2016), labels = c(2003:2016)) +
  facet_wrap(~RegionLabel, nrow = 4, scales = "free") +
  scale_colour_manual(values = TwoCols) +
  scale_fill_manual(values = TwoCols) +
  customTheme_Angle
if (SAVE) ggsave(paste0("fittingPlot_perRegion_v2_historicalTrend.png"), plot = pplot, path = ExperimentFigureDir, width = 18, height = 12, device = "png")


#### Per district
prevCordat_wide <- left_join(prevCordat_wide, DataCov2016[, c("Region", "RegionLabel")], by = "Region")

## Plot	per districts grouped by region
pplot <- ggplot(data = subset(prevCordat_wide, year <= 2016)) +
  theme_bw() +
  geom_smooth(aes(x = year, y = PRobssim.diff, group = District), size = 1, col = "deepskyblue2", alpha = 0.3, se = FALSE, span = 0.3) +
  geom_smooth(aes(x = year, y = PRobssim.diff), size = 1.7, col = "indianred", fill = "indianred", span = 1, se = TRUE) +
  geom_hline(yintercept = 1, col = "black", size = 1) +
  labs(title = "",
       subtitle = "",
       x = "Year",
       y = expression("Geospatial" *
                        italic("PfPR") *
                        " - OpenMalaria" *
                        italic("PfPR"))) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(2003:2016), labels = c(2003:2016)) +
  facet_wrap(~RegionLabel, nrow = 4) +
  customTheme_Angle
if (SAVE)ggsave(paste0("fittingPlot_diff_perDistrict.png"), plot = pplot, path = ExperimentFigureDir, width = 18, height = 12, device = "png")


#### Other plots (older) 
olderPlots = FALSE
if (olderPlots) {
  ## To Do - improve plot and add uncertainity intervals on both !
  psimobs <- ggplot(data = subset(prevCordat_wide, (year %in% historicalYears) | (year == baselineYear))) +
    geom_errorbar(aes(x = PR.obs, ymin = PRlo.sim, ymax = PRup.sim, col = MIS_UMRC), size = 1, alpha = 0.5) +
    geom_point(aes(x = PR.obs, y = PR.sim, fill = MIS_UMRC), shape = 21, size = 2) +
    geom_line(aes(x = PR.obs, y = PR.obs), size = 1.3) +
    labs(title = "Comparison of simulated and target prevalence",
         subtitle = "",
         x = expression(italic("PfPR")["2 to 10"] * "\n'Observed'"),
         y = expression(italic("PfPR")["2 to 10"] * " (OpenMalaria)"),
         fill = "Timing MIS vs UMRC",
         colour = "Timing MIS vs UMRC",
         caption = paste0("'Observed prevalence' estimated by ", fittedprevalenceSource)) +
    facet_wrap(~year, nrow = 2, scales = "free") +
    customTheme_noAngle +
    theme(legend.position = "right")

  psimobs_v2.1a <- ggplot(data = subset(prevCordat_wide, (year %in% c(baselineYear)))) +
    geom_errorbar(aes(x = PR.obs, ymin = PRlo.sim, ymax = PRup.sim, col = MIS_UMRC), size = 1, alpha = 0.5) +
    geom_point(aes(x = PR.obs, y = PR.sim, fill = MIS_UMRC), shape = 21, size = 2) +
    geom_line(aes(x = PR.obs, y = PR.obs), size = 1.3) +
    labs(title = "Baseline year",
         subtitle = "",
         x = expression(italic("PfPR")["2 to 10"] * " 'Observed'"),
         y = expression(italic("PfPR")["2 to 10"] * " (OpenMalaria)"),
         fill = "Timing MIS vs UMRC",
         colour = "Timing MIS vs UMRC") +
    facet_wrap(~year, scales = "free") +
    customTheme_noAngle +
    theme(legend.position = "right")

  psimobs_v2.1b <- ggplot(data = subset(prevCordat_wide, (year %in% c(MonitoringStart)))) +
    geom_errorbar(aes(x = PR.obs, ymin = PRlo.sim, ymax = PRup.sim, col = MIS_UMRC), size = 1, alpha = 0.5) +
    geom_point(aes(x = PR.obs, y = PR.sim, fill = MIS_UMRC), shape = 21, size = 2) +
    geom_line(aes(x = PR.obs, y = PR.obs), size = 1.3) +
    labs(title = "Pre-intervention year",
         subtitle = "",
         x = expression(italic("PfPR")["2 to 10"] * "'Observed'"),
         y = expression(italic("PfPR")["2 to 10"] * "(OpenMalaria)"),
         fill = "Timing MIS vs UMRC",
         colour = "Timing MIS vs UMRC") +
    facet_wrap(~year, scales = "free") +
    customTheme_noAngle +
    theme(legend.position = "right")

  psimobs_v2.2 <- ggplot(data = subset(prevCordat_wide, (year %in% historicalYears & !(year %in% c(baselineYear, MonitoringStart))))) +
    geom_errorbar(aes(x = PR.obs, ymin = PRlo.sim, ymax = PRup.sim, col = MIS_UMRC), size = 1, alpha = 0.5) +
    geom_point(aes(x = PR.obs, y = PR.sim, fill = MIS_UMRC), shape = 21, size = 2) +
    geom_line(aes(x = PR.obs, y = PR.obs), size = 1.3) +
    labs(title = "Historical trend",
         subtitle = "",
         x = expression(italic("PfPR")["2 to 10"] * " 'Observed'"),
         y = expression(italic("PfPR")["2 to 10"] * " (OpenMalaria)"),
         fill = "Timing MIS vs UMRC",
         colour = "Timing MIS vs UMRC",
         caption = paste0("'Observed prevalence' estimated by ", fittedprevalenceSource)) +
    facet_wrap(~year, nrow = 1, scales = "free") +
    customTheme_noAngle +
    theme(legend.position = "none")

  ## alternativ
  psimobs_v2.2b <- ggplot(data = subset(prevCordat_wide, (year %in% historicalYears & !(year %in% c(baselineYear, MonitoringStart))))) +
    geom_boxplot(aes(x = year, y = (PR.obs / PR.sim), fill = MIS_UMRC, group = interaction(year, MIS_UMRC)), size = 1) +
    labs(title = "Comparison of simulated and target prevalence",
         subtitle = "",
         fill = "Timing MIS vs UMRC",
         colour = "Timing MIS vs UMRC",
         caption = paste0("'Observed prevalence' estimated by ", fittedprevalenceSource)) +
    customTheme_noAngle +
    theme(legend.position = "bottom")

  ptop <- f_grid_arrange_shared_legend(psimobs_v2.1b, psimobs_v2.1a, nrow = 1, ncol = 2, position = "right")
  #psimobs2 <- grid.arrange(ptop, psimobs_v2.2,nrow=2,ncol=1, widths = 1:1,  heights=1:2)
  psimobs2 <- grid.arrange(ptop, psimobs_v2.2, nrow = 2, ncol = 1)
  #rm(psimobs_v2.1a,psimobs_v2.1b, psimobs_v2.2)

  filename <- paste0("lack_of_fit_sim.obs_", historicalYears[1], "-", baselineYear)
  if (SAVE)    ggsave(paste0(filename, ".png"), plot = psimobs, path = paste0(ExperimentFigureDir), width = 12, height = 7, device = "png")
  if (SAVE)    ggsave(paste0(filename, "_v2.png"), plot = psimobs2, path = paste0(ExperimentFigureDir), width = 12, height = 7, device = "png")
  if (SAVE)    ggsave(paste0(filename, "_v2_top.png"), plot = ptop, path = paste0(ExperimentFigureDir), width = 12, height = 7, device = "png")

}


##===============================================
### Prevalence maps - observed vs fitted 
## prevCordat, prevCordat_wide
###================================================
## reporting national population weighted mean on maps
## at district level 
tempdat <- prevCordat %>%
  dplyr::filter(year %in% c(MonitoringStart, baselineYear) & !is.na(PR)) %>%
  dplyr::mutate(
    PR = PR / 100,
    sourceLabel = ifelse(estimatesource == "obs", "'Observed PfPR estimates' ", "OpenMalaria predicted PfPR estimates"),
    yearLabel = ifelse(year == 2003, "Pre-intervention year (2003)", "Baseline year (2016)")) %>%
  as.data.frame()

tempdat$yearLabel <- factor(tempdat$yearLabel,
                            levels = c("Pre-intervention year (2003)", "Baseline year (2016)"),
                            labels = c("Pre-intervention year (2003)", "Baseline year (2016)"))

tempdat$PR.grp <- NA
tempdat$PR.grp[which(tempdat$PR <= 0.01)] <- 1
tempdat$PR.grp[which(tempdat$PR > 0.01 & tempdat$PR <= 0.05)] <- 2
tempdat$PR.grp[which(tempdat$PR > 0.05 & tempdat$PR <= 0.10)] <- 3
tempdat$PR.grp[which(tempdat$PR > 0.10 & tempdat$PR <= 0.25)] <- 4
tempdat$PR.grp[which(tempdat$PR > 0.25 & tempdat$PR <= 0.50)] <- 5
tempdat$PR.grp[which(tempdat$PR > 0.50)] <- 6
table(tempdat$PR.grp, exclude = NULL)

tempdat$PR.grp <- factor(as.character(tempdat$PR.grp),
                         levels = c(1:6),
                         labels = c("<=1", ">1-5", ">5-10", ">10-25", ">25-50", ">50"))

table(tempdat$PR.grp, exclude = NULL)
tempDat.df = dplyr::left_join(districts_sp.f, tempdat, by = "District")

pmap <- ggplot(, warnings = FALSE) +
  geom_polygon(data = subset(tempDat.df, !is.na(yearLabel) & !is.na(sourceLabel)),
               aes(x = long, y = lat, group = group, fill = PR.grp), color = "white", size = 0.35) +
  geom_polygon(data = regions_sp.f, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.75) +
  labs(title = "",
       fill = expression(italic("PfPR")["2 to 10"] * "(%)")) +
  facet_grid(yearLabel ~ sourceLabel) +
  map.theme +
  prevLegend_cat

if (SAVE)ggsave(paste0("MAP_FittedPrevalence_simobs_2003_2016.png"), plot = pmap, path = paste0(ExperimentFigureDir), width = 10, height = 10, device = "png")

if (SAVE)save(prevCordat, prevCordat_wide, file = file.path(ExperimentDataDir, "prevCordat.RData"))

ageConversionPlot = FALSE
if (ageConversionPlot) {
  #if(length(grep( "Refitted", ExperimentDir))<=0){

  #### Age conversion ( if 0 to 5 not processed)

  ## load lookup table 
  conversionTableAge <- as.data.frame(read_excel(paste0(PfPRDataDir, "conversionTableAge.xlsx")))
  model <- lm(PfPR_0to5 ~ poly(PfPR_2to10, 3), data = conversionTableAge)

  tempprevdat <- JAGSresults %>%
    left_join(TZADistrictDat[, c("Region", "District", "Population_2016")], by = "District") %>%
    dplyr::filter(outcome == "PR" & (year %in% c(2012, 2016) | year == 2017 & FutScen_nr == 3)) %>%
    dplyr::group_by(Region, District, Population_2016, year) %>%
    dplyr::summarize(PR_2to10 = mean(median)) %>%
    as.data.frame()

  ## change to 0 to 5 
  tempprevdat$PR_0to5 <- predict(model, data.frame(PfPR_2to10 = tempprevdat$PR_2to10))
  ggplot(data = tempprevdat) +
    geom_point(aes(x = PR_0to5 * 100, y = PR_2to10 * 100)) +
    geom_line(aes(x = PR_0to5 * 100, y = PR_0to5 * 100)) +
    theme_bw()

  tapply(tempprevdat$PR_0to5, tempprevdat$year, summary)

  ## Aggregate per region --- weighted by population!
  predPfPRRegiondat <- tempprevdat %>%
    dplyr::group_by(Region, year) %>%
    dplyr::summarize(PRmean_0to5_wt = weighted.mean(PR_0to5, Population_2016),
                     PRmean_0to5 = mean(PR_0to5)) %>%
    as.data.frame()

  predPfPRRegiondat$PR.grp <- NA
  predPfPRRegiondat$PR.grp[which(predPfPRRegiondat$PRmean_0to5_wt <= 0.01)] <- 1
  predPfPRRegiondat$PR.grp[which(predPfPRRegiondat$PRmean_0to5_wt > 0.01 & predPfPRRegiondat$PRmean_0to5_wt <= 0.05)] <- 2
  predPfPRRegiondat$PR.grp[which(predPfPRRegiondat$PRmean_0to5_wt > 0.05 & predPfPRRegiondat$PRmean_0to5_wt <= 0.10)] <- 3
  predPfPRRegiondat$PR.grp[which(predPfPRRegiondat$PRmean_0to5_wt > 0.10 & predPfPRRegiondat$PRmean_0to5_wt <= 0.25)] <- 4
  predPfPRRegiondat$PR.grp[which(predPfPRRegiondat$PRmean_0to5_wt > 0.25 & predPfPRRegiondat$PRmean_0to5_wt <= 0.50)] <- 5
  predPfPRRegiondat$PR.grp[which(predPfPRRegiondat$PRmean_0to5_wt > 0.50)] <- 6
  table(predPfPRRegiondat$PR.grp, exclude = NULL)
  predPfPRRegiondat$PR.grp <- factor(as.character(predPfPRRegiondat$PR.grp),
                                     levels = c(1:6),
                                     labels = c("<=1", ">1-5", ">5-10", ">10-25", ">25-50", ">50"))

  table(predPfPRRegiondat$PR.grp, exclude = NULL)
  tempDat.df = dplyr::left_join(regions_sp.f, predPfPRRegiondat, by = "Region")

  pmap_sim <- ggplot(, warnings = FALSE) +
    geom_polygon(data = tempDat.df,
                 aes(x = long, y = lat, group = group, fill = PR.grp), color = "white", size = 0.35) +
    geom_polygon(data = regions_sp.f, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.75) +
    labs(title = "",
         fill = expression(italic("PfPR")["0 to 5"] * "(%)")) +
    facet_wrap(~year, ncol = 1) +
    map.theme +
    prevLegend_cat


  ##########################################	
  source("C:/Users/rungma/Projects/OM_TZA/Scripts/DataDescription_and_Preparation/PfPR/MIS_pfpr_maps.R")

  predPfPRRegiondat$diagnostic = "microscopy"
  predPfPRRegiondat$value = predPfPRRegiondat$PRmean_0to5_wt
  predPfPRRegiondat$PR = predPfPRRegiondat$PRmean_0to5_wt
  predPfPRRegiondat$source = "OpenMalaria predictions"
  SCdat_long$source = "Malaria Indicator Surveys"


  tempdat <- predPfPRRegiondat %>%
    dplyr::select(colnames(SCdat_long)) %>%
    rbind(SCdat_long) %>%
    as.data.frame()

  tempDat.df = dplyr::left_join(regions_sp.f, tempdat, by = "Region")

  pmap_sim <- ggplot(, warnings = FALSE) +
    geom_polygon(data = subset(tempDat.df, diagnostic == "microscopy"),
                 aes(x = long, y = lat, group = group, fill = PR.grp), color = "white", size = 0.35) +
    geom_polygon(data = regions_sp.f, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.75) +
    labs(title = "",
         fill = expression(italic("PfPR")["2 to 10"] * "(%)")) +
    facet_grid(source ~ year, , margins = "am") +
    prevLegend_cat +
    map.theme

  if (SAVE)  ggsave(paste0("Map_MIS_simobs_region.png"), plot = pmap_sim, path = ExperimentFigureDir, width = 12, height = 9, device = "png")

  rm(SCdat_long, predPfPRRegiondat, tempDat.df)

}

