cat(paste0('Start running 02_assess_fit_perRegion.R'))

blandPlots = F
fittedprevalenceSource <- "(KEMRI)"

load(file.path('dat', "TZA_pfpr_Nov2018.RData"))  ## Requires access
ds.obs_long <- subset(ds.obs_long, source == "KEMRI")
str(ds.obs_long)
table(ds.obs_long$District, ds.obs_long$year)

prevCordat1 <- ds.obs_long %>%
  dplyr::filter(AgeGroup == "2to10" & year >= 2003 & year <= 2017) %>%
  dplyr::mutate(estimatesource = "obs", PR = value, PRlo = NA, PRup = NA) %>%
  dplyr::select(District, year, PR, PRlo, PRup, estimatesource) %>%
  as.data.frame

### take mean of future scenarios for baselne, instead of just one future scenario
#use median and credible intervals
load(file.path(simout_dir, "JAGSresults_wide.RData"))
JAGSresults_wide <- JAGSresults_wide %>%
  filter(year <= 2017) %>%
  group_by(District, year, statistic) %>%
  summarise(PR = mean(PR))
table(JAGSresults_wide$District, JAGSresults_wide$year)

prevCordat2 <- JAGSresults_wide %>%
  pivot_wider(names_from = statistic, values_from = PR) %>%
  dplyr::group_by(District, year) %>%
  dplyr::summarize(PR = mean(median), PRlo = mean(q2.5), PRup = mean(q97.5)) %>%
  dplyr::mutate(estimatesource = "sim") %>%
  dplyr::select(District, year, PR, PRlo, PRup, estimatesource) %>%
  as.data.frame()

prevCordat <- rbind(prevCordat1, prevCordat2)
prevCordat$District <- as.character(prevCordat$District)
rm(prevCordat1, prevCordat2)

## long to wide format
prevCordat_wide <- reshape(prevCordat, idvar = c("District", "year"), timevar = "estimatesource", direction = "wide")

### Add label variables
prevCordat_wide$timeperiod <- "Historical trend"
prevCordat_wide$timeperiod[prevCordat_wide$year == baselineYear] <- "Baseline"
prevCordat_wide$timeperiod[prevCordat_wide$year == MonitoringStart] <- "Pre-intervention year"

prevCordat_wide$timeperiod2 <- "Historical trend"
prevCordat_wide$timeperiod2[prevCordat_wide$year == baselineYear] <- "Baseline year (2016)"
prevCordat_wide$timeperiod2[prevCordat_wide$year == MonitoringStart] <- "Pre-intervention year (2003)"

time_labels <- c("Pre-intervention year (2003)", "Historical trend", "Baseline year (2016)")
prevCordat_wide$timeperiod2 <- factor(prevCordat_wide$timeperiod2, levels = time_labels, labels = time_labels)

dim(prevCordat); any(duplicated(prevCordat))
dim(prevCordat_wide); any(duplicated(prevCordat_wide))

###================================
### National
###================================
### Change in prevalence between pre-intervention and baseline year
## Description of historical trend, and additional variables
groupVARS <- c("District")
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

if (blandPlots) {
  library('BlandAltmanLeh')
  pplot <- bland.altman.plot(tempdat$PR.sim, tempdat$PR.obs, graph.sys = "ggplot2", conf.int = .95, pch = 19)
  ggsave(paste0("BAplot_2016.png"), plot = pplot, path = file.path('figures'), width = 8, height = 6, device = "png")
  rm(pplot)
  rm(tempdat)

  ### all years - bland altman
  tempdat <- prevCordat_wide %>% filter(!is.na(PR.sim) & !is.na(PR.obs))
  pplot <- bland.altman.plot(tempdat$PR.sim, tempdat$PR.obs, graph.sys = "ggplot2", conf.int = .95, pch = 19)
  ggsave(paste0("BAplot_2003-2016.png"), plot = pplot, path = file.path('figures'), width = 8, height = 6, device = "png")
  rm(pplot)
}

tempdat <- prevCordat_wide %>% filter(!is.na(PR.sim) & !is.na(PR.obs))
corALLYEARS <- cor(tempdat[, "PR.obs"], tempdat[, "PR.sim"], method = c("pearson"))
cccALLYEARS <- round(CCC(tempdat$PR.sim, tempdat$PR.obs, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)$rho.c, 2)

corcaptiontext <- paste0("Pearson's r 2003-2016 = ", round(corALLYEARS, 3))
CCCcaptiontext <- paste0("CCC 2003-2016 = ", cccALLYEARS[1], " (95%CI: ", cccALLYEARS[2], ", ", cccALLYEARS[3], ")")

### Fitting plot
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

pplot <- ggplot(data = tempdat, aes(x = PR.obs, y = PR.sim, ymin = PRlo.sim, ymax = PRup.sim,
                                    label = paste0("Pearson's r = ", round(rvalue, 2)))) +
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
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, 10), labels = seq(0, 75, 10)) +
  scale_x_continuous(limits = c(0, 75), breaks = seq(0, 75, 10), labels = seq(0, 75, 10)) +
  customTheme_noAngle

pplot
ggsave(paste0("fittingPlot_baseline_preIntervention.png"), plot = pplot,
       path = file.path('figures'), width = 6, height = 10, device = "png")

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
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, 10), labels = seq(0, 75, 10)) +
  scale_x_continuous(limits = c(0, 75), breaks = seq(0, 75, 10), labels = seq(0, 75, 10)) +
  customTheme_noAngle

pplot
ggsave(paste0("fittingPlot_baseline_preIntervention_CCC.png"), plot = pplot,
       path = file.path('figures'), width = 6, height = 10, device = "png")

##===============================
## Subnational - region and district
##===============================
#### Comparison per region
TZADistrictDat <- fread(file.path('dat', 'TZADistrictDat.csv')) %>%
  dplyr::select(-Strata) %>%
  rename(Strata = Stratification.5b)
prevCordat_wide <- prevCordat_wide %>% left_join(unique(TZADistrictDat[, c('District', 'Region', 'Population_2016', 'Strata')]))
prevCordat <- prevCordat %>% left_join(unique(TZADistrictDat[, c('District', 'Region', 'Population_2016', 'Strata')]))
rm(TZADistrictDat)

dim(prevCordat); any(duplicated(prevCordat))
dim(prevCordat_wide); any(duplicated(prevCordat_wide))

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
                                     function(x) data.frame(group = x$Region[1],
                                                            mCov = CCC(x$PR.obs, x$PR.sim,
                                                                       ci = "z-transform",
                                                                       conf.level = 0.95,
                                                                       na.rm = TRUE))))

DataCov2016 %>%
  dplyr::select(group, mCov.rho.c.est, mCov.rho.c.lwr.ci, mCov.rho.c.upr.ci) %>%
  unique()

DataCov2016 <- DataCov2016 %>%
  dplyr::mutate(Region = group) %>%
  dplyr::group_by(Region) %>%
  dplyr::select(-group) %>%
  dplyr::summarise_all(funs(mean)) %>%
  left_join(tempdat16, by = "Region") %>%
  mutate(cccvalue = paste0("CCC= ", round(mCov.rho.c.est, 2), "\n(95%CI: ",
                           round(mCov.rho.c.lwr.ci, 2), ", ",
                           round(mCov.rho.c.upr.ci, 2), ")")) %>%
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

DataCov2016 %>%
  group_by(Region, RegionLabel, year) %>%
  tally()


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

pplot
ggsave(paste0("fittingPlot_perRegion_2016.png"), plot = pplot,
       path = file.path('figures'), width = 20, height = 20, device = "png")


#### Historical trend
prevCordat$estimatesourceLabel <- "OpenMalaria pedicted PfPR"
prevCordat$estimatesourceLabel[prevCordat$estimatesource != "sim"] <- "Geospatial predicted PfPR"
table(prevCordat$estimatesourceLabel, prevCordat$estimatesource, exclude = NULL)

prevCordat <- left_join(prevCordat, DataCov2016[, c("Region", "District", "year", "mCov.rho.c.est")])
dim(prevCordat); any(duplicated(prevCordat))

pplot <- ggplot(data = subset(prevCordat, year <= 2016)) +
  theme_cowplot() +
  geom_smooth(aes(x = year, y = PR, col = estimatesourceLabel, fill = estimatesourceLabel), size = 1.3, span = 1) +
  labs(title = "",
       subtitle = "",
       x = "Year",
       y = expression(italic("PfPR")),
       col = "Predicton method",
       fill = "Predicton method") +
  theme(legend.position = "none") +
  #scale_x_continuous(breaks=c(2003:2016), labels=c(2003:2016))+
  facet_wrap(~Region, scales = "free") +
  scale_colour_manual(values = TwoCols) +
  scale_fill_manual(values = TwoCols) +
  customTheme_noAngle +
  theme(legend.position = "top")

pplot
ggsave(paste0("fittingPlot_perRegion_historicalTrend.png"), plot = pplot,
       path = file.path('figures'), width = 22, height = 12, device = "png")

### instead of using smooth, calculate mean and confidence intervals, or use median and 95% interquartile range
prevCordatReg <- f_weighted.aggrDat(prevCordat,
                                    groupVars = c("Region", "year", "estimatesourceLabel"),
                                    valueVar = "PR",
                                    weightVar = "Population_2016",
                                    WideToLong = FALSE)

pplot <- ggplot(data = subset(prevCordatReg, year <= 2016),
                aes(x = year, y = median.val, ymin = lower.ci.val, ymax = upper.ci.val,
                    fill = estimatesourceLabel)) +
  theme_bw() +
  geom_ribbon(alpha = 0.3) +
  geom_line(aes(col = estimatesourceLabel)) +
  labs(title = "",
       subtitle = "",
       x = "Year",
       y = expression(italic("PfPR"))) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(2003:2016), labels = c(2003:2016)) +
  facet_wrap(~Region, nrow = 4, scales = "free") +
  scale_colour_manual(values = TwoCols) +
  scale_fill_manual(values = TwoCols) +
  customTheme_Angle +
  theme(legend.position = 'top')

pplot
ggsave(paste0("fittingPlot_perRegion_v2_historicalTrend.png"), plot = pplot,
       path = file.path("figures"), width = 18, height = 12, device = "png")


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

pplot
ggsave(paste0("fittingPlot_diff_perDistrict.png"), plot = pplot,
       path = file.path('figures'), width = 18, height = 12, device = "png")
