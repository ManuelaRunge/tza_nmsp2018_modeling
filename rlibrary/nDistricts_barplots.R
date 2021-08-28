# mainscript: C:\Users\rungma\Projects\OM_TZA\Analysis\WS4_StrategicPlanning\rscripts\analysis_script_additionalAnalysisForReport.Rmd
## data needed:
# JAGSresults.long
# JAGSresults.long2

if ("JAGSresults.long" %in% ls() == FALSE) {
  source("C:/Users/rungma/Projects/OM_TZA/Analysis/WS4_StrategicPlanning/rscripts/Directories.R")
  load(paste0(ExperimentDataDir, "JAGSresultsForPlots.RData"))
  SAVEPLOT = FALSE
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(reshape)
  library(grid)
  library(gridExtra)
  library(ggplot2)
  library(cowplot)
  prevcols <- c("#1A9850", "#91CF60", "#fee8c8", "#feb24c", "#e31a1c")
}

tbase <- JAGSresults.long %>%
  dplyr::filter(statistic == "median", FutScen_nrALL == 3 & (year == 2020 | year == 2016)) %>%
  #dplyr::select(District, year , PR ) %>% unique() %>%
  dplyr::arrange(District)

colnames(tbase)[colnames(tbase) == "PR"] <- "PR.median"
tbase$PR.median <- tbase$PR.median * 100

tbase$PR.grp <- NA
tbase$PR.grp[which(tbase$PR.median <= 1)] <- 1
tbase$PR.grp[which(tbase$PR.median > 1 & tbase$PR.median <= 5)] <- 2
tbase$PR.grp[which(tbase$PR.median > 5 & tbase$PR.median <= 10)] <- 3
tbase$PR.grp[which(tbase$PR.median > 10 & tbase$PR.median <= 30)] <- 4
tbase$PR.grp[which(tbase$PR.median > 30)] <- 5

tbase$PR.grp <- factor(tbase$PR.grp,
                       levels = c(1:5),
                       labels = c("<=1%", ">1-5%", ">5-10%", ">10-30%", ">30%"))

table(tbase$PR.grp, exclude = NULL)

tbase <- table(tbase$year, tbase$PR.grp)
rowSums(tbase)
tbase <- as.data.frame(t(tbase))

colnames(tbase)[colnames(tbase) == "Var1"] <- "pr"
colnames(tbase)[colnames(tbase) == "Var2"] <- "year"

tbase$year <- as.numeric(as.character(tbase$year))
#tbase$year <- paste0("base ", tbase$year)
tbase <- tbase %>%
  group_by(year) %>%
  mutate(Prop = (Freq / 181) * 100) %>%
  as.data.frame()
tbase$label = "3.base"
tbase[tbase$year == 2016, c("label")] = "1.base"

for (STRATEGY in unique(JAGSresults.long2$Strategy))
{
  # STRATEGY <- unique(JAGSresults.long2$Strategy)[1]
  print(paste0("Creating plot for ", STRATEGY))
  tempdat <- JAGSresults.long2 %>%
    dplyr::filter(statistic == "median" &
                    year >= 2017 &
                    Strategy == STRATEGY &
                    FutScen_nrALL == Strategy_FutScen_nrALL) %>%
    dplyr::select(Strategy, District, Strata, year, PR) %>%
    unique() %>%
    dplyr::arrange(District) %>%
    as.data.frame()

  tempdat$PR.median <- tempdat$PR * 100
  tempdat$PR.grp <- NA
  tempdat$PR.grp[which(tempdat$PR.median <= 1)] <- 1
  tempdat$PR.grp[which(tempdat$PR.median > 1 & tempdat$PR.median <= 5)] <- 2
  tempdat$PR.grp[which(tempdat$PR.median > 5 & tempdat$PR.median <= 10)] <- 3
  tempdat$PR.grp[which(tempdat$PR.median > 10 & tempdat$PR.median <= 30)] <- 4
  tempdat$PR.grp[which(tempdat$PR.median > 30)] <- 5
  table(tempdat$PR.grp)
  tempdat$PR.grp <- factor(tempdat$PR.grp, levels = c(1:5), labels = c("<=1%", ">1-5%", ">5-10%", ">10-30%", ">30%"))
  table(tempdat$PR.grp, exclude = NULL)

  t1_tempdat <- table(tempdat$year, tempdat$PR.grp)
  t1 <- t1_tempdat

  rowSums(t1)
  t1 <- as.data.frame(t(t1))

  colnames(t1)[colnames(t1) == "Var1"] <- "pr"
  colnames(t1)[colnames(t1) == "Var2"] <- "year"
  t1$year <- paste0(t1$year)
  t1$label <- paste0("2.Stratified intervention")

  t1 <- t1 %>%
    dplyr::filter(year <= 2020) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(Prop = (Freq / 181) * 100) %>%
    as.data.frame()

  tplot <- as.data.frame(rbind(tbase, t1))
  tplot$label <- factor(tplot$label,
                        levels = c("1.base", "2.Stratified intervention", "3.base"),
                        labels = c("Current\nsituation", "Future interventions", "Counter\nfactual"))

  ## Crate plot
  gg <- ggplot(subset(tplot), aes(x = factor(year), y = Freq, fill = pr, label = Freq)) +
    theme_cowplot() +
    geom_bar(, stat = "identity") +
    facet_grid(~label, scale = "free_x", space = "free_x") +
    labs(x = "", y = "Number of districts", fill = expression(italic("PfPR")["2 to 10"] * " (%)")) +
    scale_fill_manual(values = prevcols) +
    scale_y_continuous(expand = c(0, 1), lim = c(0, 181)) +
    geom_text(size = 5, position = position_stack(vjust = 0.5))

  if (SAVEPLOT)ggsave(paste0(FigureDir, "barchart_nDistricts/", STRATEGY, "_n_districts_NSP.png"), gg, width = 19.7, height = 13.35, scale = 0.4)
  rm(tplot, t1, gg)
}