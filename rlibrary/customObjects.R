require(ggplot2)
require(cowplot)
require(scales)
require(RColorBrewer)

simPop <- 10000
DistrictVersion <- "2018"
MonitoringStart <- 2003
MonitoringEnd <- 2022
baselineYear <- 2016
EvaluationStart <- 2017
EvaluationEnd <- 2020
EvaluationYears <- c((baselineYear + 1):EvaluationEnd)
historicalYears <- c(MonitoringStart:2012)


theme_set(theme_cowplot())
options(scipen = 10000)
point <- format_format(big.mark = "'", decimal.mark = ".", scientific = FALSE)
strata_lbl <- c("very low", "moderate", "low", "high", "urban")

strategies_lbl <- c(
  "Counterfactual",
  "NMSP with maintained CM",
  "NMSP with improved CM",
  "Optimised for cost-effectiveness",
  "Achieving NMSP target at lowest costs",
  "potential SMMSP\n with MDA",
  "potential SMMSP",
  "SMMSP"
)
## sequential
fig5cols <- c('#00B2EE', '#8DC63F', '#EE7600', '#C53F42', '#628A2C', '#C38B4B', '#614525', '#9D3234')
TwoCols<- c("deepskyblue2", "darkorange2")

TwoCols_seq <- c("#fe9929", "#41b6c4")
ThreeCols_seq <- c("#a1dab4", "#41b6c4", "#225ea8")
FourCols_seq <- c("#fe9929", "#a1dab4", "#41b6c4", "#225ea8")
# FourCols_seq 	= c("#ffffcc", "#a1dab4","#41b6c4","#225ea8")
FiveCols_seq <- c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8", "green4")
SixCols_seq <- c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8", "green3", "green4")

### contrast
TwoCols_con <- c("cornflowerblue", "firebrick2")
ThreeCols_con <- c("#fc8d59", "#ffffbf", "#91bfdb") # c("cornflowerblue","firebrick2","green4")
FourCols_con <- c("cornflowerblue", "firebrick2", "green4", "orange")
FiveCols_con <- c("cornflowerblue", "firebrick2", "green4", "orange", "steelblue1")
SixCols_con <- c("cornflowerblue", "darkorange2", "green4", "steelblue1", "#41b6c4", "#984ea3")

## Favourits
TwoCols <- c("deepskyblue2", "darkorange2")
ThreeCols <- c("cornflowerblue", "firebrick2", "green4")
FourCols <- c("#fe9929", "#a1dab4", "#41b6c4", "#225ea8")
FourCols2 <- c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8")


# prevcols 		<- c("#1A9850" ,"#91CF60", "#fee8c8", "#feb24c", "#e31a1c")
prevcolsAdj <- c("darkorchid2", "#1A9850", "#91CF60", "gold2", "#e31a1c")
prevcols <- rev(brewer.pal(6, "RdYlGn")) #
prevcols <- gsub("#D9EF8B", "#fee8c8", prevcols)
prevLegend_cat <- scale_fill_manual(values = prevcols, drop = FALSE)

StrataCols <- c("very low" = "#1A9850",
                "low" = "#91CF60",
                "urban" = "darkorchid2",
                "moderate" = "gold2",
                "high" = "#e31a1c")


## ==========================
### SETTINGS
## ==========================
customTheme_noAngle <- theme(
  # strip.text.x = element_text(size = 16, face="plain"),
  #  strip.text.y = element_text(size = 16, face="plain"),
  panel.spacing.x = unit(1, "line"),
  strip.placement = "outside",
  strip.background = element_rect(colour = "white", fill = "white"),
  strip.text.x = element_text(size = 14, face = "bold"),
  strip.text.y = element_text(size = 14, face = "bold"),
  plot.title = element_text(size = 20, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14, angle = 0),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14)
)

customTheme_noAngle2 <- theme(
  # strip.text.x = element_text(size = 16, face="plain"),
  #  strip.text.y = element_text(size = 16, face="plain"),
  panel.spacing.x = unit(1, "line"),
  strip.placement = "outside",
  strip.text.x = element_text(size = 14, face = "bold"),
  strip.text.y = element_text(size = 14, face = "bold"),
  plot.title = element_text(size = 20, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14, angle = 0),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14)
)

customTheme_Angle <- theme(
  # strip.text.x = element_text(size = 16, face="plain"),
  #  strip.text.y = element_text(size = 16, face="plain"),
  panel.spacing.x = unit(1, "line"),
  strip.placement = "outside",
  strip.background = element_rect(colour = "black", fill = "white"),
  strip.text.x = element_text(size = 14, face = "bold"),
  strip.text.y = element_text(size = 14, face = "bold"),
  plot.title = element_text(size = 20, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 12, angle = 90),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14)
)
