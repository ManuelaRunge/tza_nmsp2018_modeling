### ====================================
### Custom objects frequently used
### ====================================
### Plot settings

DateLabels1 <- c("'03", "'04", "'05", "'06", "'07", "'08", "'09", "'10", "'11", "'12", "'16")
DateLabels2 <- c("2003", "'04", "'05", "'06", "'07", "'08", "'09", "'10", "'11", "'12", "'13", "'14", "'15", "2016")


## positions
pos <- position_dodge(width = 0.9)

### colours
## sequential
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

temp_FiveCols <- c(
  "Counterfactual" = "green4",
  "Optimised for cost-effectiveness" = "violetred1",
  "NMSP with maintained CM" = "royalblue",
  "NMSP with improved CM" = "mediumturquoise",
  "Achieving NMSP target at lowest costs" = "darkgoldenrod2",
  "SMMSP with MDA" = "red",
  "SMMSP" = "red"
)

## ==============================
##### COLOUR PALETTS AND LEGENDS
## ==============================

### PREVALENCE - RdYlGn
PrevPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))
prevLegend <- scale_fill_gradientn(colours = PrevPalette(100), limits = c(0, 1), na.value = "lightgray")

# prevcols 		<- c("#1A9850" ,"#91CF60", "#fee8c8", "#feb24c", "#e31a1c")
prevcolsAdj <- c("darkorchid2", "#1A9850", "#91CF60", "gold2", "#e31a1c")
prevcols <- rev(brewer.pal(6, "RdYlGn")) #
prevcols <- gsub("#D9EF8B", "#fee8c8", prevcols)
prevLegend_cat <- scale_fill_manual(values = prevcols, drop = FALSE)

## CASES I - RdYlGn
### cases (absolute (limits and breaks not yet specified) --- white to darkblue? white to darkred? define later
CasesPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))
CasesLegend <- scale_fill_gradientn(colours = CasesPalette(100), na.value = "lightgray")
### cases percentage (limits 0,1) --- white to darkblue? white to darkred? define later
CasesPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))
CasesLegend <- scale_fill_gradientn(colours = CasesPalette(100), limits = c(0, 1), na.value = "lightgray")

## CASES II - YlOrBr
Cases_pPcols <- (brewer.pal(6, "YlOrBr")) #
Cases_pPcols_adj <- Cases_pPcols[2:6]
Cases_pPcols_adj[1] <- "grey"
Cases_pP_Legend_cat <- scale_fill_manual(values = Cases_pPcols_adj, drop = FALSE)

## COSTS
### costs (absolute (limits and breaks not yet specified) --- white to darkblue? white to darkred? define later
CostPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))
CostLegend <- scale_fill_gradientn(colours = CostPalette(100), na.value = "lightgray")

CostPercPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))
CostPercLegend <- scale_fill_gradientn(colours = CostPercPalette(100), limits = c(0, 1), na.value = "lightgray")


## Red to blue
RedBluePalette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
RedBlueLegend <- scale_fill_gradientn(colours = RedBluePalette(100), limits = c(0, 1), na.value = "lightgray")

RedBluecols <- rev(brewer.pal(6, "RdYlBu")) #
RedBlueLegend_cat <- scale_fill_manual(values = RedBluecols, drop = FALSE)


## ==============================
##### TEXT AND LABELS
## ==============================

customTheme_noAngle <- theme(
  strip.text.x = element_text(size = 24, face = "bold"),
  strip.text.y = element_text(size = 24, face = "bold"),
  plot.title = element_text(size = 24),
  plot.subtitle = element_text(size = 20),
  plot.caption = element_text(size = 18),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20, angle = 0, hjust = 1),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20)
)

customTheme_Angle <- theme(
  strip.text.x = element_text(size = 24, face = "bold"),
  strip.text.y = element_text(size = 24, face = "bold"),
  plot.title = element_text(size = 24),
  plot.subtitle = element_text(size = 20),
  plot.caption = element_text(size = 18),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20, angle = 90, hjust = 0, vjust = 0),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20)
)

customTheme_noAngle2 <- theme(
  strip.text.x = element_text(size = 16, vjust = -1, hjust = 0),
  strip.background = element_blank(),
  plot.title = element_text(size = 20),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 16),
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 16)
)

customTheme <- theme(
  strip.text.x = element_text(size = 16, face = "bold"),
  plot.title = element_text(size = 20, vjust = -1, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 16),
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 16)
)

customThemeNoFacet <- theme(
  strip.text.x = element_text(size = 16, face = "bold"),
  strip.background = element_blank(),
  plot.title = element_text(size = 20, vjust = -1, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 16),
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 16)
)

customThemeNoFacet_angle <- theme(
  strip.text.x = element_text(size = 16, face = "bold"),
  strip.background = element_blank(),
  plot.title = element_text(size = 20, vjust = -1, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 16),
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 16)
)

## settings for map

# map.theme =  theme(strip.text.x = element_text(size = 14,vjust=-1,hjust=0.5, face="bold"),
# 				 strip.background 	= element_blank(),
# 				 plot.title 		= element_text(size =14,vjust=-1,hjust=0.5),
# 				 plot.caption 		= element_text(size =14),
# 				 legend.title		= element_text(size =12),
# 				 legend.text 		= element_text(size =12),
#                #legend.key.size	= unit(2.5,"line"),
# 				 legend.position	="right")

## edited from https://rpubs.com/danielkirsch/styling-choropleth-maps  (works better than theme nothing)
map.theme <- theme(
  strip.text.x = element_text(size = 14, vjust = -1, hjust = 0.5, face = "bold"),
  strip.text.y = element_text(size = 14, vjust = -1, hjust = 0.5, face = "bold"),
  strip.background = element_blank(),
  axis.line = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.title = element_text(size = 14, vjust = -1, hjust = 0.5),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 12),
  # legend.key.size	= unit(2.5,"line"),
  legend.position = "right"
)
