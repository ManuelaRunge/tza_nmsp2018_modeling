library(tidyverse)
library(raster)

theme_set(theme_cowplot())
prevcolsAdj <- c("darkorchid2", "#1A9850", "#91CF60", "gold2", "#e31a1c")
StrataCols <- c("very low" = prevcolsAdj[2], "low" = prevcolsAdj[3], "urban" = prevcolsAdj[1], "moderate" = prevcolsAdj[4], "high" = prevcolsAdj[5])

districts_sp <- shapefile(file.path("Shapefiles", "TZA_Districts.shp"), verbose = FALSE)
regions_sp <- shapefile(file.path("Shapefiles", "TZA_Regions.shp"), verbose = FALSE)

projection(districts_sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
projection(regions_sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

names(districts_sp)[which(names(districts_sp) == "District_1")] <- "District"
districts_sp.f <- fortify(districts_sp, region = "District")
regions_sp.f <- fortify(regions_sp, region = "Region_Nam")

colnames(districts_sp.f)[colnames(districts_sp.f) == "id"] <- "District"
colnames(regions_sp.f)[colnames(regions_sp.f) == "id"] <- "Region"

load(file.path("simdat", "AnalysisDat.RData"))
tempdat <- unique(AnalysisDat[, c("District", "Strata", "Stratification.5b", "Stratification.5b_withoutUrban")])
tempDat.df <- dplyr::left_join(districts_sp.f, tempdat)

pplot <- ggplot(, warnings = FALSE) +
  geom_polygon(
    data = tempDat.df,
    aes(x = long, y = lat, group = group, fill = Stratification.5b), color = "black", size = 0.35
  ) +
  geom_polygon(data = regions_sp.f, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.75) +
  theme_nothing() +
  scale_fill_manual(values = StrataCols, drop = FALSE)


ggsave("Fig_1map.png", plot = pplot, path = 'figures', width = 12, height = 10, device = "png")
ggsave("Fig_1map.pdf", plot = pplot, path = 'figures', width = 12, height = 10, device = "pdf")

