## ===================================================================
## Figure 1, TZA map
## ===================================================================
library(tidyverse)
library(raster)
library(malariaAtlas) # better shp to spatial dataframe function

source(file.path("rlibrary", "customObjects.R"))
shp_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

### Shapefiles obtained from NMCP Tanzania
use_old_boundaries = T
if (!use_old_boundaries) {
  districts_sp <- shapefile(file.path("Shapefiles", "TZA_Districts.shp"), verbose = FALSE)
  regions_sp <- shapefile(file.path("Shapefiles", "TZA_Regions.shp"), verbose = FALSE)
  district_sp_name_old <- 'District_1'
}else {
  ### For public repository use public available shapefiles
  regions_sp <- getData("GADM", country = "TZA", level = 1)
  districts_sp <- getData("GADM", country = "TZA", level = 2)
  district_sp_name_old <- 'NAME_2'
}


projection(districts_sp) <- shp_proj
projection(regions_sp) <- shp_proj

names(districts_sp)[which(names(districts_sp) == district_sp_name_old)] <- "District"
### as.MAPshp more convenient to use than fortify
districts_sp.f <- as.MAPshp(districts_sp)
regions_sp.f <- as.MAPshp(regions_sp)


load(file.path("dat", "AnalysisDat.RData"))
tempdat <- unique(AnalysisDat[, c("District", "Strata", "Stratification.5b", "Stratification.5b_withoutUrban")])

if (use_old_boundaries) {
  dis2 <- sort(unique(tempdat$District)[!(unique(tempdat$District) %in% unique(districts_sp.f$District))])
  source(file.path('rlibrary', 'f_adjust_tza_old_new_boundaries.R'))
  tempdat$District[tempdat$District %in% dis2] <- str_replace_all(tempdat$District[tempdat$District %in% dis2],
                                                                  dis_new_to_old)

}

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
#ggsave("Fig_1map.pdf", plot = pplot, path = 'figures', width = 12, height = 10, device = "pdf")

