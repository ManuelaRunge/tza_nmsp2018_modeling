cat(paste0('Start running Figure1.R'))

shp_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

### Shapefiles obtained from NMCP Tanzania
use_old_boundaries = F
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


load(file.path("simdat", "AnalysisDat.RData"))
tempdat <- unique(AnalysisDat[, c("District", "Strata")])

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
    aes(x = long, y = lat, group = group, fill = Strata), color = "black", size = 0.35
  ) +
  geom_polygon(data = regions_sp.f, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.75) +
  theme_nothing() +
  scale_fill_manual(values = StrataCols, drop = FALSE)

pplot
ggsave("Fig_1map.png", plot = pplot, path = 'figures', width = 12, height = 10, device = "png")
#ggsave("Fig_1map.pdf", plot = pplot, path = 'figures', width = 12, height = 10, device = "pdf")


##---------------------------------------------
### Separate by intervention
##---------------------------------------------
load(file.path("simdat", "AnalysisDat.RData"))
AnalysisDat <- AnalysisDat %>% 
  mutate(PR = PR * 100) %>%
  filter(year==2020 & statistic=='median')

NMSPdat_long <- f_load_nmsp_scendat()

AnalysisDat2 <- full_join(AnalysisDat, NMSPdat_long[, c('District', 'FutScen', 'Strategy', 'Strategy_FutScen_nr')]) %>%
  filter(!(is.na(Strategy)))
dim(AnalysisDat2)

table(AnalysisDat2$Strategy, AnalysisDat2$CMincrease, exclude = NULL)
length(unique(NMSPdat_long$Strategy))
length(unique(AnalysisDat2$Strategy))

AnalysisDat2 %>%
  filter(Strategy %in% selectedStrategies) %>% 
  dplyr::select(Strata,Strategy,FutScen_nr,FutScen) %>% 
  unique() %>% arrange(Strategy,Strata)

AnalysisDat2$Cases.pP <- AnalysisDat2$Cases / simPop
AnalysisDat2$incidence <- (AnalysisDat2$Cases / simPop) * 1000

AnalysisDat2$StrataLabel <- factor(AnalysisDat2$Strata,
                             levels = c("very low", "low", "urban", "moderate", "high"),
                             labels = c("very low", "low", "urban", "moderate", "high"))

dat_20152020 <- AnalysisDat2 %>%
  dplyr::filter( Strategy %in% selectedStrategies[1]) %>%
  dplyr::select(District, StrataLabel, Strategy, FutScen, FutScen_nr, FutScen_label,
                futCMlabel, futITNlabel, futSNPlabel, futMDAlabel, futIRSlabel, futLARVlabel, futIPTSClabel) %>%
  as.data.table()

dat_20182020 <- AnalysisDat2 %>%
  dplyr::filter(Strategy %in% selectedStrategies[2]) %>%
  dplyr::select(District, StrataLabel, Strategy, FutScen, FutScen_nr, FutScen_label,
                futCMlabel, futITNlabel, futSNPlabel, futMDAlabel, futIRSlabel, futLARVlabel, futIPTSClabel) %>%
  as.data.table()

dim(dat_20152020)
dim(dat_20182020)

dat_20152020 <- dat_20152020 %>%
  dplyr::select(-c(FutScen, FutScen_label, Strategy, FutScen_nr, futMDAlabel, futLARVlabel)) %>%
  pivot_longer(cols = -c(District, StrataLabel))

dat_20182020 <- dat_20182020 %>%
  dplyr::select(-c(FutScen, FutScen_label, Strategy, FutScen_nr, futMDAlabel, futLARVlabel)) %>%
  pivot_longer(cols = -c(District, StrataLabel))

futINTlabels <- c("futCMlabel", "futITNlabel", "futSNPlabel", "futIRSlabel", "futIPTSClabel")  #"futLARVlabel"
dat_20152020$name <- factor(dat_20152020$name, levels = futINTlabels, labels = futINTlabels)
dat_20182020$name <- factor(dat_20152020$name, levels = futINTlabels, labels = futINTlabels)

intervention_map <- function(x) {
  ggList <- lapply(split(x, x$name), function(i) {
    ggplot(i, warnings = FALSE) +
      geom_polygon(
        aes(x = long, y = lat, group = group, fill = value), color = "lightgrey", size = 0.25
      ) +
      geom_polygon(data = admin1_sp.f, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.35) +
      theme_map() +
      scale_fill_manual(values = c('orange', 'deepskyblue3'), na.value = NA, drop = FALSE) +
      facet_wrap(~name, nrow = 1) +
      labs(fill = '') +
      theme(legend.position = 'bottom') })

  pplot <- cowplot::plot_grid(plotlist = ggList, nrow = 1, align = 'hv', labels = levels(x$name))
  return(pplot)
}

admin1_sp.f <- regions_sp.f
map_20152020 <- intervention_map(x = districts_sp.f %>% dplyr::left_join(dat_20152020))
map_20182020 <- intervention_map(x = districts_sp.f %>% dplyr::left_join(dat_20182020))

pplot <- plot_grid(map_20152020, map_20182020, nrow = 2, align = 'hv', labels = c('A', 'B'))
pplot

ggsave("map2_NMSP_20152020_20182020.png", plot = pplot, path = 'figures', width = 20, height = 9, device = "png")
ggsave("map2_NMSP_20152020_20182020.pdf", plot = pplot, path = 'figures', width = 20, height = 9, device = "pdf")


