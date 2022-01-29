cat(paste0('Start running Figure2.R'))

shp_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

### Shapefiles obtained from NMCP Tanzania
use_old_boundaries = F
if (!use_old_boundaries) {
  districts_sp <- shapefile(file.path("shapefiles", "TZA_Districts.shp"), verbose = FALSE)
  regions_sp <- shapefile(file.path("shapefiles", "TZA_Regions.shp"), verbose = FALSE)
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

tempdat <- fread(file.path("dat", "TZADistrictDat.csv"))
tempdat <- unique(tempdat[, c("District", "Strata")])
dim(tempdat)

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
  scale_fill_manual(values = StrataCols, drop = FALSE) +
  theme(legend.position = 'right')

ggsave("Fig_2map.png", plot = pplot, path = 'figures', width = 12, height = 10, device = "png")
if (SAVEpdf)ggsave("Fig_2map.pdf", plot = pplot, path = 'figures', width = 12, height = 10, device = "pdf")


##---------------------------------------------
### Separate by intervention
##---------------------------------------------
load(file.path("simdat", "AnalysisDat.RData"))
AnalysisDat <- AnalysisDat %>%
  mutate(PR = PR * 100) %>%
  filter(year == 2020 & statistic == 'median')

AnalysisDat2 <- full_join(AnalysisDat, NMSPdat_long[, c('District', 'FutScen', 'Strategy', 'Strategy_FutScen_nr')]) %>%
  filter(!(is.na(Strategy)))
dim(AnalysisDat2)
rm(AnalysisDat)

table(AnalysisDat2$Strategy, AnalysisDat2$CMincrease, exclude = NULL)
length(unique(NMSPdat_long$Strategy))
length(unique(AnalysisDat2$Strategy))

AnalysisDat2 %>%
  filter(Strategy %in% selectedStrategies) %>%
  dplyr::select(Strata, Strategy, FutScen_nr, FutScen) %>%
  unique() %>%
  arrange(Strategy, Strata)

AnalysisDat2$Cases.pP <- AnalysisDat2$Cases / simPop
AnalysisDat2$incidence <- (AnalysisDat2$Cases / simPop) * 1000
AnalysisDat2$StrataLabel <- factor(AnalysisDat2$Strata, levels = Strata_labels, labels = Strata_labels)

dat_20152020 <- AnalysisDat2 %>%
  dplyr::filter(Strategy %in% selectedStrategies[1]) %>%
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
futINTlabels_new <- gsub('label', '', gsub('fut', '', futINTlabels))
futINTlabels_new <- gsub('SNP', 'ITN-SNP', gsub('ITN', 'ITN-MRC', futINTlabels_new))
futINTlabels_new <- gsub('IPTSC', 'IPTsc', futINTlabels_new)

dat_20152020$name <- factor(dat_20152020$name, levels = futINTlabels, labels = futINTlabels_new)
dat_20182020$name <- factor(dat_20182020$name, levels = futINTlabels, labels = futINTlabels_new)

dat_20152020$value_yn <- 'yes'
dat_20152020$value_yn[grep('no', dat_20152020$value)] <- 'no'

dat_20182020$value_yn <- 'yes'
dat_20182020$value_yn[grep('no', dat_20182020$value)] <- 'no'

table(dat_20182020$name, dat_20182020$value_yn)

fill_yn <- c('no' = 'lightgrey', 'yes' = 'darkorange')

intervention_map <- function(x) {
  ggList <- lapply(split(x, x$name), function(i) {
    ggplot(i, warnings = FALSE) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = value_yn),
                   color = "grey", size = 0.25) +
      geom_polygon(data = admin1_sp.f, aes(x = long, y = lat, group = group),
                   color = "black", fill = NA, size = 0.35) +
      theme_map() +
      facet_wrap(~name, nrow = 1) +
      theme(legend.position = 'none') })

  pplot <- cowplot::plot_grid(plotlist = ggList, nrow = 1, align = 'hv', labels = '') #levels(x$name)
  return(pplot)
}

admin1_sp.f <- regions_sp.f
map_20152020 <- intervention_map(x = districts_sp.f %>% dplyr::left_join(dat_20152020))
map_20182020 <- intervention_map(x = districts_sp.f %>% dplyr::left_join(dat_20182020))
plegend <- get_legend(ggplot(data = districts_sp.f %>% dplyr::left_join(dat_20152020), warnings = FALSE) +
                        geom_polygon(aes(x = long, y = lat, group = group, fill = value_yn),
                                     color = "grey", size = 0.25) +
                        scale_fill_manual(values = fill_yn, na.value = NA, drop = FALSE) +
                        labs(fill = '') +
                        customTheme_noAngle)

pplot <- plot_grid(map_20152020, map_20182020, nrow = 2, align = 'hv', labels = c('A', 'B'))
pplot <- plot_grid(pplot, plegend, nrow = 1, rel_widths = c(1, 0.2))

ggsave("Fig2_map_NMSP_20152020_20182020.png", plot = pplot, path = 'figures', width = 20, height = 9, device = "png")
if (SAVEpdf)ggsave("Fig2_map_NMSP_20152020_20182020.pdf", plot = pplot, path = 'figures', width = 20, height = 9, device = "pdf")

rm(tempdat, dat_20152020, dat_20182020, map_20152020, map_20182020, regions_sp.f, admin1_sp.f, pplot, AnalysisDat2)
