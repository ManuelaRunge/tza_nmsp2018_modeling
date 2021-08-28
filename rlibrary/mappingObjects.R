selectedProjection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

###=================================
#### DEFINE BOUNDARIES
###=================================
Admin1Boundary <- "TZA_Regions.shp"
Admin2Boundary <- "TZA_Districts.shp"

districts_sp <- shapefile(file.path(PaperDataDir, 'Tanzania Regions & Districts SHP (220818)', Admin2Boundary), verbose = FALSE)
regions_sp <- shapefile(file.path(PaperDataDir, 'Tanzania Regions & Districts SHP (220818)', Admin1Boundary), verbose = FALSE)

projection(districts_sp) <- selectedProjection
projection(regions_sp) <- selectedProjection

names(districts_sp)[which(names(districts_sp) == "District_1")] <- "District"

districts_sp.f <- fortify(districts_sp, region = "District")
regions_sp.f <- fortify(regions_sp, region = "Region_Nam")

colnames(districts_sp.f)[colnames(districts_sp.f) == "id"] <- "District"
colnames(regions_sp.f)[colnames(regions_sp.f) == "id"] <- "Region"


