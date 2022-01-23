## ===================================================================
## Tanzania councils and simulated district names with strata
## Councils names and strata citation:
## Thawer, S.G., Chacky, F., Runge, M. et al. Sub-national stratification of malaria risk in mainland Tanzania:
## a simplified assembly of survey and routine data. Malar J 19, 177 (2020). https://doi.org/10.1186/s12936-020-03250-4
## ===================================================================

## download council with strata file
## https://malariajournal.biomedcentral.com/articles/10.1186/s12936-020-03250-4#Sec24
try_download = F
si2_path <- file.path('dat', '12936_2020_3250_MOESM2_ESM.xlsx')
if (try_download) {
  si2_url <- 'https://static-content.springer.com/esm/art%3A10.1186%2Fs12936-020-03250-4/MediaObjects/12936_2020_3250_MOESM2_ESM.xlsx'
  download.file(si2_url, si2_path)
}

## council to simulated district names
name_dic <- fread(file.path('dat', 'tza_countil_district_names.csv'))
urban_strata_councils <- c('Arusha CC', 'Ilala MC', 'Kigamboni MC', 'Kinondoni MC', 'Temeke MC', 'Ubungo MC',
                           'Dodoma MC', 'Iringa MC', 'Bukoba MC', 'Mpanda MC', 'Kigoma MC', 'Moshi MC', 'Lindi MC',
                           'Musoma MC', 'Mbeya DC', 'Morogoro MC', 'Mtwara MC', 'Ilemela MC', 'Nyamagana MC',
                           'Sumbawanga MC', 'Songea MC', 'Shinyanga MC', 'Singida MC', 'Tabora MC', 'Tanga CC')
length(urban_strata_councils) #25

TZA_dat <- read_excel(si2_path, skip = 1) %>%
  dplyr::select(Region, Council, Stratum) %>%
  mutate(Region = ifelse(Region == 'Dar Es Salaam Region', 'Daressalaam', gsub(" Region", "", Region)),
         Strata = tolower(Stratum),
         Strata_urban = ifelse(Council %in% urban_strata_councils, 'urban', Strata)) %>%
  left_join(name_dic) %>%
  dplyr::select(Region, District, Council, Strata, Strata_urban)

fwrite(TZA_dat, file.path('dat', 'TZA_dat.csv'))

