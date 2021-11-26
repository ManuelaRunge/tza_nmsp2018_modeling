## See details on data collation of MIS data per region and district in:
## Runge, M., Snow, R.W., Molteni, F., Thawer, S., Mohamed, A., Mandike, R., Giorgi, E., Macharia, P.M., Smith, T.A., Lengeler, C., Pothin, E., 2020.
## Simulating the council-specific impact of anti-malaria interventions: A tool to support malaria strategic planning in Tanzania.
## PLoS ONE 15, e0228469. https://doi.org/10.1371/journal.pone.0228469

f_get_MIS_cm_dat <- function() {
  ## CM_data_allDis_v1
  ## MIS 2015/2016 -> % of febrile children sought advice or treatment at any facility in last 2 weeks,
  ## extracted per region from household clusters
  ## * 0.6 from Galactionova et al. 2015 to convert treatment seeking to effective treatment coverage
  ## polynomial regression 2000-2016 to get annual historical trend
  CMdat <- fread(file.path('dat', "CM_data_allDis_v1.csv"))

  CMdat <- CMdat %>%
    dplyr::mutate(District = as.character(District)) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarize(MISreg_CMeffectiveCoverage_2016 = mean(MISreg_CMeffectiveCoverage_2016)) %>%
    dplyr::select(Region, MISreg_CMeffectiveCoverage_2016)

  return(CMdat)
}



