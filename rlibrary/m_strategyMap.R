m_strategyMap <- function(tempdat, selectedStrategies, selectedBlanketInterventions, yvarName, selectedStatistic, selectedYears) {
  # tempdat  			= OMStrategyDat
  # selectedYears 		= 2020
  # selectedStrategies 	= "NMSPcurrent"
  # selectedBlanketInterventions 	=  33  ## 33 , 45, 48, 47  ## single interventiony only
  # yvarName 			= "PR.grp"

  customTitle <- ""
  if (length(selectedStrategies) == 1 & length(selectedBlanketInterventions) == 1) {
    if (selectedStrategies == "NMSPcurrent") customTitle <- "NMSP 2015-2020\nmaintained case management"
    if (selectedStrategies == "NMSPcurrent.withCM") customTitle <- "NMSP 2015-2020\nimproved case management"
    if (selectedStrategies == "NMSPcurrent.withCM_noITNinLow") customTitle <- "NMSP 2015-2020\nimproved case management discontinued ITNs in very low prevalence districts"
    if (selectedStrategies == "lowestCost") customTitle <- "Intervention strategy reaching NMSP target\n(Prealence of less than 1% by 2020 at national level)"
    if (selectedStrategies == "smallestICER") customTitle <- "Most cost-effective intervention strategy)"
    if (selectedStrategies == "counterfactual") customTitle <- "Maintaining case management only)"
    if (sum(nchar(selectedBlanketInterventions)) != 0) customTitle <- unique(tempdat$FutScen_label[tempdat$FutScen_nr == selectedBlanketInterventions])
    if (customTitle == "") customTitle <- selectedStrategies
  }

  tempdat <- subset(tempdat, year %in% selectedYears &
    Strategy %in% selectedStrategies &
    statistic == selectedStatistic)
  if (sum(nchar(selectedBlanketInterventions)) > 0) tempdat <- subset(tempdat, FutScen_nr %in% selectedBlanketInterventions)
  tempdat$tempvar <- tempdat[, colnames(tempdat) == yvarName]

  tempDat.df <- dplyr::left_join(districts_sp.f, tempdat, by = "District")

  if (length(selectedStrategies) == 1 &
    length(selectedYears) == 1 &
    length(selectedBlanketInterventions) == 1) {
    suppressWarnings(
      pmap <- ggplot(, warnings = FALSE) +
        geom_polygon(
          data = tempDat.df,
          aes(x = long, y = lat, group = group, fill = tempvar), color = "white", size = 0.35
        ) +
        geom_polygon(data = regions_sp.f, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.75) +
        theme_nothing(legend = TRUE) +
        prevLegend_cat +
        labs(title = customTitle, fill = expression(italic("Pf") * "PR"[2][to][10])) +
        map.theme
    )
  }

  if (length(selectedStrategies) > 1 |
    length(selectedYears) > 1 |
    length(selectedBlanketInterventions) > 1) {
    if (length(selectedBlanketInterventions) > 1) tempDat.df$facetvar <- tempDat.df$FutScen_label
    if (length(selectedStrategies) > 1) tempDat.df$facetvar <- tempDat.df$Strategy
    if (length(selectedYears) > 1) tempDat.df$facetvar <- tempDat.df$year

    suppressWarnings(
      pmap <- ggplot(, warnings = FALSE) +
        geom_polygon(
          data = tempDat.df,
          aes(x = long, y = lat, group = group, fill = tempvar), color = "white", size = 0.35
        ) +
        geom_polygon(data = regions_sp.f, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.75) +
        theme_nothing(legend = TRUE) +
        prevLegend_cat +
        labs(title = customTitle, fill = expression(italic("Pf") * "PR"[2][to][10])) +
        facet_wrap(~facetvar) +
        map.theme
    )
  }


  return(pmap)
}
