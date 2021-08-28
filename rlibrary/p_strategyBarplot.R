p_strategyBarplot <- function(data, selectedStrategies, selectedBlanketInterventions, yvarName, xvarName, out = "plot") {

  ### data 				<- OMStrategyDat
  ### selectedStrategies 	<- "NMSPcurrent.withCM_noITNinLow"
  ### yvarName 			<- "nDistricts"
  ### xvarName 			<- "year"
  ### selectedBlanketInterventions <- c("")# c(33, 45, 48, 47)  ### or empty c("") if looking at strategies
  ### Notes:
  ### yvarName needs to be the exact variable name to be plotted on y

  selectedYears <- c(baselineYear:EvaluationYears[length(EvaluationYears)])

  if (sum(nchar(selectedBlanketInterventions)) > 1) {
    groupVars <- c("Strategy", "Strategy_adj", "FutScen_label", "year", "PR.grp")
    filterExpr <- paste0('(Strategy=="counterfactual" | (Strategy=="Blanket" & FutScen_nr %in% selectedBlanketInterventions )) & statistic =="median"')
  }
  if (sum(nchar(selectedBlanketInterventions)) == 0) {
    groupVars <- c("Strategy", "Strategy_adj", "year", "PR.grp")
    filterExpr <- paste0('Strategy %in% c("counterfactual",selectedStrategies) & statistic =="median"')
  }


  DatForPlot_ndis <- data %>%
    dplyr::filter(eval(parse(text = filterExpr, srcfile = NULL))) %>%
    dplyr::group_by_(.dots = groupVars) %>%
    dplyr::select_(.dots = groupVars) %>%
    tally() %>%
    dplyr::mutate(nDistricts = n) %>%
    dplyr::select(-n) %>%
    as.data.frame()

  DatForPlot_pPop <- data %>%
    dplyr::filter(eval(parse(text = filterExpr, srcfile = NULL))) %>%
    dplyr::group_by_(.dots = groupVars) %>%
    dplyr::summarize(population = sum(Population_2016)) %>%
    dplyr::mutate(percpop = (population / sumPop) * 100) %>%
    as.data.frame()

  mergevars <- f_mergevars(DatForPlot_ndis, DatForPlot_pPop)
  DatForPlot <- merge(DatForPlot_ndis, DatForPlot_pPop, by = mergevars)

  ## ===========================
  ## x axis and facet specifications

  ### Compare several years for ONE strategy
  if (xvarName == "year") {
    plotdat <- DatForPlot %>% dplyr::filter((Strategy == "counterfactual" & (year %in% selectedYears[c(1, length(selectedYears))])) |
                                              (Strategy %in% selectedStrategies & year %in% selectedYears[-1]))

    xlabel <- ""
    plotdat$xtempvar <- plotdat[, colnames(plotdat) == xvarName]

    plotdat$facetVar <- as.character(plotdat$Strategy_adj)
    plotdat$facetVar[plotdat$facetVar != "counterfactual"] <- "Stratified interventions"
    plotdat$facetVar[plotdat$facetVar == "counterfactual"] <- "Counter\nfactual"
    plotdat$facetVar[plotdat$facetVar == "Counter\nfactual" & plotdat$year == baselineYear] <- "Base\nline"

    if (sum(nchar(selectedBlanketInterventions)) == 0) plotdat$facetVar <- factor(plotdat$facetVar, levels = c("Base\nline", "Stratified interventions", "Counter\nfactual"))
    if (sum(nchar(selectedBlanketInterventions)) > 1) plotdat$facetVar <- factor(plotdat$facetVar, levels = c("Base\nline", "Stratified interventions", "Counter\nfactual"), labels = c("Base\nline", "Blanket interventions", "Counter\nfactual"))
  }


  ### Compare several Strategies for ONE year ( compared to baseline though)
  if (xvarName == "Strategy") {

    # plotdat <- DatForPlot  %>%  dplyr::filter((Strategy=="counterfactual" & (year %in% selectedYears[1])) |
    # 										   (Strategy !="counterfactual" & year %in% selectedYears[length(selectedYears)]))
    #
    plotdat <- DatForPlot %>% dplyr::filter((Strategy == "counterfactual" & (year %in% c(selectedYears[1], selectedYears[length(selectedYears)]))) |
                                              (Strategy != "counterfactual" & year %in% selectedYears[length(selectedYears)]))

    xlabel <- ""
    plotdat$xtempvar <- plotdat[, colnames(plotdat) == xvarName]
    plotdat$facetVar <- plotdat[, colnames(plotdat) == "year"]
    plotdat$facetVar[plotdat$year %in% selectedYears[length(selectedYears)]] <- paste0("Predicted impact for ", selectedYears[length(selectedYears)])
    plotdat$facetVar[plotdat$year %in% selectedYears[1]] <- paste0("Baseline ", selectedYears[1])

    if (sum(nchar(selectedBlanketInterventions)) > 1) plotdat$xtempvar[plotdat$Strategy != "counterfactual"] <- plotdat$FutScen_label[plotdat$Strategy != "counterfactual"]
  }


  ## ===========================
  ###### Y axis specifications

  if (yvarName == "nDistricts") {
    ylabel <- "Number of districts"
    uplim <- 181.5
    plotdat$tempvar <- plotdat[, colnames(plotdat) == yvarName]
    plotdat$templabel <- plotdat$tempvar
  }
  if (yvarName == "percpop") {
    ylabel <- "Population (%)"
    uplim <- 100.5
    plotdat$tempvar <- plotdat[, colnames(plotdat) == yvarName]
    plotdat$templabel <- round(plotdat$tempvar, 1)
  }


  pplot <- ggplot(data = plotdat, aes(x = as.character(xtempvar), y = tempvar, fill = PR.grp, label = templabel)) +
    geom_bar(, stat = "identity") +
    facet_grid(~facetVar, scale = "free_x", space = "free_x") +
    labs(title = "", subtitle = "", x = "", y = ylabel, fill = expression(italic("PfPR")["2 to 10"] * " (%)")) +
    scale_fill_manual(values = prevcols) +
    # scale_y_continuous(expand=c(0,1),lim=c(0,uplim))+
    geom_text(size = 6, position = position_stack(vjust = 0.5)) +
    customTheme

  if (out == "plot") return(pplot)
  if (out == "dataframe") return(plotdat)
}


## Testing  - varying interventions
## ( blanket vs stratified), varying yaxis, varying comparison (comparing stratgies per year or years per strategy)
## output either plot (default) or dataset
# p_strategyBarplot(OMStrategyDat, "Blanket",c(12), "nDistricts", "year")
# p_strategyBarplot(OMStrategyDat, "Blanket",c(12), "percpop", "year")
# p_strategyBarplot(OMStrategyDat, "Blanket",c(12, 45, 33), "percpop", "Strategy")
# p_strategyBarplot(OMStrategyDat, "Blanket",c(12), "percpop", "year", "dataframe")
