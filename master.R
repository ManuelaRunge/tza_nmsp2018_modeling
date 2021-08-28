packages_needed <- c('DescTools', 'BlandAltmanLeh', 'xlsx', 'gsubfn', 'grid', 'tidyverse', 'data.table', 'plyr',
                     'gridExtra', 'reshape', 'Rmisc', 'iterators', 'RColorBrewer', 'cowplot', 'ggExtra', 'scales', 'raster', 'rworldmap',
                     'ggmap', 'rgdal', 'plyr', 'rgeos', 'sp', 'maptools', 'spatstat', 'rasterVis', 'tableone', 'here')
lapply(packages_needed, require, character.only = TRUE)

PaperDir <- file.path('C:/Users/mrung/Documents/Paper/Paper2')
PapersimdatDir <- file.path(PaperDir, 'reproducedPipeline_RESIM_v2/simdat')
SettingsDir <- file.path(PaperDir, 'reproducedPipeline_RESIM_v2', 'scripts', "../settings")
PaperDataDir <- file.path(PaperDir, 'reproducedPipeline_RESIM_v2/dat')
FunctionsDir <- file.path(PaperDir, 'reproducedPipeline_RESIM_v2/scripts/rlibrary')

source(file.path("rlibrary", "customObjects.R"))
#source(file.path("rlibrary", "mappingObjects.R"))
source(file.path("rlibrary", "f_AggrDat.R"))
source(file.path("rlibrary", "f_mergevars.R"))
source(file.path("rlibrary", "f_grid_arrange_shared_legend.R"))
source(file.path("rlibrary", "f_spread.R"))
source(file.path("rlibrary", "f_customBoxplot.R"))


## ==========================
### SETTINGS
## ==========================

customTheme_noAngle <- theme(
  # strip.text.x = element_text(size = 16, face="plain"),
  #  strip.text.y = element_text(size = 16, face="plain"),
  panel.spacing.x = unit(1, "line"),
  strip.placement = "outside",
  strip.background = element_rect(colour = "white", fill = "white"),
  strip.text.x = element_text(size = 14, face = "bold"),
  strip.text.y = element_text(size = 14, face = "bold"),
  plot.title = element_text(size = 20, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14, angle = 0),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14)
)

customTheme_noAngle2 <- theme(
  # strip.text.x = element_text(size = 16, face="plain"),
  #  strip.text.y = element_text(size = 16, face="plain"),
  panel.spacing.x = unit(1, "line"),
  strip.placement = "outside",
  strip.text.x = element_text(size = 14, face = "bold"),
  strip.text.y = element_text(size = 14, face = "bold"),
  plot.title = element_text(size = 20, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14, angle = 0),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14)
)

customTheme_Angle <- theme(
  # strip.text.x = element_text(size = 16, face="plain"),
  #  strip.text.y = element_text(size = 16, face="plain"),
  panel.spacing.x = unit(1, "line"),
  strip.placement = "outside",
  strip.background = element_rect(colour = "black", fill = "white"),
  strip.text.x = element_text(size = 14, face = "bold"),
  strip.text.y = element_text(size = 14, face = "bold"),
  plot.title = element_text(size = 20, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 12, angle = 90),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14)
)

selectedStrat1 <- c(
  "NMSPcurrent",
  "NMSPcurrent.withCM",
  "revNMSP4a_adj_CMall",
  "revNMSP4b_adj_CMall_withIPTsc",
  "revNMSP4d_adj_CMall_withIPTsc_LARVurbanOnly_MDAinVeryLow"
)

selectedStrat2 <- c(
  "NMSPcurrent",
  "NMSPcurrent.withCM",
  "revNMSP7a_adj_VConly",
  "revNMSP4a_adj_CMall",
  "revNMSP4d_adj_CMall_withIPTsc_LARVurbanOnly_MDAinVeryLow"
)
StrategyLevelsSub <- c(
  "lowestCost",
  "NMSPcurrent",
  "counterfactual",
  "NMSPcurrent.withCM",
  "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM",
  "revNMSP4c_adj_CMall_withIPTsc_LARVurbanOnly",
  "revNMSP4d_adj_CMall_withIPTsc_MDA_noLARV_inVeryLow"
)
StrategySub <- c(
  "revNMSP3b_adj_withIPTsc",
  "revNMSP4b_adj_CMall_withIPTsc",
  "revNMSP4d_adj_CMall_withIPTsc_MDA_noLARV_inVeryLow",
  "revNMSP4c_adj_CMall_withIPTsc_LARVurbanOnly",
  "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM"
) # selectedStrat1


options(scipen = 10000)
point <- format_format(big.mark = "'", decimal.mark = ".", scientific = FALSE)


### ------------------Defining subsets of interventions---------------------
incrementalFutScenLabels <- c(
  "increase in CM", "increase in CM - ITN MRC",
  "increase in CM - ITN MRC-IPTsc",
  "increase in CM - ITN MRC continuous",
  "increase in CM - ITN MRC continuous-IPTsc",
  "increase in CM - ITN MRC continuous-IRS",
  "increase in CM - ITN MRC continuous-IRS-IPTsc"
)


incrementalFutScenLabels <- c(
  "increase in CM", "increase in CM - ITN MRC",
  "increase in CM - ITN MRC-IRS",
  "increase in CM - ITN MRC-IPTsc",
  "increase in CM - ITN MRC continuous",
  "increase in CM - ITN MRC continuous-IPTsc",
  "increase in CM - ITN MRC continuous-IRS",
  "increase in CM - ITN MRC continuous-IRS-IPTsc"
)


###  NMSP Strategies to compare
selectedStrategies <- c("NMSPcurrent.withCM", "revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM") # c("NMSPcurrent.withCM","revNMSP8a_new_IPTscHighOnly_SMCmoderat_noLSM")


## Strata colors
StrataCols <- c("very low" = prevcolsAdj[2], "low" = prevcolsAdj[3], "urban" = prevcolsAdj[1], "moderate" = prevcolsAdj[4], "high" = prevcolsAdj[5])


### To do - update selection of FutScen_nr based on intervention variables
incrementalInterventions <- c(33, 45, 48, 47)
# Create incremental impact maps
singleInterventions <- c(1, 2, 3, 6, 27, 15, 33)
selectedInterventions <- c(singleInterventions, incrementalInterventions[!(incrementalInterventions %in% singleInterventions)])

MDAinterventions <- c(2, 32, 44, 56)
selectedInterventions_withMDA <- c(selectedInterventions, MDAinterventions)

## Strategies based in predicted impat
CALCULATED.STRATEGIES <- "revisedNMSP" # c("lowestCost", "smallestICER") #,"revisedNMSP"   ## potentially adding more calculated strategies / stratifications based in impact
SUBSETFORCOMPLETEANALYSIS <- c("") # "noLSM" ## c("")   ## how to generalise?
subDat <- "noSub" ## sub for ICER and lowestCosts, not sub for whole analysis

## for all cases simPop =10'000 (not age stratified)
simPop <- 10000
DistrictVersion <- "2018" # "2016"

MonitoringStart <- 2003
MonitoringEnd <- 2022
baselineYear <- 2016
EvaluationStart <- 2017
EvaluationEnd <- 2020

EvaluationYears <- c((baselineYear + 1):EvaluationEnd)
historicalYears <- c(MonitoringStart:2012) ## c(MonitoringStart:baselineYear)   2012 specific for OM-TZA



