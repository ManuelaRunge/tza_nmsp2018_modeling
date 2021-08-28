####==================================================
#   Script to manage custom configurations for the project
#   Project: Malaria modelling in Tanzania
#   Manuela Runge, manuela.runge@swisstph.ch
#
####==================================================

startTime <- Sys.time()

## ----Settings,-------------------------------------------------
LOCAL = TRUE
SAVE = TRUE
LOAD = FALSE
ASSESSFIT = TRUE
DESCRIBEPARAMETER = TRUE
VALIDATE = FALSE
COSTCALCULATION = TRUE
addPBOProxyStrategy = FALSE
createBarplot = TRUE
incrementalMAPS = TRUE
resistancecomparison = FALSE    ## resistancecomparison might not be needed for all experiments or for all projects
createBarplot = TRUE


### To do - update selection of FutScen_nr based on intervention variables
incrementalInterventions <- c(33, 45, 48, 47)
# Create incremental impact maps 	
singleInterventions <- c(1, 2, 3, 6, 27, 15, 33)
selectedInterventions <- c(singleInterventions, incrementalInterventions[!(incrementalInterventions %in% singleInterventions)])


# MDAinterventions <- JAGSresults_wide %>%
#    dplyr::filter(MDAcov !=0 & LARVcov==0 & IRScov==0 & futITNcov!=0.5 & futSNPcov!=0.4 ) %>%
#    dplyr::select(FutScen_nr, FutScen_label) %>%
#    unique()
# MDAinterventions
#
##    FutScen_nr                       FutScen_label
## 1            2                                 MDA
## 241         32                  increase in CM-MDA
## 321         44        increase in CM - ITN MRC-MDA
## 401         56 increase in CM - ITN continuous-MDA
##
# MDAinterventions <- MDAinterventions$FutScen_nr
MDAinterventions <- c(2, 32, 44, 56)
selectedInterventions_withMDA <- c(selectedInterventions, MDAinterventions)

## Strategies based in predicted impat
CALCULATED.STRATEGIES <- "revisedNMSP" # c("lowestCost", "smallestICER") #,"revisedNMSP"   ## potentially adding more calculated strategies / stratifications based in impact
SUBSETFORCOMPLETEANALYSIS <- c("") # "noLSM" ## c("")   ## how to generalise?
subDat <- "noSub" ## sub for ICER and lowestCosts, not sub for whole analysis

## ----packages,------------------------------------------------
### packages
packages_needed <- c('DescTools', 'BlandAltmanLeh', 'xlsx', 'gsubfn', 'grid', 'tidyverse', 'data.table', 'plyr',
                     'gridExtra', 'reshape', 'Rmisc', 'iterators', 'RColorBrewer', 'cowplot', 'ggExtra', 'scales', 'raster', 'rworldmap',
                     'ggmap', 'rgdal', 'plyr', 'rgeos', 'sp', 'maptools', 'spatstat', 'SDMTools', 'rasterVis', 'tableone', 'here')
lapply(packages_needed, require, character.only = TRUE)


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


#### ==================== LOCAL ==============================================
username <- as.character((Sys.info()["user"]))
UserDir <- file.path("../../../..", username)
gitrepo <- file.path("C:/git_repos")
FunctionsDir <- file.path(gitrepo, "runge/rlibrary")
LibraryDir <- FunctionsDir
LookUpDir <- file.path(gitrepo, "runge/rlookup")
LIBLOC <- file.path(UserDir, "Documents/R/win-library/3.4")

# Projects
OMDir <- file.path(UserDir, "SwissTPH/OpenMalaria")
#ProjectsDir <- file.path(UserDir, "PhD/Projects")
ProjectsDir <- file.path(UserDir, "Box/Tanzania")

DataDir <- file.path(ProjectsDir, "OM_TZA/3. Data/RawDat")

ProjectDir <- file.path(ProjectsDir, "OM_TZA")
OMDir <- file.path(ProjectDir, "5_OpenMalaria")
SettingsDir <- file.path(PaperScriptDir, "Settings")
FunctionsDir <- file.path(PaperScriptDir, "rlibrary")

### Data set directories (generated)
ProjectDataDir <- file.path(ProjectDir, "3. Data/EditedDat")
BoundaryDataDir <- file.path(ProjectDataDir, "Boundaries")
SeasonalityDataDir <- file.path(ProjectDataDir, "Seasonality")
PopulationDataDir <- file.path(ProjectDataDir, "Population")
ITNDataDir <- file.path(ProjectDataDir, "ITN")
IRSDataDir <- file.path(ProjectDataDir, "IRS")
CMDataDir <- file.path(ProjectDataDir, "CM")
PfPRDataDir <- file.path(ProjectDataDir, "PfPR")
AllInputDataDir <- file.path(ProjectDataDir, "AllInput")
SMPSDataDir <- file.path(ProjectDataDir, "SMPS")
StrategiesDataDir <- file.path(ProjectDataDir, "Strategies")

### Data descriptions
ProjectFigureDir <- file.path(ProjectDir, "4. Analysis/Figures/DataDescription_and_Preparation")
SeasonalityFigureDir <- file.path(ProjectFigureDir, "Seasonality")
ITNFigureDir <- file.path(ProjectFigureDir, "ITN")
IRSFigureDir <- file.path(ProjectFigureDir, "IRS")
CMFigureDir <- file.path(ProjectFigureDir, "CM")
PfPRFigureDir <- file.path(ProjectFigureDir, "PfPR")
All_InputFigureDir <- file.path(ProjectFigureDir, "All_Input")
SMPSFigureDir <- file.path(ProjectFigureDir, "SMPS")
StrategiesFigureDir <- file.path(ProjectDataDir, "Figure")

### Spatial
SpatialDir <- file.path(DataDir, "12_Spatial")
SpatialDir_NMCP <- file.path(SpatialDir, "NMCP_shp")

Admin1BoundaryDirectory <- file.path(SpatialDir, "KEMRI_files/Tanzania Regions & Districts SHP (220818)") ### using newest shapefiles
Admin2BoundaryDirectory <- Admin1BoundaryDirectory

ExperimentDataDir <- file.path(PaperDataDir, 'TZA_MappingWS_Nov2018_Jan2019', 'NewSimulations', '03032019_OMStrategicPlanning_resimAll_v2_combined/processedExpResults')

### OpenMalaria
# ExperimentsDir <- file.path(OMDir, "Experiments")
# ExperimentsDir_Sub <- file.path(ExperimentsDir, "Hist_and_Fut")

#
# if ("ITERATION" %in% ls()) {
#   if (ITERATION == "ORIGINAL") ExperimentIterationDir <- file.path(ExperimentsDir_Sub, "MalariaExpertMeeting_WS3_Feb2018")
#   if (ITERATION == "NEWSIMULATIONS") ExperimentIterationDir <- file.path(ExperimentsDir_Sub, "TZA_MappingWS_Nov2018_Jan2019/NewSimulations")
#   if (ITERATION == "SENSITIVITYANALYSIS") ExperimentIterationDir <- file.path(ExperimentsDir_Sub, "TZA_MappingWS_Nov2018_Jan2019/SensitivityAnalysis")
#   if (ITERATION == "REFITTEDSIMULATIONS") ExperimentIterationDir <- file.path(ExperimentsDir_Sub, "TZA_MappingWS_Nov2018_Jan2019/RefittedSimulations")
#   if (ITERATION == "STRATEGICPLANNING") ExperimentIterationDir <- file.path(ExperimentsDir_Sub, "ForStrategicPlanning_May2018")
# } else {
#   ExperimentIterationDir <- ExperimentsDir_Sub
# }
#
# ### Experiment spefific directories
# if ("nameExperiment" %in% ls()) {
#   ExperimentDir <- file.path(ExperimentIterationDir, nameExperiment) ## Experiment specific
#   if (ITERATION == "STRATEGICPLANNING") ExperimentDir <- file.path(ExperimentIterationDir, nameExperiment, "/Rerun_Dec2018_resistance")
#   ExperimentDataDir <- file.path(ExperimentDir, "processedExpResults")
#   ExperimentFigureDir <- file.path(ExperimentDir, "Figures")
#   ExperimentOutDatDir <- file.path(ExperimentDir, "OutDat")
#   ExperimentStrategiesDir <- file.path(ExperimentDir, "Strategies")
#   ExperimentMappingDir <- file.path(ExperimentDir, "csv")
#   ExperimentCostDir <- file.path(ExperimentDir, "costs")
#
#   ## if whole analysis is subsetted (e.g. remove LSM intervention), create sub directories for output
#   if (nchar(SUBSETFORCOMPLETEANALYSIS) > 1) {
#     ExperimentFigureDir <- file.path(ExperimentDir, "Figures/", SUBSETFORCOMPLETEANALYSIS)
#     ExperimentOutDatDir <- file.path(ExperimentDir, "OutDat/", SUBSETFORCOMPLETEANALYSIS)
#     ExperimentStrategiesDir <- file.path(ExperimentDir, "Strategies/", SUBSETFORCOMPLETEANALYSIS)
#     ExperimentMappingDir <- file.path(ExperimentDir, "csv/", SUBSETFORCOMPLETEANALYSIS)
#     ExperimentCostDir <- file.path(ExperimentDir, "costs/", SUBSETFORCOMPLETEANALYSIS)
#   }
#
#   dirNotExist <- sapply(mget(ls()[grep("Experiment", ls())][grep("Dir", ls()[grep("Experiment", ls())])], .GlobalEnv), dir.exists)[sapply(mget(ls()[grep("Experiment", ls())][grep("Dir", ls()[grep("Experiment", ls())])], .GlobalEnv), dir.exists) == FALSE]
#   sapply(mget(attributes(dirNotExist)$names, .GlobalEnv), dir.create)
#   rm(dirNotExist)
#
#   # ForStrategicPlanning=FALSE
#   ### TO DO - integrate directories to other directories, automatically select the proper modelling iteration
#   # if(ForStrategicPlanning){
#   #### Directores and data
#   # 	AnalysisDir 				= file.path(ProjectDir,"Analysis/WS4_StrategicPlanning")
#   # 	AnalysisDatDir      		= file.path(AnalysisDir,"dat")
#   # 	ScriptDir 					= file.path(AnalysisDir, "rscripts")
#   # 	OutDir						= file.path(AnalysisDir,"out")
#   # 	FigureDir 					= file.path(AnalysisDir,"Figures")
#   # 	ExperimentDir 				= file.path(ProjectDir,"OM_Iterations/Experiments/Hist_and_Fut/ForStrategicPlanning_May2018/Combined")
#   # 	ExperimentDataDir 			= file.path(ExperimentDir,"OMTZARegions_2020_weight50_extended/resistance")
#   #
#   # }
# }
#

## load other general objects defined
source(file.path(SettingsDir, "../rlibrary/customObjects.R"))
source(file.path(SettingsDir, "../rlibrary/mappingObjects.R"))
source(file.path(FunctionsDir, "f_AggrDat.R"))
source(file.path(FunctionsDir, "f_mergevars.R"))
source(file.path(FunctionsDir, "f_grid_arrange_shared_legend.R"))
source(file.path(FunctionsDir, "f_spread.R"))
source(file.path(FunctionsDir, "f_CostCasesTable.R"))
source(file.path(FunctionsDir, "f_customBoxplot.R"))


### Define not yet saved functions 
smooth_compact <- function(t, L, k) {
  k <- 2.148776
  L <- 6.085687
  y <- ifelse(t < L, exp(k - k / (1 - (t / L)^2)), 0)
  return(y)
}


f_subsetForQGIS <- function(data, selectedStrategy) {
  keepvars <- c("District", strategyLabels[strategyLabels != "FutScen_nr"], selectedStrategy)

  subdat <- merge(data, strategyLabelsDat, by.x = selectedStrategy, by.y = "FutScen_nr")
  subdat <- subdat %>%
    dplyr::select_(.dots = keepvars) %>%
    as.data.frame()
  return(subdat)
}

