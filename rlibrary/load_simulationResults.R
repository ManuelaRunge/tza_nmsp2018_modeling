#-----------------------------------------------------------------------------------
## Function to assemble the database 
## Created by Emilie Pothin and edited by Manuela Runge
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# ARGUMENTS, defined for testing
#-----------------------------------------------------------------------------------
# require(reshape)
# nameExperiment <- "20171007_MOZ_estParam_n35_estFut_local"
# FileName <- paste0(nameExperiment,"_ageDisagg.Rdata")
# FileName_Res_ageAll <- paste0(nameExperiment,"_ageDisagg.Rdata")
# DatabaseName <- "results_disagg_age"
# outcome <- "nPatent"
# dirOut <- paste0(OMpath,"processedExpResults/",nameExperiment,"/model_RdataFiles/")
# param <- c("districtsMOZ","EIR","Importation")
# PARAM=param
# DIROUT <- dirOut
# nHost=10000
# ORIGIN="2000-01-01"  ## origin of monitoring
# SURVEYTIMES ="annual"				   
#-----------------------------------------------------------------------------------


#Clean_Database_function (aggregating seeds already)
##f_cleanDatabase <-function(FileName,DatabaseName,outcome,DIROUT=dirOut, PARAM=param, SURVEYTIMES=surveyTimes){
f_cleanDatabase <- function(FileName, DatabaseName, outcome, DIROUT = dirOut, PARAM = param) {
  ### load file of results
  load(file.path(DIROUT, FileName))

  scenarios = scenarios[, !grepl("dummy", names(scenarios))]
  scenarios = scenarios[, which(names(scenarios) %in% c(PARAM, "seed"))]

  Nvar = ncol(scenarios) - 1
  list_factors = names(scenarios)[1:(Nvar - 1)]  # not selecting "seed"

  list_outcomes = names(get(DatabaseName))

  if (outcome == "nPatent")res = cbind(scenarios, res = get(DatabaseName)[[outcome]] / get(DatabaseName)[["nHost"]])
  else res = cbind(scenarios, res = get(DatabaseName)[[outcome]])

  ### Compute summary statistics
  alllist <- c(); for (i in 1:length(PARAM)) {
    l <- list(res[, which(names(res) == PARAM[i])])
    alllist <- c(alllist, l)
  }
  res_mean = aggregate(subset(res, select = -c(which(names(res) %in% c(PARAM, "seed")))), by = alllist, FUN = mean); colnames(res_mean)[1:length(PARAM)] <- PARAM

  ### Transform the data from wide to list format (vectorised)
  res_vec = melt(res_mean, id = PARAM)

  ### Rename columns
  #res_vec<-rename(res_vec,c(value="res_mean",variable="survey"))
  names(res_vec)[names(res_vec) == "value"] <- "res_mean"
  #	  if(DatabaseName=="results_disagg_age"){
  #  library(tidyr)
  #  res_vec$AgeGrpNr <- as.numeric(substr(as.character(res_vec$variable), nchar(as.character(res_vec$variable)), nchar(as.character(res_vec$variable))))
  #  res_vec$variable <-  substr(as.character(res_vec$variable), 1, nchar(as.character(res_vec$variable))-2)
  #  names(res_vec)[names(res_vec)=="variable"] <- "survey"
  #  }
  # if(DatabaseName!="results_disagg_age") names(res_vec)[names(res_vec)=="variable"] <- "survey"

  names(res_vec)[names(res_vec) == "variable"] <- "survey"

  ### create date variable
  #   if(SURVEYTIMES=="monthly")res_vec$date=as.Date(sapply(res_vec$survey,function(e)as.numeric(strsplit(as.character(e),split="[.]")[[1]][2]))*30, origin=ORIGIN)
  #  if(SURVEYTIMES=="annual")res_vec$date=as.Date(sapply(res_vec$survey,function(e)as.numeric(strsplit(as.character(e),split="[.]")[[1]][2]))*365, origin=ORIGIN)

  res_vec$date = as.Date(sapply(res_vec$survey, function(e)as.numeric(strsplit(as.character(e), split = "[.]")[[1]][2])) * 30, origin = SURVEYSTART)

  for (i in 1:length(PARAM)) {
    res_vec[, i] = (gsub(PARAM[i], "\\1", as.character(res_vec[, i])))
  }

  return(subset(res_vec, select = -survey))
  #return(res_vec)
}


## same function, keeping seeds
##f_cleanDatabaseSeeds <-function(FileName,DatabaseName,outcome,DIROUT=dirOut, PARAM=param, SURVEYTIMES=surveyTimes){
f_cleanDatabaseSeeds <- function(FileName, DatabaseName, outcome, DIROUT = dirOut, PARAM = param) {
  ### load file of results
  load(file.path(DIROUT, FileName))

  scenarios = scenarios[, !grepl("dummy", names(scenarios))]
  scenarios = scenarios[, which(names(scenarios) %in% c(PARAM, "seed"))]

  Nvar = ncol(scenarios) - 1
  list_factors = names(scenarios)[1:(Nvar - 1)]  # not selecting "seed"

  list_outcomes = names(get(DatabaseName))

  if (outcome == "nPatent")res = cbind(scenarios, res = get(DatabaseName)[[outcome]] / get(DatabaseName)[["nHost"]])
  else res = cbind(scenarios, res = get(DatabaseName)[[outcome]])

  ### Compute summary statistics
  alllist <- c(); for (i in 1:length(c(PARAM, "seed"))) {
    l <- list(res[, which(names(res) == c(PARAM, "seed")[i])])
    alllist <- c(alllist, l)
  }
  res_mean = aggregate(subset(res, select = -c(which(names(res) %in% c(PARAM, "seed")))), by = alllist, FUN = mean); colnames(res_mean)[1:length(c(PARAM, "seed"))] <- c(PARAM, "seed")

  ### Transform the data from wide to list format (vectorised)
  res_vec = melt(res_mean, id = c(PARAM, "seed"))

  ### Rename columns
  #res_vec<-rename(res_vec,c(value="res_mean",variable="survey"))
  names(res_vec)[names(res_vec) == "value"] <- "res_mean"
  #   if(DatabaseName=="results_disagg_age"){
  #  library(tidyr)
  #  res_vec$AgeGrpNr <- as.numeric(substr(as.character(res_vec$variable), nchar(as.character(res_vec$variable)), nchar(as.character(res_vec$variable))))
  #  res_vec$variable <-  substr(as.character(res_vec$variable), 1, nchar(as.character(res_vec$variable))-2)
  #  names(res_vec)[names(res_vec)=="variable"] <- "survey"
  #  }
  #  if(DatabaseName!="results_disagg_age") names(res_vec)[names(res_vec)=="variable"] <- "survey"
  names(res_vec)[names(res_vec) == "variable"] <- "survey"

  ### create date variable
  # if(SURVEYTIMES=="monthly")res_vec$date=as.Date(sapply(res_vec$survey,function(e)as.numeric(strsplit(as.character(e),split="[.]")[[1]][2]))*30, origin=ORIGIN)
  #  if(SURVEYTIMES=="annual")res_vec$date=as.Date(sapply(res_vec$survey,function(e)as.numeric(strsplit(as.character(e),split="[.]")[[1]][2]))*365, origin=ORIGIN)
  res_vec$date = as.Date(sapply(res_vec$survey, function(e)as.numeric(strsplit(as.character(e), split = "[.]")[[1]][2])) * 30, origin = SURVEYSTART)

  for (i in 1:length(PARAM)) {
    res_vec[, i] = (gsub(PARAM[i], "\\1", as.character(res_vec[, i])))
  }

  return(subset(res_vec, select = -survey))
  #return(res_vec)
}


