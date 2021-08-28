# Supplementary r-script to be included by others, to load key information from base xml 

## load package
library(xml2)
library(XML)

#ExperimentDir is one OpenMalaria Experiment directory
##ExperimentDir =""

#############  BASE  --- GET nHost and Origin
if (file.exists((paste0(ExperimentDir, "base.xml"))))base <- read_xml((paste0(ExperimentDir, "base.xml")))
if (file.exists((paste0(ExperimentDir, "description/base.xml"))))base <- read_xml((paste0(ExperimentDir, "description/base.xml")))
if (file.exists((paste0(ExperimentDir, "Analysis/description/base.xml"))))base <- read_xml((paste0(ExperimentDir, "Analysis/description/base.xml")))

demography <- xml_find_all(base, ".//demography")
ageGroup <- xml_find_all(base, ".//ageGroup")


## get nHost
nHost <- strsplit(as.character(demography), " ")[[1]][4]
nHost <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", nHost)
nHost <- as.numeric(gsub("popSize", "", nHost))

## get AgeGroups  (needs to be modified depending on the number of age groups)
cat(strsplit(as.character(ageGroup), " ")[[2]])
ageCutOffs <- strsplit(as.character(ageGroup), "[^[:digit:]]")[[2]]
## convert strings to numeric ("" become NA)
ageCutOffs <- as.numeric(unlist(ageCutOffs))
## remove NA and duplicates
ageCutOffs <- unique(ageCutOffs[!is.na(ageCutOffs)])
nAgeGroups <- as.matrix(c(1:(length(ageCutOffs) - 1)))

### This part needs to be modified to be flexible depending on the number of age groups
AgeGroupsLabel = as.matrix(c(paste0(ageCutOffs[1], "-", ageCutOffs[2]),
                             paste0(ageCutOffs[2], "-", ageCutOffs[3]),
                             paste0(ageCutOffs[3], "-", ageCutOffs[4]),
                             paste0(ageCutOffs[4], "-", ageCutOffs[5]),
                             paste0(ageCutOffs[5], "-", ageCutOffs[6])))
AgeGroupsDat <- as.data.frame(cbind(nAgeGroups, AgeGroupsLabel))
colnames(AgeGroupsDat) <- c("nAgeGroups", "AgeGroupsLabel")

## get timesteps and number of surveys
surveysXML <- xml_find_all(base, ".//surveys")
timesteps <- xml_integer(xml_children(surveysXML)) # timesteps
nsurveys <- length(timesteps)

### Timing
SIMSTART <- "1957-01-01"  ## <---- fixed number, put in manually!! -> include timestep1 with date in base, to be extracted!!!
##ORIGIN="2000-01-01"			   
timedates <- as.Date(timesteps * 5, origin = SIMSTART)
SURVEYSTART <- min(timedates)
TimeDat <- cbind(as.data.frame(timesteps), as.data.frame(timedates))
TimeDat$monitoringStep <- rownames(TimeDat)
colnames(TimeDat) <- c("Timestep", "Date", "monitoringStep")
TimeDat$year <- as.numeric(format(TimeDat$Date, '%Y'))

colnames(TimeDat)[colnames(TimeDat) == "monitoringStep"] <- "survey"
TimeDat$survey <- as.numeric(TimeDat$survey)


### Display extracted values
cat("Loaded objects: \n")
cat("'SIMSTART' - start simulation (default):", SIMSTART, "\n")
cat("'SURVEYSTART' - start monitoring:", as.character(SURVEYSTART), "\n")
cat("'nHost' - total population size:", nHost, "\n")
cat("Number of timesteps", length(timedates), "\n")
cat("Timedata saved in 'TimeDat'", "\n")
cat("AgeGroups saved in 'AgeGroupsDat' (n = ", length(nAgeGroups), ")", "\n")