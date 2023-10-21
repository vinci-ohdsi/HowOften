install.packages("renv")
renv::init()
install.packages("remotes")

remotes::install_github("OHDSI/Strategus")

Sys.setenv(DATABASECONNECTOR_JAR_FOLDER="C:/Users/msuch/Documents/HowOften/jars")
DatabaseConnector::downloadJdbcDrivers(dbms = "postgresql")

library(Strategus)

##=========== START OF INPUTS ==========
connectionDetailsReference <- "BUild"
workDatabaseSchema <- "temp"
cdmDatabaseSchema <- "cdm"
outputLocation <- "C:/Users/msuch/Documents/HowOften/Strategus"
resultsLocation <- "C:/Users/msuch/Documents/HowOften/Results"
minCellCount <- 10
cohortTableName <- "howoften_cohort"

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = keyring::key_get("buildServer"),
  user = keyring::key_get("buildUser"),
  password = keyring::key_get("buildPassword")
)

##=========== END OF INPUTS ==========

executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = connectionDetailsReference,
  workDatabaseSchema = workDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = cohortTableName),
  workFolder = file.path(outputLocation, connectionDetailsReference, "strategusWork"),
  resultsFolder = file.path(outputLocation, connectionDetailsReference, "strategusOutput"),
  minCellCount = minCellCount
)

executeAnalysis <- function(analysisFile, executionSettings, analysisName, outputLocation, resultsLocation) {

  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = analysisFile
  )

  Strategus::execute(
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings,
    executionScriptFolder = file.path(outputLocation, connectionDetailsReference, "strategusExecution")
  
  )

  # copy Results to final location
  resultsDir <- file.path(resultsLocation, analysisName, connectionDetailsReference)

  if (dir.exists(resultsDir)) {
    unlink(resultsDir, recursive = TRUE)
  }
  dir.create(file.path(resultsDir), recursive = TRUE)
  file.copy(file.path(outputLocation, connectionDetailsReference, "strategusOutput"),
            file.path(resultsDir), recursive = TRUE)

  return(NULL)

}

# Step 0: Execute Azza-blank Analysis to instantitate modules
executeAnalysis("howoften_azza_blank.json", executionSettings, "azza", outputLocation, resultsLocation)

# Step 1 : Execute Azza Analysis
executeAnalysis("howoften_azza.json", executionSettings, "azza", outputLocation, resultsLocation, keyringName)

# Step 2 : Execute Andreas Analysis
executeAnalysis("howoften_andreas.json", executionSettings, "andreas", outputLocation, resultsLocation, keyringName)

# Step 3, Joel Analysis
executeAnalysis("howoften_joel.json", executionSettings, "joel", outputLocation, resultsLocation, keyringName)

# step 4, Evan analysis
executeAnalysis("howoften_evan.json", executionSettings, "evan", outputLocation, resultsLocation, keyringName)

# step 5, gowza analysis
executeAnalysis("howoften_gowza.json", executionSettings, "gowza", outputLocation, resultsLocation, keyringName)

# step 6, overall analysis
executeAnalysis("howoften_overall.json", executionSettings, "overall", outputLocation, resultsLocation, keyringName)

# step 7, george analysis
executeAnalysis("howoften_george.json", executionSettings, "george", outputLocation, resultsLocation, keyringName)



