library(R6, lib.loc = 'C:/Users/msuch/Documents/HowOften/renv/library/R-4.2/x86_64-w64-mingw32/R6/..')
library(dplyr, lib.loc = 'C:/Users/msuch/Documents/HowOften/renv/library/R-4.2/x86_64-w64-mingw32/dplyr/..')
library(openssl, lib.loc = 'C:/Users/msuch/Documents/HowOften/renv/library/R-4.2/x86_64-w64-mingw32/openssl/..')
library(keyring, lib.loc = 'C:/Users/msuch/Documents/HowOften/renv/library/R-4.2/x86_64-w64-mingw32/keyring/..')
library(DatabaseConnector, lib.loc = 'C:/Users/msuch/Documents/HowOften/renv/library/R-4.2/x86_64-w64-mingw32/DatabaseConnector/..')
library(CohortGenerator, lib.loc = 'C:/Users/msuch/Documents/HowOften/renv/library/R-4.2/x86_64-w64-mingw32/CohortGenerator/..')
library(ParallelLogger, lib.loc = 'C:/Users/msuch/Documents/HowOften/renv/library/R-4.2/x86_64-w64-mingw32/ParallelLogger/..')
library(Strategus, lib.loc = 'C:/Users/msuch/Documents/HowOften/renv/library/R-4.2/x86_64-w64-mingw32/Strategus/..')
options(renv.consent = TRUE)
source("Main.R")
jobContext <- readRDS("C:/Users/msuch/Documents/HowOften/Strategus/BUild/strategusWork/CohortIncidenceModule_2/jobContext.rds")
unlockKeyring <- function(keyringName) {
    keyringLocked <- keyring::keyring_is_locked(keyring = keyringName)
    if (keyringLocked) {
        keyring::keyring_unlock(keyring = keyringName, password = Sys.getenv("STRATEGUS_KEYRING_PASSWORD"))
    }
    return(keyringLocked)
}
keyringName <- jobContext$keyringSettings$keyringName
keyringLocked <- unlockKeyring(keyringName = keyringName)
ParallelLogger::addDefaultFileLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, "log.txt"))
ParallelLogger::addDefaultErrorReportLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, "errorReport.R"))
options(andromedaTempFolder = file.path(jobContext$moduleExecutionSettings$workFolder, "andromedaTemp"))
options(tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema)
options(databaseConnectorIntegerAsNumeric = jobContext$moduleExecutionSettings$integerAsNumeric)
options(databaseConnectorInteger64AsNumeric = jobContext$moduleExecutionSettings$integer64AsNumeric)
if (Sys.getenv("FORCE_RENV_USE", "") == "TRUE") {
    renv::use(lockfile = "renv.lock")
}
if (TRUE) {
    connectionDetails <- Strategus::retrieveConnectionDetails(connectionDetailsReference = jobContext$moduleExecutionSettings$connectionDetailsReference, keyringName = keyringName)
    jobContext$moduleExecutionSettings$connectionDetails <- connectionDetails
} else {
    resultsConnectionDetails <- Strategus::retrieveConnectionDetails(connectionDetailsReference = jobContext$moduleExecutionSettings$resultsConnectionDetailsReference, keyringName = keyringName)
    jobContext$moduleExecutionSettings$resultsConnectionDetails <- resultsConnectionDetails
}
if (keyringLocked) {
    keyring::keyring_lock(keyring = keyringName)
}
execute(jobContext)
ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE)
ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE)
writeLines("done", "C:/Users/msuch/Documents/HowOften/Strategus/BUild/strategusOutput/CohortIncidenceModule_2/done")
