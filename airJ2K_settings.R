#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      airJ2K settings     ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sets airJ2K [Richard*Bonté*Veyssier]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set directory (Not necessary if you open from .Rproject)
wd <- getwd()
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.dirname <- dirname(script.name)
if (paste0(script.dirname, "runInConsole") == "runInConsole")
  script.dirname <- wd

# Load functions
wd_Functions <- file.path(script.dirname, "Rfunctions/")
for(FileName in list.files(wd_Functions, pattern="\\.[Rr]$")){ source(file.path(wd_Functions, FileName)); }

# Load libraries 
load <- c(require(ConfigParser), require(R.utils), require(RSQLite), require(feather), require(zoo), require (multiplex), require(tidyr), require(dplyr), require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

# Core parallelism
cores <- parallel:::detectCores(); registerDoParallel(cores-2);

# Read config file
args = commandArgs(trailingOnly=TRUE)
DEBUG = FALSE
configFilePath = "./rcoupler.cfg"
for (arg in args) {
  if (arg == '-d') {
    DEBUG = TRUE
  } else {
    configFilePath = arg
  }
}
configFileName = basename(configFilePath)
configFileDir = dirname(configFilePath)
stderrP = FALSE
stdoutP = FALSE
if (DEBUG) {
  stderrP = ""
  stdoutP = ""
}
config = ConfigParser$new(NULL)
config$read(configFilePath)

jamsRootPath = config$get("jamsRoot", NA, "tools")
if (!isAbsolutePath(jamsRootPath)) {
  jamsRootPath = paste(configFileDir, jamsRootPath, sep="/")
}
jamsStarterPath = paste(jamsRootPath, "jams-starter.jar", sep="/")

requiredFiles = c(jamsStarterPath)
for (path in requiredFiles) {
  if (!file.exists(path)) {
    cat(paste("File ", path, " not found.\n", sep=""))
    quit(status=1)
  }
}