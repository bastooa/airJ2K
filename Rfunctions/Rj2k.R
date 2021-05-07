library(httr)
library(xml2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
library(RJSONIO)

genDictElement <- function(name, value) {
    strValue = as.character(value)
    if (startsWith(strValue, '[') || startsWith(strValue, '{')) {
        return(paste0('"', name, '": ', value))
    }
    else {
        return(paste0('"', name, '": "', value, '"'))
    }
}

genTabElement <- function(value) {
  strValue = as.character(value)
  if (startsWith(strValue, '[') || startsWith(strValue, '{')) {
    return(paste0('"', value))
  }
  else {
    return(paste0('"', value, '"'))
  }
}

genJsonDict <- function(names, values) {
    result = "{\n"
    args <- mapply(genDictElement, names, values)
    result <- paste0(result, paste0(c(args), collapse=",\n"))
    result <- paste0(result, "\n}")
    return(result)
}

genJsonTbl <- function(values) {
  result = "[\n"
  args <- mapply(genTabElement, values)
  result <- paste0(result, paste0(c(args), collapse=",\n"))
  result <- paste0(result, "\n]")
  return(result)
}

j2kDictListToDataframe <- function(str) {
    js = fromJSON(str)
    js = lapply(js, function(x) {
        x[sapply(x, is.null)] <- NA
        unlist(x)
    })
    df = do.call("rbind", js) %>%
      as.data.frame(stringsAsFactors = F) %>% 
      mutate_all(as.numeric)
    return(df);
}

j2kDictToDataframe <- function(str) {
  js = fromJSON(str)
  return(t(js) %>% as.data.frame())
}

askJ2K <- function(argNames = c(), argValues = c(), ip, port) {
    payload <- genJsonDict(argNames, argValues)
    result <- POST(paste0("http://", ip, ":", port), body = payload)
    return(list(payload, content(result, encoding='UTF-8')))
}

##################### Here are the end user functions #######################

# tell j2k to make N steps
j2kMakeStep <- function(nbStep=1, ip="localhost", port="9999") {
    res = askJ2K(c("command", "nbStep"), c("step", nbStep), ip, port)
    res = paste("[J2K]", res[[2]])
    return(res)
}

# free j2k model so it runs until the end
j2kFree <- function(ip="localhost", port="9999") {
    res = askJ2K(c("command"), c("free"), ip, port)
    res = paste("[J2K]", res[[2]])
    return(res)
}

# free j2k model so it runs until the end
j2kStop <- function(ip="localhost", port="9999") {
    res = askJ2K(c("command"), c("stop"), ip, port)
    res = paste("[J2K]", res[[2]])
    return(res)
}

# set values for all hrus or all reach
# what parameter can be infiltration, aspersion, drip, surface (keys are HRU ids)
# and also reachin, reachout (keys are reach ids)
# !! all values need to be in litres !!
j2kSet <- function(what, keys, values, ip="localhost", port="9999") {
    if (length(keys) == 0) return("no value to set")
    dict = genJsonDict(keys, values)
    res = askJ2K(c("command", "key", "value"), c("set", what, dict), ip, port)
    res = paste("[J2K]", res[[2]])
    return(res)
}

# get values for all hrus or all reachs
# what can be "hru" or "reach"
#TODO: Attention chez moi cette fonction ne renvoie qu'une valeur
# (la colonne s'appelle actRD1 pour les reach par exemple)
# Maintenant qu'on a des fonctions pour avoir les valeurs une par une
# je propose que cette fonction renvoie toutes les variables pour hru ou reach
# selon ce que l'on choisit.

j2kGet <- function(what, ip="localhost", port="9999") {
    res = askJ2K(c("command", "key"), c("get", what), ip, port)
    df = j2kDictListToDataframe(res[[2]])
    return(df)
}

# get attributes for all hrus
# what can be "netrain", "etpot"...

j2kGetValuesAllHrus <- function(attributes, ids = NULL, ip="localhost", port="9999") {
    res = askJ2K(c("command", "keys", "ids"), c("getHru", genJsonTbl(attributes), genJsonTbl(ids)), ip, port)
    if (startsWith(res[[2]], '[')) {
        df = j2kDictListToDataframe(res[[2]])
        return(df)
    }
    else {
        cat(res[[2]], '\n')
        return(NULL)
    }
}

# For backwards compatibility..
j2kGetOneValueAllHrus <- function(attribute, ids = NULL, ip="localhost", port="9999") {
  return(j2kGetValuesAllHrus(attributes = attribute, ids, ip="localhost", port="9999"))
}

# Get values of attributes sumed on all HRUS
j2kSumedValuesAllHrus <- function(attributes, ip="localhost", port="9999") {
  res = askJ2K(c("command", "keys"), c("getHruSum", genJsonTbl(attributes)), ip, port)
  if (startsWith(res[[2]], '{')) {
    df = j2kDictToDataframe(res[[2]])
    return(df)
  } else {
    cat(res[[2]], '\n')
    return(NULL)
  }
}

# get one attribute for all reachs
# what can be "actRD1", "Runoff"...
j2kGetValuesAllReachs <- function(attributes, ids = NULL, ip="localhost", port="9999") {
  res = askJ2K(c("command", "keys", "ids"), c("getReach", genJsonTbl(attributes), genJsonTbl(ids)), ip, port)
  if (startsWith(res[[2]], '[')) {
    df = j2kDictListToDataframe(res[[2]])
    return(df)
  } else {
    cat(res[[2]], '\n')
    return(NULL)
  }
}

# For backwards compatibility..
j2kGetOneValueAllReachs <- function(attribute, ids = NULL, ip="localhost", port="9999") {
  return(j2kGetValuesAllReachs(attributes = attribute, ids, ip="localhost", port="9999"))
}

# Get values of attributes sumed on all HRUS
j2kSumedValuesAllReachs <- function(attributes, ip="localhost", port="9999") {
  res = askJ2K(c("command", "keys"), c("getReachSum", genJsonTbl(attributes)), ip, port)
  if (startsWith(res[[2]], '{')) {
    df = j2kDictToDataframe(res[[2]])
    return(df)
  } else {
    cat(res[[2]], '\n')
    return(NULL)
  }
}

# get aggregated values for water balance
j2kWaterBalanceStorages <- function(ip="localhost", port="9999") {
  #hruStorageBis <- sum(j2kGetValuesAllHrus(c("actMPS", "actLPS", "actDPS","TotSWE", "actRG1", "actRG2", "storedInterceptedWater")) %>%
   #                       select(-ID))
  hruStorage <- sum(j2kSumedValuesAllHrus(c("actMPS","actLPS","actDPS","TotSWE","actRG1","actRG2","storedInterceptedWater"))) #%>%
    #t() %>% 
    #as.data.frame() %>%
    #rename_all(funs(paste0(.,"bis")))
  #reachStorageBis <- sum(j2kGetValuesAllReachs(c("actRD1", "actRD2", "actRG1",  "actRG2"))%>%
  #                      select(-ID))
  reachStorage <- sum(j2kSumedValuesAllReachs(c("actRD1","actRD2","actRG1","actRG2"))) #%>% 
    #t() %>% 
    #as.data.frame() %>%
    #rename_all(funs(paste0(.,"bis")))
  return(data.frame(hruStorage, reachStorage
                    #,hruStorageBis, reachStorageBis
                    ))
}

# get selected HRU values for water storages
j2kWaterBalanceStoragesSelectedHRUs <- function(ip="localhost", port="9999", selectedIDs) {
  # storage <- j2kGetValuesAllHrus(c("actMPS","satMPS","actLPS","satLPS","actDPS","TotSWE","actRG1","satRG1","actRG2","satRG2","storedInterceptedWater"))
  # selectedHrusStorage <- data.frame(storage[storage$ID %in% selectedIDs,])
  storage <- j2kGetValuesAllHrus(c("actMPS","satMPS","actLPS","satLPS","actDPS","TotSWE","actRG1","satRG1","actRG2","satRG2","storedInterceptedWater"), ids=selectedIDs)
   return(data.frame(hruCount = length(storage$ID),
                     actMPS = sum(storage$actMPS),
                     satMPS = sum(storage$satMPS),
                     actLPS = sum(storage$actLPS),
                     satLPS = sum(storage$satLPS),
                     actDPS = sum(storage$actDPS),
                     TotSWE = sum(storage$TotSWE),
                     actRG1 = sum(storage$actRG1),
                     satRG1 = sum(storage$satRG1),
                     actRG2 = sum(storage$actRG2),
                     satRG2 = sum(storage$satRG2),
                     storedInterceptedWater = sum(storage$storedInterceptedWater)
                     ))
}

# get selected HRU values for water flux
j2kWaterBalanceFlowsSelectedHRUs <- function(ip="localhost", port="9999", selectedIDs) {
  # flows <- j2kGetValuesAllHrus(c("RD1", "RD2", "RG1", "RG2", "RD1OUT", "RD2OUT", "RG1OUT", "RG2OUT","rain","snow","etact"))
  flows <- j2kGetValuesAllHrus(c("RD1", "RD2", "RG1", "RG2", "RD1OUT", "RD2OUT", "RG1OUT", "RG2OUT","rain","snow","etact"), ids=selectedIDs)
  # selectedHrusFlows <- data.frame(flows[flows$ID %in% selectedIDs,])
  return(data.frame(hruCount = length(flows$ID), 
                    RD1 = sum(flows$RD1), 
                    RD2 = sum(flows$RD2),
                    RG1 = sum(flows$RG1), 
                    RG2 = sum(flows$RG2), 
                    RD1OUT = sum(flows$RD1OUT),
                    RD2OUT = sum(flows$RD2OUT),
                    RG1OUT = sum(flows$RG1OUT),
                    RG2OUT = sum(flows$RG2OUT),
                    rain = sum(flows$rain),
                    snow = sum(flows$snow),
                    etact = sum(flows$etact)
  )) 
}

j2kWaterBalanceFlows <- function(ip="localhost", port="9999") {
  res = askJ2K(c("command"), c("getCatchmentRunoff"), ip, port)
  runoff <- as.numeric(res[[2]])
  #runoffBis <- j2kGetOneValueAllReachs("Runoff", c(52001)) %>% select("Runoff") %>% pull()
 # runoffBis <- j2kGetOneValueAllReachs("Runoff") %>% filter(ID == 25401) %>% pull()
  #if (is.na(runoff)) {
  #  runoff <- res[[2]]
  #}
  hrusInOut <- j2kSumedValuesAllHrus(c("rain","snow","etact"))
  return(data.frame(runoff, 
   #                 runoffBis, 
                    hrusInOut))
}

# get selected Reachs values for water flux
j2kWaterBalanceFlowsSelectedReachs <- function(ip="localhost", port="9999", selectedIDs) {
  runoffComponents <- j2kGetValuesAllReachs(c("inRD1", "inRD2", "inRG1","inRG2", "ouRD1", "outRD2", "ouRG1", "ouRG2"), ids=selectedIDs)
  # runoffComponents <- data.frame(runoffComponents[runoffComponents$ID %in% selectedIDs,])
  return(data.frame(reachCount = length(runoffComponents$ID),
                    inRD1 = sum(runoffComponents$inRD1),
                    inRD2 = sum(runoffComponents$inRD2),
                    inRG1 = sum(runoffComponents$inRG1),
                    inRG2 = sum(runoffComponents$inRG2),
                    outRD1 = sum(runoffComponents$outRD1),
                    outRD2 = sum(runoffComponents$outRD2),
                    outRG1 = sum(runoffComponents$outRG1),
                    outRG2 = sum(runoffComponents$outRG2),
  ))
}

# get Runoff for all reaches
j2kRunoffAllReachs <- function(ip="localhost", port="9999") {
  runoff <- t(j2kGetValuesAllReachs(c("Runoff")))
  runoff.df <- data.frame(runoff); colnames(runoff.df) <- runoff[1,]
  runoff.df <- runoff.df[-1,]
  return(runoff.df)
}

# get Runoff for selected reaches
j2kRunoffSelectedReaches <- function(ip="localhost", port="9999", selectedIDs) {
  runoff <- t(j2kGetValuesAllReachs(c("Runoff"), ids=selectedIDs))
  runoff.df <- data.frame(runoff); colnames(runoff.df) <- runoff[1,]
  runoff.df <- runoff.df[-1,]
  return(runoff.df)
}

############## UTILS ###############

killJ2K <- function() {
    system2('kill', args=c('-9', "$(ps aux | grep -i 'jams-starter' | grep java | awk '{print $2}')"), wait=F)
}


initJ2K <- function(jamsRootPath, jams_file_name, stdoutP, stderrP, wd) {
setwd(jamsRootPath)
system2(
  'java',
  args=c('-jar', 'jams-starter.jar', '-m', paste0('data/J2K_cowat/',jams_file_name), '-n'),
  wait=F, stdout=stdoutP, stderr=stderrP
)
cat('\n', 'Waiting 5 seconds to make sure J2K coupling module starts listening...','\n')
Sys.sleep(5)
setwd(wd)
}