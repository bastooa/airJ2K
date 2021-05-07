#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      airJ2K      ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Makes J2K simulations from R [Richard*Bonté*Veyssier*Braud*Barreteau]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# General settings for R
rm(list=ls()); startTime = Sys.time();
wd = '~/airJ2K'
source('airJ2K_settings.R')

# Simulation Settings
caseName = 'buech'
dateStart = as.Date('1970-01-01', '%Y-%m-%d') # Comment: Attention la date de début de simulation doit être la même que dans le fichier modèle .jam (à modifier "à la main" dans le .jam)
dateEnd = as.Date('2018-07-31', '%Y-%m-%d') # Comment: Choisir une date de fin inférieure ou égale à celle dans le .jam (ou modifier le .jam)
jamsFileName = 'cowat_for_new_com_module-bigHrus.jam' # Comment: Renseigner le nom du fichier modèle .jam qui est à placer dans le répertoir ~/watasitrcoupler/superjams/data/.../
saveOutputs = T # Comment: Si True les sorties seront enregistrées dans ~airJ2K/superjams/data/.../output

# Initializes J2K model
killJ2K() # Comment: Kill JAMS if it's running
initJ2K(jamsRootPath, jamsFileName, stdoutP, stderrP, wd) # Comment: Lance le fichier modèle dans JAMS

# Runs j2k on the simulation period 
print('Ongoing J2K simulation...')
simuProgress <- txtProgressBar(min = 1, max =as.numeric(difftime(dateEnd,dateStart,units='days')), style = 3) # Barre de progression
inOutWater = NULL; runoffSelectedReaches = NULL # Comment: Variable de stockage des sorties
for (i in 1:as.numeric(difftime(dateEnd,dateStart,units='days'))){ # Comment: Boucle temporelle
setTxtProgressBar(simuProgress, i)
  j2kMakeStep() # Comment: Un seul pas de temps
  inOutWater <- rbind(inOutWater, j2kWaterBalanceFlows()) # Comment: Enregistrement du CatchmentRunoff et somme des precipitations, ETR et T°C sur l'ensemble des HRUs
  # runoffSelectedReaches <- rbind(runoffSelectedReaches, j2kRunoffSelectedReaches(selectedIDs = c(55000,57800,61000,62200,78200,79400))) # Comment: Enregistrement de un ou plusieurs reachs en particulier
}
cat('\n','Glimpse to output file:','\n')
glimpse(inOutWater) # Comment: Apercu des sorties

# Save outputs
if (saveOutputs) {
  write.csv(inOutWater, paste0(file.path(script.dirname, 'output/'),caseName,'_inOutWater.csv'), row.names = F)
  # write.csv(runoffSelectedReaches, paste0(file.path(script.dirname, 'output/'),caseName,'_runoffSelectedReaches.csv'), row.names = F)
}

# Ending airJ2K
j2kStop(); killJ2K()
endTime = Sys.time(); simuTime = endTime - startTime; cat ('................................................................',
                                                                '\n','Simulation time is ', round(simuTime,2), 'minutes', '\n')
