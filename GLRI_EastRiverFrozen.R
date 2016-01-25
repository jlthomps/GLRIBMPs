load("M:/NonPoint Evaluation/GLRI Edge-of-field/JessicaStuff/GLRI/dataSubEastRiverAll.RData")

# pare down data to only frozen ground events
data_sub <- data_sub[which(data_sub$frozen=="2"),]

# save data_sub with merged data ready for regression
save(data_sub,file="M:/NonPoint Evaluation/GLRI Edge-of-field/JessicaStuff/GLRI/dataSubEastRiverFrozen.RData")


