load("M:/NonPoint Evaluation/GLRI Edge-of-field/JessicaStuff/GLRI/dataSubEastRiverAll.RData")

# cutting out storms with frozen ground or estimated QW numbers
data_sub <- data_sub[which(data_sub$frozen=="1"),]

# save data_sub with merged data ready for regression
save(data_sub,file="M:/NonPoint Evaluation/GLRI Edge-of-field/JessicaStuff/GLRI/dataSubEastRiverSplit.RData")


