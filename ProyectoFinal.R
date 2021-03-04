##Final Project

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") #se lee colclasses como character para que los id con leading zero no elimine los ceros por delante

outcome[, 11] <- as.numeric(outcome[, 11]) #convierte de caracter a numerico.
hist(outcome[, 11])
