##Testing

data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)
data1 <- data[, c(2, 7, 11, 17, 23)]
names(data1) <- c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "Pneumonia")

stringCol <- data1[, c("Hospital.Name", "State")]
numericCol <- as.data.frame(lapply(data1[, c(3,4,5)], as.numeric))

data2 <- cbind(stringCol, numericCol)

data_attack <- na.omit(data2[, c(1, 2, 3)])
row.names(data_attack) <- 1:nrow(data_attack)

split_data_attack <- split(x = data_attack, data_attack$State)

df_state_attack <- split_data_attack[["TX"]] #Aqui se pone el argumento state
row.names(df_state_attack) <- 1:nrow(df_state_attack)

df_state_attack1 <- df_state_attack[order(df_state_attack$Heart.Attack, df_state_attack$Hospital.Name), ]

result <- df_state_attack1[1,1]

#sapply(split_data_attack, function(x) min(x["TX"]))

#maxValue <- which(data_attack[,3] == max(data_attack[, 3]))
#data_attack[maxValue, "Hospital.Name"]
#min(split_data_attack[["TX"]]["Heart.Attack"])