#Best function
 best <- function(state, outcome){
   
 }

##Ajuste de la data. 
data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)
data<- data[, c(2, 7, 11, 17, 23)]
names(data) <- c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "Pneumonia")
stringCol <- data[, c("Hospital.Name", "State")]
numericCol <- as.data.frame(lapply(data[, c(3,4,5)], as.numeric))
data <- cbind(stringCol, numericCol)


#Funcion que separa los datos

#formato a para la tabla de data_attack.
data_attack <- na.omit(data.frame(data$Hospital.Name, data$State, data$"Heart.Attack", stringsAsFactors = FALSE)) #en heart attack se pone el arg outcome
row.names(data_attack) <- 1:nrow(data_attack)
colnames(data_attack) <- c("Hospital.Name", "State", "Heart.Attack")

#Se separa los datos para visualizacion del ranking.
split_data_attack <- split(x = data_attack, f = data_attack$State)
df_state_attack <- split_data_attack[["TX"]] #Aqui se pone el argumento state
row.names(df_state_attack) <- 1:nrow(df_state_attack)
df_state_attack1 <- df_state_attack[order(df_state_attack$"Heart.Attack", df_state_attack$Hospital.Name), ] #en heart attack se pone el arg outcome

result <- df_state_attack1[1,1]

#sapply(split_data_attack, function(x) min(x["TX"]))

#maxValue <- which(data_attack[,3] == max(data_attack[, 3]))
#data_attack[maxValue, "Hospital.Name"]
#min(split_data_attack[["TX"]]["Heart.Attack"])