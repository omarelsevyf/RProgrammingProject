best <- function(state, outcome){
  
  ##Ajuste de la data. 
  data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)
  data<- data[, c(2, 7, 11, 17, 23)]
  names(data) <- c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "pneumonia")
  stringCol <- data[, c("Hospital.Name", "State")]
  numericCol <- as.data.frame(lapply(data[, c(3,4,5)], as.numeric))
  data <- cbind(stringCol, numericCol)
  
  ##Condiciones
  
  #Formato a para la tabla de outcome.
  columns <- c("Hospital.Name", "State", outcome)
  data_outcome <- na.omit(data.frame(data[, columns])) #en heart attack se pone el arg outcome
  row.names(data_outcome) <- 1:nrow(data_outcome)
  colnames(data_outcome) <- c("Hospital.Name", "State", outcome)
  
  #Se separa los datos para visualizacion del ranking.
  split_data_outcome <- split(x = data_outcome, f = data_outcome$State)
  df_state_outcome <- split_data_outcome[[state]] #Aqui se pone el argumento state
  row.names(df_state_outcome) <- 1:nrow(df_state_outcome)
  
  df_state_attack1 <- df_state_outcome[order(df_state_outcome[, 3], df_state_outcome[, 1]), ] #en heart attack se pone el arg outcome
  
  result <- df_state_attack1[1,1]
  return(result)
  
}
















