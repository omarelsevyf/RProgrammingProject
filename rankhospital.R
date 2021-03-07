##Funcion rankinghospital

#Ajuste de formato de la data.
#Se utilizar para extraer la data, tomar un subconjuntos de esos datos con las columnas deseadas y su respectivas clases de datos.
data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)[, c(2, 7, 11, 17, 23)] 
names(data) <- c("Hospital.Name", "State", "heart.attack", "heart.failure", "pneumonia")
stringCol <- data[, c("Hospital.Name", "State")]
numericCol <- as.data.frame(lapply(data[, c(3,4,5)], as.numeric))
data <- cbind(stringCol, numericCol)


rankinghospital <- function(state, outcome, num){
  
  if (!any(grepl(paste0("^", state, "$"), data[, "State"]))){
    stop("invalid state")
  }
  
  else if (!any(grepl(paste0("^", outcome, "$"), colnames(data)))){
    stop("invalid outcome")
  }
  
  else{
    
    columns <- c("Hospital.Name", "State", outcome)
    data_outcome <- na.omit(data.frame(data[, columns]))
    
    split_data_outcome <- split(x = data_outcome, f = data_outcome$State)
    df_state_outcome <- split_data_outcome[[state]]
    df_state_outcome <- df_state_outcome[order(df_state_outcome[, 3], df_state_outcome[, 1]), ]
    
    
    
  }
  
}