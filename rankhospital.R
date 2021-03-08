##Funcion rankinghospital

#Ajuste de formato de la data.
#Se utilizar para extraer la data, tomar un subconjuntos de esos datos con las columnas deseadas y su respectivas clases de datos.
data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)[, c(2, 7, 11, 17, 23)] 
names(data) <- c("Hospital.Name", "State", "heart.attack", "heart.failure", "pneumonia")
stringCol <- data[, c("Hospital.Name", "State")]
numericCol <- as.data.frame(lapply(data[, c(3,4,5)], as.numeric))
data <- cbind(stringCol, numericCol)

#Esta funcion se utilizar para presentar el hospital segun el rango que se ingrese, respecto a su estado y su outcome (mortality rate)
rankhospital <- function(state, outcome, num){
  
  #Tenemos 3 condiciones:
  #1. Si el estado ingresado se encuentra en la data.
  #2. Si el outcome ingresado se encuentra en la data.
  #3. Si los argumentos cumplen con las condiciones anteriores.
  #nota: en el argumento paste0 (^ y $) se utiliza para comparar con exactitud en la funcion de grepl
  
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
    
    #Las siguientes 3 condiciones sirven para filtrar el rango que se desea.
    #1. El hospital con el peor mortality rate.
    #2. El hospital con el mejor mortality rate.
    #3. El hospital con el rank especifico que se ingreso
    
    if(num == "worst"){
      result <- df_state_outcome[nrow(df_state_outcome), "Hospital.Name"]
    }
    
    else if(num == "best"){
      result <- df_state_outcome[1, "Hospital.Name"]
    }
    
    else if(class(num) == "numeric"){
      result <- df_state_outcome[num, "Hospital.Name"]
    }
    
  }
  
  #Se retorna el con el hospital segun el rango ingresado en la funcion
  return(result)
  
}