##Funcion best.

#Ajuste de formato de la data.
#Se utilizar para extraer la data, tomar un subconjuntos de esos datos con las columnas deseadas y su respectivas clases de datos.
data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)[, c(2, 7, 11, 17, 23)] 
names(data) <- c("Hospital.Name", "State", "heart.attack", "heart.failure", "pneumonia")
stringCol <- data[, c("Hospital.Name", "State")]
numericCol <- as.data.frame(lapply(data[, c(3,4,5)], as.numeric))
data <- cbind(stringCol, numericCol)

#Esta funcion es utilizada para presentar el mejor hospital con la mejor calidad de servicios, tomando como referencia
#sus mortality rates por heart attack, heart failure y pneumonia.
best <- function(state, outcome){  
  
  #Condiciones
  
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
    
    #Formato a para la tabla de outcome.
    
    #Tomamos los datos de nuestra tabla ajustada y tomamos un subconjunto de esta dependiendo del outcome que se ingrese
    #lo guardamos en un dataframe para conservar sus clases y eliminamos las celdas con NA.
    columns <- c("Hospital.Name", "State", outcome)
    data_outcome <- na.omit(data.frame(data[, columns]))
    
    #Luego separamos nuestros datos en una lista respecto a los estados, tomamos un subconjunto de la lista (tomando en cuenta
    #que sus subconjuntos son dataframes) que contenga el estado ingresado en la funcion y ordenamos estos datos de menor a mayor 
    #primero por el valor del outcome y luego por orden alfabetico del hospital.
    split_data_outcome <- split(x = data_outcome, f = data_outcome$State)
    df_state_outcome <- split_data_outcome[[state]]
    df_state_outcome <- df_state_outcome[order(df_state_outcome[, 3], df_state_outcome[, 1]), ]
    
    #El primer valor del dataframe sera el hospital con el rate de mortalidad segun el outcome mas bajo (mejor calidad)
    result <- df_state_outcome[1,1]
    
  }
  
  return(result)
  
}