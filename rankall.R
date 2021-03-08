##Funcion rankinghospital

#Ajuste de formato de la data.
#Se utilizar para extraer la data, tomar un subconjuntos de esos datos con las columnas deseadas y su respectivas clases de datos.
data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)[, c(2, 7, 11, 17, 23)] 
names(data) <- c("Hospital.Name", "State", "heart.attack", "heart.failure", "pneumonia")
stringCol <- data[, c("Hospital.Name", "State")]
numericCol <- as.data.frame(lapply(data[, c(3,4,5)], as.numeric))
data <- cbind(stringCol, numericCol)

#Esta funcion se utiliza para determinar que hospital se encuentra en el rango (num) ingresado 
rankall <- function(outcome, num = "best"){
  
  #Esta condicion se utiliza en caso de que el argumento sea invalido
  if (!any(grepl(paste0("^", outcome, "$"), colnames(data)))){
    stop("invalid outcome")
  }
  
  else{
    #Creamos un data frame sin NA con el hospital, estado y outcome
    columns <- c("Hospital.Name", "State", outcome)
    data_outcome <- na.omit(data.frame(data[, columns]))
    
    #Separamos los datos en una lista de dataframes por estados y ordenamos en base a su
    #mortality rate (outcome) y luego el nombre del hospital.
    split_outcome <- split(x = data_outcome, f = data_outcome$State)
    outcome_order <- lapply(split_outcome, function(x) x[order(x[, outcome], x[, "Hospital.Name"]), ])
    
    #Aqui buscamos la primera posicion de cada dataframe de la lista y luego con el do call unimos
    #todos los dataframes de la lista en un solo dataframe.
    if(num == "best"){
      best_outcome <- lapply(outcome_order, function(x) x[1, c("Hospital.Name", "State")])
      table_outcome <- do.call("rbind", best_outcome)
    }
    
    #Aqui buscamos la ultima posicion de cada dataframe de la lista y luego con el do call unimos
    #todos los dataframes de la lista en un solo dataframe.
    else if(num == "worst"){
      worst_outcome <- lapply(outcome_order, function(x) x[nrow(x), c("Hospital.Name", "State")])
      table_outcome <- do.call("rbind", worst_outcome)
    }
    
    #Aqui buscamos la posicion "num" (argumento de la funcion) de cada dataframe de la lista y 
    #luego con el do call unimos todos los dataframes de la lista en un solo dataframe.
    else if(class(num) == "numeric"){
      rank_outcome <- lapply(outcome_order, function(x) x[num, c("Hospital.Name", "State")])
      table_outcome <- do.call("rbind", rank_outcome)
    }
      
  }
  
  #Se muestra la tabla que cumpla una de las condiciones anteriores.
  return(table_outcome)
}
