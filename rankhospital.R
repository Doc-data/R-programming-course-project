rankhospital <- function(state, outcome, num = "best") {

    data <- read.csv("outcome-of-care-measures.csv")

    outcomes <- c("heart failure" , "heart attack" , "pneumonia")
   
     if(!any(data$State == state)){
        print("invalid state")
        stop()
        }
   
     if(!any(outcome %in% outcomes)){
        print("invalid outcome")
        stop()
        }
    
    else{
    data <- subset(data, data$State == state, drop = T)
    
    if (outcome == "heart attack")
            {
                data[,11] <- as.numeric(data[,11])
                data_HA <- data[order(data[,11], data[,2]), ]
                Hos_name <- as.vector(data_HA$Hospital.Name[!is.na(data_HA[,11])])
            }  
    if (outcome == "heart failure")
    {
        data[,17] <- as.numeric(data[,17])
        data_HA <- data[order(data[,17],data[,2]), ]
        Hos_name <- as.vector(data_HA$Hospital.Name[!is.na(data_HA[,17])])
    } 
    
    if (outcome == "pneumonia")
    {
        data[,23] <- as.numeric(data[,23])
        data_HA <- data[order(data[,23],data[,2]), ]
        Hos_name <- as.vector(data_HA$Hospital.Name[!is.na(data_HA[,23])])
    }  
    
    }
  
    if (num == "best"){num <- 1}
    if (num == "worst"){num <- length(Hos_name)}
    Hos_name[num]
}


