best <- function(state, outcome) {
    
    data <- read.csv("outcome-of-care-measures.csv")
    
    outcomes <- c("heart failure" , "heart attack" , "pneumonia")
    
    if(!any(data$State == state))
        {
        print("invalid state")
        stop()
        }
    
    if(!any(outcome %in% outcomes))
        {
        print("invalid outcome")
        stop()
        }
   
    else
        {
    data <- subset(data, data$State == state, drop = T)
    
    if (outcome == "heart attack")
        {
        data[,23] <- as.numeric(data[,23])
        Hos_name <- data$Hospital.Name[data[,11] == min(data[,11], na.rm = T)]
        }   
    if (outcome == "heart failure")
        {
        data[,17] <- as.numeric(data[,17])
        Hos_name <- data$Hospital.Name[data[,17] == min(data[,17], na.rm = T)]
        }
    if (outcome == "pneumonia")
    {
        data[,23] <- as.numeric(data[,23])
        Hos_name <- c(data$Hospital.Name[data[,23] == min(data[,23], na.rm = T)])
        }

    Hos_name <- sort(Hos_name)
    Hos_name[1]
}
}
