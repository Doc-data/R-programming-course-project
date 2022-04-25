rankall <- function(outcome, num = "best")
{
    dt <- read.csv("outcome-of-care-measures.csv")
    outcomes <-
      c("heart failure" , "heart attack" , "pneumonia")

    states <- as.vector(levels(factor(data$State)))

    if (!any(outcome %in% outcomes))
    {
        print("invalid outcome")
        stop()
    }

    if (outcome == "heart attack"){colnum <- 11}
    if(outcome == "heart failure"){colnum <- 17}
    if(outcome == "pneumonia"){colnum <- 23}

    dt[,colnum] <- as.numeric(dt[,colnum])
    dt <- dt[!is.na(dt[,colnum]),]


    splitted <- split(dt, dt$State)

    ans = lapply (splitted,
                  function(x, num)
                        {
                    x = x[order(x[,colnum], x$Hospital.Name),]

                    if (num == "best"){return(x$Hospital.Name[1])}
                    else if (num == "worst"){return(x$Hospital.Name[nrow(x)])}
                    else{
                        return(x$Hospital.Name[num])}}, num)
                        return( data.frame(hospital=unlist(ans), state=names(ans)) )


}

    
    
    
    
    
    

    
    
    