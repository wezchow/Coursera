complete <- function(directory, id = 1:332) {
    ## Function to create a vector contains all target filenames
    createFileList <- function(id) {
        # Force id must larger than zero
        id <- id[id > 0]
        
        # Adapte the real file name
        single <- vector()
        ten <- vector()
        hudreds <- vector()
        
        if(sum(id < 10) > 0) {
            single <- paste0(paste0("00", id[id<10]), ".csv")
        }         
        if(sum(id > 9 & id < 100) > 0) {
            ten <- paste0(paste0("0", id[id > 9 & id < 100]), ".csv")
        } 
        if(sum(id > 99) > 0) {
            hudreds <- paste0(id[id > 99], ".csv")
        }
        
        fileList <- c(single, ten, hudreds)
        
        # Return fileList
        fileList
    }
    
    
    ## Create a function(pollutantmean) scope variable to contain the results of target filenames
    fileList <- tryCatch({
        createFileList(id)
    }, warning = function(w) {
        print(w)
    }, error = function(e) {
        print(e)
    }, finally = {
        return
    })
    
        
    resultDataFrame <- data.frame(id=integer(), nobs=integer())
    
    for(i in fileList) {
        data <- read.csv(paste0(directory, "/", i), sep = ",")
        temp <- data.frame(id=data$ID[1], nobs=nrow(data[complete.cases(data),]))
        resultDataFrame <- rbind(resultDataFrame, temp)
    }
    
    # Return fullDataFrame
    resultDataFrame
}