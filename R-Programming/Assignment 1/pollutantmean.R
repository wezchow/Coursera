pollutantmean <- function(directory, pollutant, id = 1:332) {
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
    
    
    ## Function to create a dataframe contains all data depends parameters that user gives
    myDataFrame <- function(directory, fileList) {
        
        fullDataFrame <- data.frame()
        
        for(i in fileList) {
            data <- read.csv(paste0(directory, "/", i), sep = ",")
            fullDataFrame <- rbind(fullDataFrame, data)
        }
        
        # Return fullDataFrame
        fullDataFrame
    }
    
    
    ## Create a function(pollutantmean) scope variable to contain the results of target datasets
    fullDataFrame <- tryCatch({
        myDataFrame(directory, fileList)
    }, warning = function(w) {
        print(w)
    }, error = function(e) {
        print(e)
    }, finally = {
        return
    })
    
    ## Calcuation depends on the condition given by user
    if(pollutant == "sulfate") {
        tryCatch({
            mean(fullDataFrame$sulfate, na.rm = TRUE)
        }, warning = function(w) {
            print(w)
        }, error = function(e) {
            print(e)
        }, finally = {
            return
        })
    } else if(pollutant == "nitrate") {
        tryCatch({
            mean(fullDataFrame$nitrate, na.rm = TRUE)
        }, warning = function(w) {
            print(w)
        }, error = function(e) {
            print(e)
        }, finally = {
            return
        })
    } else {
        print("Error, please double check your parameters.")
    }
}