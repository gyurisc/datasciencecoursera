complete <- function(directory, id = 1:332) 
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  filenames <- sprintf("%03d.csv", id)
  filenames <- paste(directory, filenames, sep="/")
  dataframes <- lapply(filenames, read.csv)
  
  n <- length(dataframes)
  v <- vector(mode="numeric", length=n)
  
  for(i in 1:length(dataframes))
  {
    cc <- complete.cases(dataframes[[i]])
    df_correct <- dataframes[[i]][cc,]
    v[i] <- nrow(df_correct)
    
    ## df <- rbind(df, dataframes[[i]])
  }
  
  result <- data.frame(id = id, nobs = v)
  result
}