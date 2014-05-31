corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  id = 1:332
  v <- vector(mode="numeric", length = 0)
  filenames <- sprintf("%03d.csv", id)
  filenames <- paste(directory, filenames, sep="/")
  dataframes <- lapply(filenames, read.csv)
  
  for(i in 1:length(dataframes))
  {
    df <- dataframes[[i]]
    complete_rows = nrow(df[complete.cases(df), ])
    
    if(complete_rows > threshold)
    {
      val <- cor(df$sulfate, df$nitrate, use="complete.obs")
      v <- c(v, val)
    }    
  }
  
  as.numeric(v)
}