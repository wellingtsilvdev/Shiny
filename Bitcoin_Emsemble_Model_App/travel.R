
travel <-  function(data,element = 0,last.na = F,first_row.na=F,df_matrix=T){
  
  #charging zoo library to use na.locf function
  library(zoo)
  
  #if the dataset are not a dataframe or a matrix, convert into one to avoid errors
  if(df_matrix==F){
    data <- as.data.frame(data)
  }
  
  #replacing inf an NaN values by NA to replace this NA's latter
  data[sapply(data,is.infinite)] <- NA
  data[sapply(data,is.nan)] <- NA

  #verifying first_row.na
  if(first_row.na==T){
    
    #replacing first row NA's,NaN or inf elements
    data[1,is.na(data[1,])] <- element
  }
  
  #replacing NA elements by element that was choosed in the "element" argument
  if(last.na==F){
    
    data[is.na(data)] <- element
  }
  
  #replacing NA elements by the previews values using na.locf()
  if(last.na==T){
    
    data <- na.locf(data)
  }
  return(data)
}
