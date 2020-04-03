data_preprocessing<- function(X,words_number){
  
  iterations <- 10
  rows <- 6
  columns <- 6
  character_per_word <- 5

  df <- X[FALSE,]
  index = (rows+columns)*iterations
  for (i in 0:((words_number*character_per_word)-1) ){
    k<-1
    start = (i*index) +1
    end = (i+1)*(index)
    filtered <- X[start:end,]
    agg = aggregate(filtered,
                    by = list(filtered$C),
                    FUN = mean)
    agg = agg[,2:ncol(agg)]
    df[(nrow(df)+1):(nrow(df)+nrow(agg)),] <- agg[1:nrow(agg),]
    k<-k+1
  }
  return(df)
  
}