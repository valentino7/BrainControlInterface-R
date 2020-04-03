training_bestParameters <- function(df){
  rows <- 6
  columns <- 6
  number_of_words <- 6
  character_per_word <- 5
  iterations <- 1
  res <- matrix(0, nrow = number_of_words, ncol = 1)
  best_c <- 0.01
  for(i in 1:6){
    start_index<-(i-1)*(rows+columns)*character_per_word*iterations +1
    end_index<-start_index+(rows+columns)*character_per_word*iterations -1
    seq_test<-seq(start_index,end_index)
    
    training_set<-df[-seq_test,]
    test_set<-df[seq_test,]
    
    data_train <- training_set[,1:(ncol(training_set)-2)]
    character_train <- training_set[,(ncol(training_set)-1)]
    label_train <- training_set[,ncol(training_set)]
    
    data_test <- test_set[,1:(ncol(test_set)-2)]
    character_test <- test_set[,(ncol(training_set)-1)]
    label_test <- test_set[,ncol(test_set)]
    
    scaled_training <- scale(data_train,center = T,scale = T)
    scaled_test <- scale(data_test,attr(scaled_training,"scaled:center"),attr(scaled_training,"scaled:scale"))
    res[i]<-linear_SVM(as.data.frame(scaled_training),label_train,as.data.frame(scaled_test),label_test,character_test,best_c)
    
  }
  
  
  avg_accuracy<- lapply(res,function(value)paste(value*100,"%",sep=""))
  res <- do.call("rbind",avg_accuracy)
  colnames(res) <- c("Avg_Accuracy")
  return(res)
}