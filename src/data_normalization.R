data_normalization<-function(df){
  data_train <- df$training_set[,1:(ncol(df$training_set)-2)]
  character_train <- df$training_set[,(ncol(df$training_set)-1)]
  label_train <- df$training_set[,ncol(df$training_set)]
  
  data_test <- df$test_set[,1:(ncol(df$test_set)-2)]
  character_test <- df$test_set[,(ncol(df$training_set)-1)]
  label_test <- df$test_set[,ncol(df$test_set)]
  
  scaled_training <- scale(data_train,center = T,scale = T)
  scaled_test <- scale(data_test,attr(scaled_training,"scaled:center"),attr(scaled_training,"scaled:scale"))
  
  output <- list(as.data.frame(scaled_training),label_train,character_train,as.data.frame(scaled_test),label_test,character_test)
  names(output) <- c("scaled_training","label_train","character_train","scaled_test","label_test","character_test")
  return(output)
}