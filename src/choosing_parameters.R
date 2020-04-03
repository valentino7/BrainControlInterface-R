choosing_parameters <- function(df){
  set.seed(123)
  c_vector <- c( 10, 1, 0.1, 0.01, 0.001, 0.0001,0.00001)
  
  n_parameters <- 1:length(c_vector)
  n_folds <- 5
  rows_for_word <- 60 
  
  classification_parameter <- lapply(n_parameters,function(n_parameters)matrix(0, nrow = n_folds, ncol = 1))
  names(classification_parameter) <- c(paste("c", c_vector, sep = "_"))
  for (j in 1:length(c_vector)) {
   for (k in 1:n_folds) {
     start<-(k-1)*rows_for_word+1 
     validation_index<-seq(start,start+rows_for_word-1)
     
     train <- df$scaled_training[-validation_index, ]
     validation <- df$scaled_training[validation_index, ]
     
     character_train <- df$character_train[-validation_index]
     character_validation <- df$character_train[validation_index]
     
     label_train <- df$label_train[-validation_index]
     label_validation <- df$label_train[validation_index]

     classification_parameter[[j]][k,] <-  linear_SVM(train,label_train,validation,label_validation,character_validation,c_vector[j])
     #classification_parameter[[j]][k,] <-  gaussian_SVM(train,label_train,validation,label_validation,character_validation,c_vector[j])
     #classification_parameter[[j]][k,] <-  polynomial_SVM(train,label_train,validation,label_validation,character_validation,c_vector[j])
     
     
     }
  }
  mean_parameter <- lapply(classification_parameter, function(x){
    x <- replace(x, is.na(x), 0)
    parameter_mean <- apply(x,2,mean)
    return(paste(parameter_mean*100,"%",sep=""))
  })
  classifier_accuracy <- do.call("rbind",mean_parameter)
  colnames(classifier_accuracy) <- c("Avg_Accuracy")
  return(classifier_accuracy)
}