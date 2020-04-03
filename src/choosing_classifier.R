choosing_classifier <- function(df){
  set.seed(123)
  n_classifiers <- 1:3
  n_folds <- 5
  rows_for_word <- 60 
  
  classification_parameter <- lapply(n_classifiers,function(n_classifiers)matrix(0, nrow = n_folds, ncol = 1))
  names(classification_parameter) <- c("linear_SVM","gaussian_SVM","polynomial_SVM")
  for (k in 1:n_folds) {
    start<-(k-1)*rows_for_word+1 
    validation_index<-seq(start,start+rows_for_word-1)
    
    train <- df$scaled_training[-validation_index, ]
    validation <- df$scaled_training[validation_index, ]
    
    character_train <- df$character_train[-validation_index]
    character_validation <- df$character_train[validation_index]

    label_train <- df$label_train[-validation_index]
    label_validation <- df$label_train[validation_index]
    

    classification_parameter$linear_SVM[k,] <- linear_SVM(train,label_train,validation,label_validation,character_validation,0.01)
    classification_parameter$gaussian_SVM[k,] <- gaussian_SVM(train,label_train,validation,label_validation,character_validation,0.01)
    classification_parameter$polynomial_SVM[k,] <- polynomial_SVM(train,label_train,validation,label_validation,character_validation,0.01)
    
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