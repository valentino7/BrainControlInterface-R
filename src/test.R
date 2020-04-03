
test <- function(model,data){
  
  row_for_words <- 600
  words_number<- nrow(data) / row_for_words
  df<- data_preprocessing(data,words_number)
  df <- remove_signals(df)
  row_for_character <- 12
  data_test <- df[,1:(ncol(df)-2)]
  character_test <- df[,(ncol(df)-1)]
  label_test <- df[,ncol(df)]
  
  scaled_test <- scale(data_test,attr(model$scaled_training,"scaled:center"),attr(model$scaled_training,"scaled:scale"))
  predictions <- predict(model$linear_model, scaled_test, decisionValues = TRUE)
  
  my_pred <- predictions$decisionValues[,1]
  start_index_rc<-seq(1,nrow(data_test),by=row_for_character/2)
  for (i in start_index_rc){
    end<- i + (row_for_character/2)-1
    temp <- my_pred[i:end]
    y_true<-which.max(temp)
    my_pred[seq(i,end)[y_true]]<-1
    my_pred[seq(i,end)[-y_true]]<--1
  }
  
  
  start_index_char<-seq(1,nrow(data_test),by=row_for_character)
  count<-0 
  for (start in start_index_char){
    end<-start+11
    bool<-TRUE
    for (i in start:end){
      if (label_test[i]!=my_pred[i]){
        bool<-FALSE
        break
      }
    }
    if (bool==TRUE)
      count<-count+1
  }
  accuracy <- count/length(start_index_char)
  print(paste("La parola predetta con linear_SVM :",prediction_to_character(my_pred,character_test),"; Accuracy:",paste(accuracy*100,"%",sep="")))
  return (accuracy)
  
}