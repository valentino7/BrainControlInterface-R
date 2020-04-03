linear_SVM <- function(TR,YTR,VS,YVS,CVS,c){
  set.seed(123)
  row_for_character <- 12
  
  # training
  model<-LiblineaR(data=TR, target=YTR, type=1, cost=c, bias=TRUE, verbose=FALSE)
  # prediction
  predictions <- predict(model, VS, decisionValues = TRUE)
 
  my_pred <- predictions$decisionValues[,1]
  start_index_rc<-seq(1,nrow(VS),by=6)
  for (i in start_index_rc){
    end<- i + 5
    temp <- my_pred[i:end]
    y_true<-which.max(temp)
    my_pred[seq(i,end)[y_true]]<-1
    my_pred[seq(i,end)[-y_true]]<--1
    
  }
  
  
  start_index_char<-seq(1,nrow(VS),by=row_for_character)
  count<-0 # conta quanti sono i caratteri indovinati dentro una parola
  for (start in start_index_char){
    end<-start+(row_for_character-1)
    bool<-TRUE
    for (i in start:end){
      if (YVS[i]!=my_pred[i]){
        bool<-FALSE
        break
      }
    }
    if (bool==TRUE)
      count<-count+1
  }

  accuracy <- count/length(start_index_char)
  print(paste("La parola predetta con linear_SVM :",prediction_to_character(my_pred,CVS),"; Accuracy:",paste(accuracy*100,"%",sep="")))
  #length(start_index_char) : numero di lettere dentro una parola
  return (accuracy)
}