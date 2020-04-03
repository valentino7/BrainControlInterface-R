gaussian_SVM<-function(TR,YTR,VS,YVS,CVS,C){
  set.seed(123)
  
  row_for_character <- 12
  
  #training
  model<-svm(x=TR,y=YTR,scale=F,type="C-classification",kernel="radial",cost=C) 
  #prediction
  predictions <- predict(model,VS,decision.values = T)
  
  
  start_index_rc<-seq(1,nrow(VS),by=row_for_character/2)
  for (i in start_index_rc){
    end<-i+(row_for_character/2)-1
    my_pred<-attr(predictions,"decision.values")[i:end]
    y_true<-which.min(my_pred)
    predictions[seq(i,end)[y_true]]<-1
    predictions[seq(i,end)[-y_true]]<--1
    
  }
  
  start_index_char<-seq(1,nrow(VS),by=row_for_character)
  count<-0
  for (start in start_index_char){
    end<-start+row_for_character-1
    bool<-TRUE
    for (i in start:end){
      if (YVS[i]!=predictions[i]){
        bool<-FALSE
        break
      }
    }
    if (bool==TRUE)
      count<-count+1
  }
  accuracy <- count/length(start_index_char)
  print(paste("La parola predetta con gaussian_SVM :",prediction_to_character(predictions,CVS),"; Accuracy:",paste(accuracy*100,"%",sep="")))
  return (accuracy)
}