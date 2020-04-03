prediction_to_character<-function(YTR,CTR){
  B <-matrix(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","1","2","3","4","5","6","7","8","9","_"), byrow = TRUE, ncol = 6)
  row_for_character <- 12
  start_index_char<-seq(1,length(YTR),by=row_for_character)
  string<-""
  for (start in start_index_char){
    end<- start+ row_for_character-1
    row<-1
    col<-1
    for (i in start:end){
      if (YTR[i]==1 && CTR[i]<=6){
        row<-CTR[i]
      }
      if (YTR[i]==1 && CTR[i]> 6){
        col<-CTR[i]-6
      }
    }
    string<-paste(string,B[row,col],sep="")
  }
  return (string)
  
}