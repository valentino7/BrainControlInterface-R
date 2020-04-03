split_data <- function(my_data){
  set.seed(123)
  
  rows <- 6
  columns <- 6
  number_of_words <- 6
  character_per_word <- 5
  iterations <- 1
  test_word<-sample(1:number_of_words,1)
  
  start_index<-(test_word-1)*(rows+columns)*character_per_word*iterations +1
  end_index<-start_index+(rows+columns)*character_per_word*iterations -1
  seq_test<-seq(start_index,end_index)
  
  training_set<-my_data[-seq_test,]
  test_set<-my_data[seq_test,]
  
  data_split <- list(training_set,test_set)
  names(data_split) <- c("training_set","test_set") 
  return(data_split)
}