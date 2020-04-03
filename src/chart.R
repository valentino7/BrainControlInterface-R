chart<- function(df,plot_dir){
  signals <- 204
  words_number <- 6
  character_per_word <- 5
  
  
  my_plot <- function(filter,cnames,dir) {
    res <- gather(filter,key,value,cnames)
    ggplot(data=res,aes(x = Signals, y = value, colour=key)) +
      geom_line() +
      xlab("Time step") +
      ylab("Values")
    ggsave(paste(dir, paste(paste("canale",i),"png", sep="."),sep=""))
    
  }
  
  for (j in 0:29 ){
    w<-paste("character",j)
    plot_dir_word_J <- paste(plot_dir, w , sep="/" )
    dir.create(plot_dir_word_J)
    
    s<-(j*12)+1
    e<- (j+1)*12
    dataf<- df[s:e,]
    dataTrue <- dataf[dataf["Y"]==1,]
    dataFalse <- dataf[dataf["Y"]==-1,]
    for (i in 0:7){
      start <- (signals*i)+1
      end <- signals*(i+1)
      electrods <- c("Fz","Cz","Pz","Oz","P3","P4","P7","P8")
      signal_filter <- dataf[,(start:end)]
      signal_filter_true <- dataTrue[,start:end]
      signal_filter_false <- dataFalse[,start:end]
      
      cnames <- dataf["C"] 
      cnames_true <- dataTrue["C"] 
      cnames_false <- dataFalse["C"] 

      matrix <- data.matrix(signal_filter)
      matrix <- t(matrix)
      
      cnames <- as.character(t(cnames))
      cnames_true <- as.character(t(cnames_true))
      cnames_false <- as.character(t(cnames_false))
      
      matrix_true <-data.matrix(signal_filter_true)
      matrix_true <- t(matrix_true)
      matrix_false <-data.matrix(signal_filter_false)
      matrix_false <- t(matrix_false)
      
      filter <- as.data.frame(matrix)
      filter_true <-as.data.frame(matrix_true)
      filter_false <- as.data.frame(matrix_false)
      
      colnames(filter) <- cnames
      colnames(filter_true) <- cnames_true
      colnames(filter_false) <- cnames_false
  
      filter$Signals <- c(1:signals)
      filter_true$Signals <- c(1:signals)
      filter_false$Signals <- c(1:signals)
      
      plot_dir_all<- paste(plot_dir_word_J, "all/", sep="/" )
      dir.create(plot_dir_all)
     
      plot_dir_true <- paste(plot_dir_word_J, "true/", sep="/" )
      dir.create(plot_dir_true)
      
      plot_dir_false <- paste(plot_dir_word_J, "false/", sep="/" )
      dir.create(plot_dir_false)
      
      my_plot(filter,cnames,plot_dir_all)
      my_plot(filter_true,cnames_true,plot_dir_true)
      my_plot(filter_false,cnames_false,plot_dir_false)
    }
   
    
  }
  
}