training <- function(df){
    data_train <- df[,1:(ncol(df)-2)]
    character_train <- df[,(ncol(df)-1)]
    label_train <- df[,ncol(df)]
    scaled_training <- scale(data_train,center = T,scale = T)
    # training
    linear_model<-LiblineaR(data=scaled_training, target=label_train, type=1, cost=0.01, bias=TRUE, verbose=FALSE)
    m <-  list(linear_model,scaled_training)
    names(m) <- c("linear_model","scaled_training")
    return(m)

}