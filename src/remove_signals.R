remove_signals <- function(dataf){
  ALL=204
  M=38
  # FIRST=30
  # firstNToRemove <- c(
  #   paste("Fz_",1:FIRST,sep=""),
  #   paste("Cz_",1:FIRST,sep=""),
  #   paste("Pz_",1:FIRST,sep=""),
  #   paste("Oz_",1:FIRST,sep=""),
  #   paste("P3_",1:FIRST,sep=""),
  #   paste("P4_",1:FIRST,sep=""),
  #   paste("P7_",1:FIRST,sep=""),
  #   paste("P8_",1:FIRST,sep="")
  # )
  lastNToRemove <- c(
    paste("Fz_",(ALL-M+1):ALL,sep=""),
    paste("Cz_",(ALL-M+1):ALL,sep=""),
    paste("Pz_",(ALL-M+1):ALL,sep=""),
    paste("Oz_",(ALL-M+1):ALL,sep=""),
    paste("P3_",(ALL-M+1):ALL,sep=""),
    paste("P4_",(ALL-M+1):ALL,sep=""),
    paste("P7_",(ALL-M+1):ALL,sep=""),
    paste("P8_",(ALL-M+1):ALL,sep="")
  )
  #dataf <- dataf[ , -which(names(dataf) %in% firstNToRemove)] 
  df <- dataf[ , -which(names(dataf) %in% lastNToRemove)]
  return(df)
}