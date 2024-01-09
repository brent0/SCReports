toNums<-function(x,cols,numerics=T) {
  for(i in 1:length(cols)) {
    if(is.factor(x[,cols[i]])) {
      if(numerics==F){
        x[,cols[i]]<-as.character(levels(x[,cols[i]])[x[,cols[i]]])
      }
      else {
        x[,cols[i]]<-as.numeric(levels(x[,cols[i]])[x[,cols[i]]])
      }
    }
    else {
      x[,cols[i]]<-as.numeric(x[,cols[i]])
    }
  }
  return(x)
}