# This function takes as input a tibble dataframe and cross tabulation variable
# It then crosstabulates all factors and puts them into a table.  If numeric, 
# it calculates a mean with na.rm.

nfCrossTable <- function(data, CTvar){
  
  do.call(rbind, sapply(data, function(x){
    if(is.numeric(x)==TRUE){
      means<-t(aggregate(x ~ CTvar, FUN=mean, na.action=na.omit))[2,]
    }
    else{
      if(length(levels(x))==2){prop.table(table(x, CTvar),2)[2,]}
      else{prop.table(table(x, CTvar),2)
      }
    }
  }))
  
}
