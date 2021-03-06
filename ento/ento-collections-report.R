library(jsonlite)
library(ridigbio)
library(dplyr)

#########################################################
# Get data from the collections catalog
df <- fromJSON("http://idigbio.github.io/idb-us-collections/collections.json")
# Let's only look at collections with data
df <- df %>% filter(recordsetQuery!="")
# Use the iDigBio search API to add back our counts and collection types
for (i in seq_along(df$collection_uuid)){
  if(nrow(as.data.frame(fromJSON(df$recordsetQuery[i])))>1 &length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))>1 & !class(fromJSON(df$recordsetQuery[i]))=="list"){
    countVector <- c()
    typeVector <- c()
    for(ii in 1:length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))){
      rqCount <- idig_count_records(rq=fromJSON(df$recordsetQuery[i],simplifyVector = list())[[ii]])
      countVector <- c(countVector,rqCount)
      rqType <- names(idig_top_records(rq=fromJSON(df$recordsetQuery[i],simplifyVector = list())[[ii]],top_fields ="phylum" )[[1]])[1]
      typeVector <-c(typeVector,rqType)
    }
    df$count[i] <- sum(countVector)
    df$type[i] <- paste0(unique(typeVector),collapse = ", ")
  }else{
    df$count[i] <- idig_count_records(rq=fromJSON(df$recordsetQuery[i]))
    df$type[i] <- names(idig_top_records(rq=fromJSON(df$recordsetQuery[i]),top_fields ="phylum" )[[1]])[1]
  }
    
}
## Simplify the dataframe for other uses
rsDF <- df %>% filter(count>0) %>% select(institution,collection,type,count)

## Subset to arthropod collections
ento <- df %>% filter(grepl("arthropoda",type,ignore.case = T)) %>% arrange(desc(count))

# Make histogram
options(scipen=999)
pdf('ento--collections-histogram.pdf')
histBreaks <- hist(ento$count, main = paste0("Collection Record Counts \n (Entomology) \n n = ",nrow(ento)),xlab = "Collections Size (iDigBio Records)",col = "grey",xaxt="no")
axis(1, at=seq(0,1400000,by=200000),labels = c("0","200,000","400,000","600,000","800,000","1m","1.2m","1.4m")) 
d <- density(ento$count)
lines(x = d$x, y = d$y * length(ento$count) * diff(hist(ento$count,plot = F)$breaks)[1], lwd = 2,col="red")
dev.off() 

# Write csv
write.csv(ento,"ento--collections-histogram.csv",row.names = F)