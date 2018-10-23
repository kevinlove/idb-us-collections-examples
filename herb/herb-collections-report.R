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
  if(nrow(as.data.frame(fromJSON(df$recordsetQuery[i])))>1 &length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))>1& !class(fromJSON(df$recordsetQuery[i]))=="list"){
    countVector <- c()
    typeVector <- c()
    for(ii in 1:length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))){
      rqCount <- idig_count_records(rq=fromJSON(df$recordsetQuery[i],simplifyVector = list())[[ii]])
      countVector <- c(countVector,rqCount)
      rqType <- names(idig_top_records(rq=fromJSON(df$recordsetQuery[i],simplifyVector = list())[[ii]],top_fields ="kingdom" )[[1]])[1]
      typeVector <-c(typeVector,rqType)
    }
    df$count[i] <- sum(countVector)
    df$type[i] <- paste0(unique(typeVector),collapse = ", ")
  }else{
    df$count[i] <- idig_count_records(rq=fromJSON(df$recordsetQuery[i]))
    df$type[i] <- names(idig_top_records(rq=fromJSON(df$recordsetQuery[i]),top_fields ="kingdom" )[[1]])[1]
  }
    
}
## Simplify the dataframe for other uses
rsDF <- df %>% filter(count>0) %>% select(institution,collection,type,count)

## Subset to herb collections
herb <- df %>% filter(grepl("plantae",type,ignore.case = T)) %>% arrange(desc(count))

# Make histogram
options(scipen=999)
pdf('herb--collections-histogram.pdf')
histBreaks <- hist(herb$count, main = paste0("Collection Record Counts \n (Herbarium) \n n = ",nrow(herb)),xlab = "Collections Size (iDigBio Records)",col = "grey",xaxt="no")
axis(1, at=seq(0,4000000,by=500000),labels = c("0","500,000","1m","1.5m","2m","2.5m","3m","3.5m","4m")) 
d <- density(herb$count)
lines(x = d$x, y = d$y * length(herb$count) * diff(hist(herb$count,plot = F)$breaks)[1], lwd = 2,col="red")
dev.off() 

# Write csv
write.csv(herb,"herb--collections-histogram.csv",row.names = F)
