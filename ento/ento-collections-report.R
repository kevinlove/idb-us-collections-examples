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
  if(nrow(as.data.frame(fromJSON(df$recordsetQuery[i])))>1 &length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))>1 & !grepl(pattern = "a6e02b78-6fc6-4cb6-bb87-8d5a443f2c2a|271a9ce9-c6d3-4b63-a722-cb0adc48863f|9d8ced48-62c5-4ce0-99e7-a03550c674c0|b000920c-6f7d-49d3-9d0f-2bb630d2e01a|042dbdba-a449-4291-8777-577a5a4045de|9dce915b-3de4-4a7d-a68d-e4c4c15809ce|5386d272-06c6-4027-b5d5-d588c2afe5e5",x = df$recordsetQuery[i])){
    countVector <- c()
    typeVector <- c()
    for(ii in 1:length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))){
      rqCount <- fromJSON(paste0("http://search.idigbio.org/v2/search/records?rq=",URLencode(
        substr(toJSON(fromJSON(df$recordsetQuery[i])[ii,]),2,nchar(toJSON(fromJSON(df$recordsetQuery[i])[ii,]))-1)
      )))$itemCount
      countVector <- c(countVector,rqCount)
      rqType <- names(fromJSON(paste0("http://search.idigbio.org/v2/summary/top/records?top_fields=[\"phylum\"]&rq=",URLencode(
        substr(toJSON(fromJSON(df$recordsetQuery[i])[ii,]),2,nchar(toJSON(fromJSON(df$recordsetQuery[i])[ii,]))-1)
      )))$phylum)
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