library(jsonlite)
library(ridigbio)
library(dplyr)

## Lets look at the distribution of published recordset sizes
## Although recordsets are not representative of collections,
## we can look for trends in data publishing

rsEP <- "http://search.idigbio.org/v2/search/recordsets?limit=3000"
rsDF <- fromJSON(rsEP)
rsDF <- rsDF$items
rsDF <- rsDF$indexTerms
rsDF <- flatten(rsDF, recursive = TRUE)
rsDF$contacts <- NULL
rsDF$recordids <- NULL

rsDF <- rsDF %>% select(uuid,name,datemodified)

##Need to subset these into just known US recordsets
# and get the data from the collections catalog
df <- fromJSON("http://idigbio.github.io/idb-us-collections/collections.json")
#only collections with data
df <- df %>% filter(recordsets!="")

rsDF$us <- ""
for(i in seq_along(rsDF$uuid)){
  rsDF$us[i] <- any(grepl(rsDF$uuid[i],df$recordsets))
}
rsDF <- rsDF %>% filter(us==TRUE)

rsDF$count <- ""
for(i in seq_along(rsDF$uuid)){
        rsDF$count[i] <- idig_count_records(rq=list(recordset=rsDF$uuid[i]))
}
rsDF$count <- as.numeric(rsDF$count)

##Plot and save the histogram
pdf('recordset-histogram.pdf')
tmp <- rsDF %>% select(count,uuid) 
d <- density(tmp$count)
options(scipen=999)
histBreaks <- hist(tmp$count, main = "Recordset Counts\n (iDigBio Recordsets Contributed by U.S. Collections)",xlab = "Recordset Size (Millions of Records)",col = "grey",xaxt="n",yaxt="n",ylim=c(0,800))
axis(1, at=seq(min(tmp$count),max(tmp$count),by=1000000), labels=c("0", "1", "2", "3", "4", "5","6","7")) 
axis(2, at=seq(0,800,by=200), labels=c("0", "200", "400","600","800")) 
tmpHist <- hist(tmp$count,plot = F)$breaks
lines(x = d$x, y = d$y * length(tmp$count) * diff(tmpHist)[1], lwd = 2,col="red")
dev.off()



#########################################################
# Get data from the collections catalog
df <- fromJSON("http://idigbio.github.io/idb-us-collections/collections.json")
#only collections with data
df <- df %>% filter(recordsetQuery!="")
for (i in seq_along(df$collection_uuid)){
  if(nrow(as.data.frame(fromJSON(df$recordsetQuery[i])))>1 &length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))>1 & !class(fromJSON(df$recordsetQuery[i]))=="list"){
    countVector <- c()
    for(ii in 1:length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))){
      rqCount <- idig_count_records(rq=fromJSON(df$recordsetQuery[i],simplifyVector = list())[[ii]])
      countVector <- c(countVector,rqCount)
    }
    df$count[i] <- sum(countVector)
  }else{
    df$count[i] <- idig_count_records(rq=fromJSON(df$recordsetQuery[i]))
  }
    
}
##clean messy
rsDF <- df %>% filter(count>0)



###########################
options(scipen=999)
pdf('small-collections-histogram.pdf')
tmp <- rsDF %>% select(count,collection_uuid) %>% filter(count<=100000)
histBreaks <- hist(tmp$count, main = paste0("Collection Record Counts \n (Collection size less than 100,000 published records) \n n = ",nrow(tmp)),xlab = "Collections Size (Records)",col = "grey",xaxt="no")
axis(1, at=seq(0,100000,by=20000),labels = c("0","20,000","40,000","60,000","80,000","100,000")) 
d <- density(tmp$count)
lines(x = d$x, y = d$y * length(tmp$count) * diff(hist(tmp$count,plot = F)$breaks)[1], lwd = 2,col="red")
dev.off() 

##########################
# Write the data to a file for further inspection
smallColl <- rsDF %>% rowwise()%>%mutate(collection_link=paste0("https://www.idigbio.org/portal/collections/",strsplit(collection_uuid,split = "urn:uuid:")[[1]][2])) %>% select(count,institution, collection,collection_link) %>% filter(count<=100000)
smallColl <- smallColl[order(smallColl$count),]
write.csv(smallColl,"small-collections.csv",row.names = F)

