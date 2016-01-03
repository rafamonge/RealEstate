source("parseMainSource.R")
source("AddPriceInUSD.R")
#install.packages("openxlsx")
require(openxlsx, quietly = TRUE)

run <- function(database ="RealEstate.csv",backup="RealEstateBackup.csv"){
  currentResults <- RunMainSource()
  names(currentResults) <- enc2native(names(currentResults))
  currentResults$PriceInUSD <- 0
  merged <- NA
  if(file.exists(database)){
    oldResults <- read.csv(database,stringsAsFactors=F)
    
    merged <- rbind(oldResults,currentResults)
    
    duplicatedItems <- duplicated(merged[,c("Source", "EstateId")])
    merged <- merged[!duplicatedItems,]
    file.rename(database,backup)    
  }
  else{
    merged <- currentResults
  }
  merged<- AddPriceInUSD(merged)
  options(scipen=999)
  write.csv(merged, database,row.names=F)
  options(scipen=0)
  save(merged)
  realEstateDf <<- merged
  
  
}
#realEstateDf <- read.csv("RealEstate.csv",stringsAsFactors=F)
#write.csv(realEstateDf, "RealEstate.csv",row.names=F)


save <- function(df, outputFile="realEstateDf.xlsx" ){
  wb <- createWorkbook()
  addWorksheet(wb, "realEstateDf")
  writeDataTable(wb, sheet = 1, x = df)
  setColWidths(wb, sheet = 1, cols=1:ncol(df), widths = "auto")
  saveWorkbook(wb, outputFile, overwrite=T)
}

#realEstateDf$PriceInUSD / realEstateDf$MtsConstruccion
