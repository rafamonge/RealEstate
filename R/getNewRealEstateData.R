#' TitlegetNewRealEstateData
#'
#' Scraps the sources to extract new real estate data. Adds new records to the database. creates a backip of the previous version of the database.
#' @param database 
#' @param backup 
#'
#' @return NA
#' @export
#'
#' @examples
#' getNewRealEstateData()
getNewRealEstateData <-
function(database ="RealEstate.csv",backup="RealEstateBackup.csv"){
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
