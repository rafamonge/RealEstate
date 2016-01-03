parseBaseUrl <-
function(url){
  pageVector <- sapply(1:getTotalNumberOfPages(url), function(i){getNewPageUrl(i,url)})
  pageVector
  listOfDataFrames <- lapply(pageVector, parsePage)
  ldply(listOfDataFrames, data.frame)
}
