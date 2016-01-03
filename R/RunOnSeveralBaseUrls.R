RunOnSeveralBaseUrls <-
function(urls){
  listOfDataFrames <- lapply(urls, parseBaseUrl)
  ldply(listOfDataFrames, data.frame)
}
