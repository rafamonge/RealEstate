getTotalNumberOfPages <-
function(url){
  mainUrl <- html(url)
  numberOfPages <- mainUrl %>% html_nodes(".paging_sec strong")  %>%
    html_text()
  numberOfPages <- numberOfPages[2]
  as.numeric(numberOfPages)
}
