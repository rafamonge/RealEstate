parseBlock <-
function(block){
  estateUrl <- block %>% 
    html_nodes(".top_row a") %>%
    html_attr("href")  
  
  esteCategory <- block %>% 
    html_nodes(".top_row a") %>%
    html_text()
  
  estatePrice <- block %>% 
    html_nodes(".top_row span") %>%
    html_text()
  
  estateProperties <- block %>% 
    html_nodes("strong") %>%
    html_text(ignoreComments = TRUE, trim= TRUE,recursive =FALSE) %>%
    str_replace(":", "") %>%
    str_replace ("\n\n\n", "") %>%
    str_trim()
  
  estatePropertiesValueMinusVisitsAndPublishDate <- block %>% 
    html_nodes(".left_sec01 > span") %>%
    html_text(recursive = F)
  
  ## clearly wrong...
  estateVisits <- block %>% 
    html_nodes(".first span") %>%
    html_text()
  
  ## clearly wrong...
  estatePublishDate<- block %>% 
    html_nodes(".gray li:nth-child(2)") %>%
    html_text(recursive = F)
  
  estatePropertiesValues <- str_trim(c(estatePropertiesValueMinusVisitsAndPublishDate,estateVisits,estatePublishDate), side="both")
  
  splitUrl <- str_split(estateUrl, "/")[[1]]
  estateName <- splitUrl[7]
  estateSeller <- splitUrl [5]
  estateId  <- splitUrl[6]
  
  estatePropertiesValues <- c(mainSource,estateName, estateSeller, estatePrice,estatePropertiesValues, estateUrl)
  estateProperties <- c("Source", "EstateName", "EstateSeller", "EstatePrice", estateProperties, "URL")
  
  
  
  data.frame(EstateId = estateId, Property  = estateProperties, Value = estatePropertiesValues)
  
}
