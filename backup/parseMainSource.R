library(plyr)
library(tidyr)
library(dplyr)
library(rvest)
library(XML)
library(stringr)
library(quantmod)
source("private.R")
from <- c("USD","CRC")
to <- c("CRC", "USD")
test <- getQuote(paste0(from, to, "=X"))




RunOnSeveralBaseUrls<- function(urls){
  listOfDataFrames <- lapply(urls, parseBaseUrl)
  ldply(listOfDataFrames, data.frame)
}


parseBaseUrl <- function(url){
  pageVector <- sapply(1:getTotalNumberOfPages(url), function(i){getNewPageUrl(i,url)})
  pageVector
  listOfDataFrames <- lapply(pageVector, parsePage)
  ldply(listOfDataFrames, data.frame)
}

getTotalNumberOfPages <- function(url){
  mainUrl <- html(url)
  numberOfPages <- mainUrl %>% html_nodes(".paging_sec strong")  %>%
    html_text()
  numberOfPages <- numberOfPages[2]
  as.numeric(numberOfPages)
}
parsePage <- function(page){
  searchPage <- html(page)
  blocks <- searchPage %>%  html_nodes(".advice.linear")
  listOfDataFrames <- lapply(blocks, parseBlock)
  
  df<- ldply(listOfDataFrames, data.frame) %>% filter(Property != "Visitas")  %>%
  mutate(Property = camel(as.character(Property)))  %>%
  spread(Property, Value)  %>%
  separate(EstatePrice, c("Currency", "Price"), sep=" ", extra="drop")  %>%
  mutate(Price = as.numeric(gsub(",", "", Price)))
  
  if(!"MtsTerreno" %in% names(df)){
    df$MtsTerreno <- c(0)
  }
  if(!"MtsConstruccion" %in% names(df)){
    df$MtsConstruccion <- c(0)
  }
  df  %>%
  mutate( Currency = enc2native(Currency)
         ,Provincia = enc2native(Provincia)
         ,Canton = enc2native(Canton)
         ,Direccion = enc2native(Direccion)
         ,EstateName = enc2native(EstateName)
         ,EstateSeller = enc2native(EstateSeller)
         ,MtsConstruccion = as.numeric(MtsConstruccion)
         ,MtsTerreno	 = as.numeric(MtsTerreno)
         ) %>%
  select(Source, EstateId, Provincia, Canton, Direccion, EstateName, Currency, Price, EstateSeller, MtsConstruccion, MtsTerreno, Publicado, URL)
}

camel <- function(x){ #function for camel case and removing accents
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  temp<-sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=""))
  iconv(temp,from="UTF-8", to="ASCII//TRANSLIT")
}

getNewPageUrl <- function(index, url){
  paste(url, "?page=", index, sep ="")
}

parseBlock <- function(block){
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

RunMainSource <- function(){RunOnSeveralBaseUrls(baseUrls)}