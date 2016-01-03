library(tidyr)
library(dplyr)
library(devtools)
#install_github("ndphillips/yarrr")
library("yarrr")

#realEstateDfAnalysis <- prepareForAnalysis(realEstateDf)
#save(realEstateDfAnalysis, "realEstateDfAnalysis.xlsx")

prepareForAnalysis <- function(data){
  data %>%
    mutate(PriceByM2Construccion = PriceInUSD / MtsConstruccion ) %>%
    ##filter( 300 <= PriceByM2Construccion & PriceByM2Construccion <=  1700           )
    group_by(Provincia)  %>%
    mutate(NormalizedPriceByM2Construccion = as.vector(scale(PriceByM2Construccion) ))
  
}

getSummaryPriceInUSDByProvincia <- function(data){
  data %>%
    mutate (Provincia = enc2native(Provincia))   %>%
    group_by(Provincia)  %>%
    summarise(Min = min(PriceInUSD, na.rm=TRUE),
              Q1 = quantile(PriceInUSD, na.rm =T, probs=c(0.25)),
              Median = median(PriceInUSD, na.rm=TRUE),
              Q3 = quantile(PriceInUSD, na.rm =T, probs=c(0.75)),
              Max = max(PriceInUSD,na.rm=TRUE),
              Iqr =  IQR(PriceInUSD, na.rm=TRUE),
              Mean = mean(PriceInUSD, na.rm=TRUE),
              Var = var(PriceInUSD, na.rm=TRUE),
              SD = sd(PriceInUSD, na.rm=TRUE),
              N = n()
              )
}

getSummaryPriceByM2ConstruccionByProvincia <- function(data){
  data %>%
    mutate (Provincia = enc2native(Provincia))   %>%
    group_by(Provincia)  %>%
    summarise(Min = min(PriceByM2Construccion, na.rm=TRUE),
              Q1 = quantile(PriceByM2Construccion, na.rm =T, probs=c(0.25)),
              Median = median(PriceByM2Construccion, na.rm=TRUE),
              Q3 = quantile(PriceByM2Construccion, na.rm =T, probs=c(0.75)),
              Max = max(PriceByM2Construccion,na.rm=TRUE),
              Iqr =  IQR(PriceByM2Construccion, na.rm=TRUE),
              Mean = mean(PriceByM2Construccion, na.rm=TRUE),
              Var = var(PriceByM2Construccion, na.rm=TRUE),
              SD = sd(PriceByM2Construccion, na.rm=TRUE),
              N = n()
    )
}


test <- getSummaryPriceInUSDByProvincia(realEstateDf)
test <- getSummaryPriceByM2ConstruccionByProvincia(analysisDf)

analysisDf <- prepareForAnalysis(realEstateDf)

