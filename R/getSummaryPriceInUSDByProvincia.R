#' getSummaryPriceInUSDByProvincia
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
getSummaryPriceInUSDByProvincia <-
function(data){
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
