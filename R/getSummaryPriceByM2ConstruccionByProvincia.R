#' getSummaryPriceByM2ConstruccionByProvincia
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
getSummaryPriceByM2ConstruccionByProvincia <-
function(data){
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
