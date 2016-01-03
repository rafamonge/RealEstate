#' prepareForAnalysis
#'
#' creates new columns useful to analyze data
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
prepareForAnalysis <-
function(data){
  data %>%
    mutate(PriceByM2Construccion = PriceInUSD / MtsConstruccion ) %>%
    ##filter( 300 <= PriceByM2Construccion & PriceByM2Construccion <=  1700           )
    group_by(Provincia)  %>%
    mutate(NormalizedPriceByM2Construccion = as.vector(scale(PriceByM2Construccion) ))
  
}
