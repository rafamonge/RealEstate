#' camel
#'
#' converts text to camel case.
#' @param x 
#'
#' @return text converted to camel case
#' @export
#'
#' @examples
#' camel("hola mundo") -> "holaMundo"
camel <-
function(x){ #function for camel case and removing accents
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  temp<-sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=""))
  iconv(temp,from="UTF-8", to="ASCII//TRANSLIT")
}
