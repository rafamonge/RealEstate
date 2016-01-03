#' save
#'
#' Save dataframe as xlsx file.
#' @param df 
#' @param outputFile 
#'
#' @return
#' @export
#'
#' @examples
save <-
function(df, outputFile="realEstateDf.xlsx" ){
  wb <- createWorkbook()
  addWorksheet(wb, "realEstateDf")
  writeDataTable(wb, sheet = 1, x = df)
  setColWidths(wb, sheet = 1, cols=1:ncol(df), widths = "auto")
  saveWorkbook(wb, outputFile, overwrite=T)
}
