#' @export
sure_independence_screener <- function(dataframe) {
  sis <- new('driver.withSureIndependenceScreening', data = dataframe,
             filter_bad_levels = FALSE)
  sis@data <- dataframe
  sureIndependenceScreening(sis)@data
}
