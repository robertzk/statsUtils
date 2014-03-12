#' @export
variable_summaries <- function(dataframe) {
  numeric_cols <- vapply(dataframe, is.numeric, logical(1))
  list(
     missing_percentages = vapply(dataframe, function(x) mean(is.na(x)), numeric(1))
   , means = vapply(dataframe[, numeric_cols], function(x) mean(x, na.rm = TRUE), numeric(1))
   , standard_deviations = vapply(dataframe[, numeric_cols], function(x) sd(x, na.rm = TRUE), numeric(1))
  )
}
