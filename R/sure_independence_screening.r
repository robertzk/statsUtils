#' Sure independence screening helper.
#' TODO: Document better.
#' @export
sure_independence_screening <- function(dep_var, indep_var, cutoff = 0.05, freq_cutoff = 0.05, link = "logit", depth = 1) {
  if (length(uniques <- unique(indep_var)) <= 1 ||
      (length(uniques) == 2 && NA %in% uniques)) return(NULL)
  dataframe <- data.frame(dep_var, indep_var)
  regression <-
    tryCatch(glm(dep_var ~ indep_var,
        data = dataframe,
        na.action = "na.exclude",
        family = binomial(link = link)))
  if (inherits(regression, 'try-error')) return(NULL)
  coefs <- summary(regression)$coefficients
   
  worst_level_pval <- 0
  parsed_levels <- parse_regression_into_levels(coefs, column = indep_var,
                               variable = 'indep_var', indep_vars = 'indep_var',
                               active_vars = c('indep_var', 'dep_var'), reject_coef = cutoff)
  worst_level <- parsed_levels[['worst_level']]
  good_levels <- parsed_levels[['good_levels']]
  bad_levels <- parsed_levels[['bad_levels']]

  if ((sum(indep_var %in% good_levels) / nrow(dataframe)) < cutoff
      && length(bad_levels) < 2) return(NULL)
  if (length(bad_levels) == 0) return(indep_var)

  reduced_col <- as.character(indep_var)
  factors <- as.character(indep_var)
  factors[factors == worst_level] <- 'BAD'
  
  if (length(good_levels) == 0 && length(bad_levels) == 1) {
    if (length(factors) > 1 && factors[1] != 'BAD') return(indep_var)
    else return(NULL)
  }
  
  potential_levels <- union(c('BAD'), setdiff(levels(indep_var), worst_level))
  factors <- factor(factors, levels = potential_levels)
  
  # re-run regression if more than one bad level
  if(length(bad_levels) > 1) {
    return(sure_independence_screening(
      dep_var, factors, cutoff, freq_cutoff, link, depth = depth + 1))
  }
  
  factors
}

