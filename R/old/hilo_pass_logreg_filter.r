(function() {
  # Hi-lo pass logistic regression variable filter
  library(hash)
  setClassUnion('characterOrNull', c('character', 'NULL'))

  .DEBUG <- TRUE
  .className <- 'hiloPass.logregFilter'
  methodHelper <- new('methodHelper', className = .className)

  setClass(.className, representation(
    variable = 'characterOrNull',
    indep_vars = 'characterOrNull',
    dep_var = 'characterOrNull',
    formula = 'formula',
    with_replacement = 'logical',
    frequency_cutoff = 'numeric',
    p_val_admit_cutoff = 'numeric',
    p_val_flunk_cutoff = 'numeric',
    flunk_grace_period = 'numeric',

    silent = 'logical'
  ), prototype(
    frequency_cutoff = 0.05,
    p_val_admit_cutoff = 0.05,
    p_val_flunk_cutoff = 0.07,
    flunk_grace_period = 0,
    with_replacement = TRUE,
    silent = FALSE
  ), contains = 'filter')  

  defineMethod(methodHelper, 'initialize') <- function(.Object,
      variable, indep_vars = NULL, formula = as.formula(NULL), silent = FALSE, ...) {
    .Object@dep_var <- 'dep_var'
    # if(is.null(indep_vars)) indep_vars <- c(variable)
    #.Object <- callNextMethod(.Object, ..., dep_var = dep_var, variable = variable,
    #                          indep_vars = indep_vars, formula = formula)
    .Object@variable <- variable
    .Object@indep_vars <- indep_vars
    if (length(formula) == 0) {
      setFormula(.Object) <- NULL
    }
    .Object@silent <- silent
    .Object
  }

  # Grab level name from a row name in a regression coefficients output
  # For example, filter_level_name("inquiries[0,4)", "inquiries",
  # c("inquiries", "score"), c("[0,4)", "[4, 10)")) returns '[0,4)'
  .filter_level_name <- function(level_name, correct_varname, vars, level_names) {
    for(varname in vars) {
      if (substr(level_name, 0, nchar(varname)) == varname) {
        if (!(substr(level_name, nchar(varname) + 1, nchar(level_name)) %in% level_names)) next
        if (varname != correct_varname) return(FALSE)
        return(substr(level_name, nchar(varname) + 1, nchar(level_name)))
      }
    }
    FALSE
  }

  # Mutators for active (to-be-filtered) variable and independent_variables
  defineMethod(methodHelper, 'setVariable<-') <- function(object, value) {
    object@variable <- value
    setFormula(object) <- NULL 
    object
  }

  defineMethod(methodHelper, 'setIndepVars<-') <- function(object, value) {
    object@indep_vars <- value
    setFormula(object) <- NULL 
    object
  }

  defineMethod(methodHelper, 'setFormula<-') <- function(object, value) {
    object@formula <- generate_formula(object@dep_var,
                                       union(object@indep_vars, object@variable))
    object
  }

  # Filter variable by naive p-test filtering
  # Returns the filtered column, or NULL if the variable is insignificant
  defineMethod(methodHelper, 'filter_out') <-
    function(object, data, depth = 1, reject_coef = object@p_val_admit_cutoff) {
      if (is.factor(col) && nlevels(col) == 1) {
        if(!object@silent) print(paste("BAD (only 1 level): ", object@variable) )
        return(NULL)
      }

      col <- data[[object@variable]]

      # TODO : Figure out a way to do this later
      active_vars <- union(object@variable, object@indep_vars)
      active_vars <- sapply(active_vars, function(v) {
        if (is.factor(data[[v]])) v else paste("ns(", v, ", df = 3)", sep = '')
      })

      temp_formula <- generate_formula(object@dep_var, active_vars)

      err <- try(regression <- logistic_regression(temp_formula, data))
      # TODO: Rethink this. Right now it dies on really crappy tailed variables
      if (length(err) == 1 && class(err) == 'try-error') return(NULL)
      coefs <- summary(regression)$coef # coefficients table from the regression
      good_levels <- c()
      bad_levels <- c()
        
      # Now cycle through all the levels and throw them into good or bad depending on the P_VALUE_CUTOFF
      worst_level <- ''
      worst_level_pval <- 0
      if (is.factor(col) || 'ns' %in% class(col) || TRUE) { # REALLY BAD: || true b/c we use in-line splines, remove later 
        parsed_levels <- parse_regression_into_levels(regression_coefficients = coefs, column = col,
                                     variable = object@variable, indep_vars = object@indep_vars,
                                     active_vars =active_vars, reject_coef = reject_coef)
        worst_level <- parsed_levels[['worst_level']]
        good_levels <- parsed_levels[['good_levels']]
        bad_levels <- parsed_levels[['bad_levels']]
      } else { # if(!is.factor(col)
        p_value <- coefs[object@variable,4]
        if (p_value < reject_coef) {
          if(!object@silent) print(paste("GOOD (continuous): ", object@variable))
          return(col)
        } else {
          if(!object@silent) print(paste("BAD (continuous): ", object@variable))
          return(NULL)
        }
      }

      if ('ns' %in% class(col) || !is.factor(col)) { # second expr b/c of in-line splines. TODO FIX LATER
        if (length(good_levels) == 0) {
          # Spline with no good levels
          if(!object@silent) print(paste("BAD (spline, no good levels): ", object@variable))
          return(NULL)
        } else {
          # Spline with at least one good level
          if(!object@silent) print(paste("GOOD (spline, >= 1 good level): ", object@variable))
          attr(col, 'good_levels') <- good_levels
          return(col)
        }
      }

      if ((sum(col %in% good_levels) / nrow(data)) < object@frequency_cutoff
          && length(bad_levels) < 2) {
        if(!object@silent) print(paste("BAD DUE TO FREQUENCY: ", object@variable))
        return(NULL)
      }

      
      if (length(bad_levels) == 0) {
        if(!object@silent) print(paste("GOOD (no bad levels): ", object@variable))
        return(col)
      }
      
      reduced_col <- sapply(col, function(el) { return(as.character(el)) } )
      # remove just the worst level for now, so we can re-run regression
      factors <- ifelse(reduced_col != worst_level, reduced_col, 'BAD')
      
      if (length(good_levels) == 0 && length(bad_levels) == 1) {
        if (length(factors) > 1 && factors[1] != 'BAD') {
          if(!object@silent) print(paste('GOOD (only one level remaining): ', object@variable))
          return(col)
        }
        if(!object@silent) print(paste('BAD DUE TO PVALUES: ', object@variable, ' '))
        return(NULL)
      }
      
      potential_levels <- union(c('BAD'), levels(col)[levels(col) != worst_level])
      factors <- factor(factors, levels = potential_levels)
      data[[object@variable]] <- factors
      
      # re-run regression if more than one bad level
      if(length(bad_levels) > 1) {
        return(filter_out(object, data = data, depth = depth + 1))
      }
      
      if(!object@silent) print(paste("GOOD (", depth, " bads removed): ", object@variable))
      data[[object@variable]]
    }

  defineMethod(methodHelper, 'flunk') <- function(object, data) {
    filter_out(object, data, depth = 1, reject_coef = object@p_val_flunk_cutoff)
  }

  defineMethod(methodHelper, 'setFrequencyCutoff<-') <- function(object, value) {
    object@frequency_cutoff <- value
    object
  }

  # parameter "value" should be of class 'filter'
  #.set(.methods, .filter_data = function(object, value) {
  #  object@data[[object@variable]] <- filter_out(object, value)
  #  object
  # })
})()

