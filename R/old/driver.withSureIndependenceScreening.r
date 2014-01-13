
# Mixin for drivers to set up crossfold validation
(function() {
  .DEBUG <- TRUE
  .className <- 'driver.withSureIndependenceScreening'
  methodHelper <- new('methodHelper', className = .className)
  .PARALLELIZATION_ENABLED <- FALSE
  apply_method <- if(.PARALLELIZATION_ENABLED) { library(parallel); mclapply } else sapply

  setClass(.className,
           representation(original_data = 'data.frame',
                          filter_bad_levels = 'logical'),
           prototype(filter_bad_levels = FALSE),
           contains = 'driver')

  defineMethod(methodHelper, 'initialize') <-
    function(.Object, data, filter_bad_levels = FALSE, ...) {
      .Object@filter_bad_levels = filter_bad_levels
      .Object
    }

  defineMethod(methodHelper, 'sureIndependenceScreening') <-
    function(object, variables = colnames(object@data), force_recache = FALSE,
             dep_var = 'dep_var', silent = FALSE) {
     if(class(variables) == 'numeric') variables <- colnames(object@data)[1:variables]
     
     sisFilter <- new('hiloPass.logregFilter', variable = NULL,
                     dep_var = 'dep_var', indep_vars = NULL, silent = silent)
     
     # TODO: SOOOO BAD, DONT EVER DO THIS
     #s3path <- paste('s3://avantminer/', 'drivers/', .className, '/', sep = '')

     #data_hash <- digest(as.matrix(object@data))
     #cache_path <- paste(s3path, 'data/', data_hash, '.rds', sep = '')
     #cache_attempt <- try(cache <- s3.get(cache_path))
     #valid_cache <- !force_recache && class(cache_attempt) != 'try-error' && class(cache) == 'data.frame'
     #vars_cache_path <- paste(s3path, 'originalVariables/', data_hash, '.rds', sep = '')
     #if (valid_cache) {
     #  cached_vars <- s3.get(vars_cache_path)
     #  if (is.data.frame(cached_vars)) cached_vars <- cached_vars$original_vars
     #  screen_colnames <- variables[!(variables %in% cached_vars)]
     #}
     #else {
     screen_colnames <- variables
     #  cached_vars <- c()
     #}
     original_vars <- variables

     if (!silent) print('Beginning SIS...')
     result <- apply_method(screen_colnames, function(colname) {
       localFilter <- sisFilter

       col <- object@data[,colname]
       if (colname == 'dep_var') return(object@data[[colname]]) # don't screen dependent variable!
       if (length(unique(object@data[[colname]])) == 1) {
         return(NULL)
       }
       setVariable(localFilter) <- colname
       setIndepVars(localFilter) <- c(colname)
       # TODO: Do SIS on validation buckets. Now! THINK! WHAT DO WE DO? (to: Tong, from: Tong)
       filter_out(localFilter, object@data[,c(colname, 'dep_var')])
     })
     print('Saving SIS results...')
     #if (object@filter_bad_levels) object@data[, screen_colnames] <- result
     #else object@data[, screen_colnames] <- result[!sapply(result, is.null)]
     for(i in 1:length(result)) {
      if (is.null(result[[i]]) || object@filter_bad_levels)
          object@data[screen_colnames[[i]]] <- result[[i]]
     }
     #result <- result[unlist(lapply(result, function(col) { !is.null(col) }))]
     #for (varname %in screen_colnames) if(
     #object@data[screen_colnames] <- as.data.frame(result)
     #colnames(object@data[screen_colnames]) <- screen_colnames

     #if (valid_cache) {
     #   cached_variables <- original_vars[!(original_vars %in% screen_colnames)]
     #   to_replace <- cached_variables %in% colnames(cache)
     #   cached_replace_variables <- cached_variables[to_replace]
     #   cached_remove_variables <- cached_variables[!to_replace]
     #   object@data[cached_replace_variables] <- cache[cached_replace_variables]
     #   object@data <- object@data[!(colnames(object@data) %in% cached_remove_variables)]
     #}

     #if (!valid_cache || length(cache) != length(object@data)) {
     #   if (!silent) print('Uploading to s3: ')
     #   s3.put(union(colnames(object@data), cached_vars), vars_cache_path)
     #  if (!silent) print(paste('Variables stored at: ', vars_cache_path))
     #   s3.put(as.matrix(object@data), cache_path)
     #   if (!silent) print(paste('Data stored at: ', cache_path))
     #}

     if (!silent) print(paste(as.character(abs(length(original_vars) - length(object@data))), 'variables removed'))
     if (!silent) print(paste('New dimensions: ', dim(object@data)))
     # We genuinely no longer care about the SIS-filtered variables
     object@original_data <- object@data
     object
  }

})()
