library(hash)

(function(){
  isGeneric <- function(x) methods::isGeneric(x) && exists(x)

  setClass('classHelper', representation(.DEBUG = 'logical', className = 'character'),
           prototype(.DEBUG = FALSE), extends('character')) # 'character' so not virtual class

  setMethod('initialize', 'classHelper', function(.Object, className, .DEBUG = FALSE) {
    .Object@className <- className
    .Object
  })

  if(!isGeneric('classHelper.defineMethod'))
    setGeneric('classHelper.defineMethod',
               function(object, methodName, method) standardGeneric('classHelper.defineMethod'))

  setMethod('classHelper.defineMethod', 'classHelper',
            function(object, methodName, method) {
    if(substr(methodName, nchar(methodName) - 1, nchar(methodName)) == '<-') replace <- TRUE
    else replace <- FALSE

    # Set up a generic first
    if(!isGeneric(methodName) && methodName != 'initialize') {
      # Determine argument list using some meta-programming
      formal_args <- formals(method)
      if(is.null(formal_args)) {
        stop(paste('Error classHelper.defineMethods: ',
                      'Problem defining method "', methodName, '" for class "',
                      object@className, '" because passed method function has no ',
                      'parameters, and all methods must take at least "object" as a ',
                      'parameter', sep = ''))
      }
      arguments <- names(formal_args)

      # We will now introduce a splat ("...") into the function arguments
      # so that the generic method we create will be as flexible as possible.

      # Remove splatter if already present, because we will creatively overwrite it
      # http://stackoverflow.com/questions/652136/how-can-i-remove-an-element-from-a-list
      if('...' %in% arguments) formal_args[['...']] <- NULL

      arguments <- gsub('= ([,)])', '\\1', substring(deparse(formal_args),5))
      # TODO: Check if ... already is present
      splatter <- ', ...'
      # TODO: This is wrong. Careful with arguments ending in: , value = c(blah, ...))
      if(replace) {
        if (length(formal_args) == 1) arguments <- paste(substr(arguments, 0, nchar(arguments) - 1), ", ...)")
        else arguments <- gsub("(.*)\\,","\\1, ...,", arguments)
      }
      else if (length(grep(',', arguments)) > 0)
        arguments <- paste(substring(arguments, 0, nchar(arguments)-1),
                           paste(splatter, ')', sep = ''), sep = '')

      if(object@.DEBUG) {
        print("Instantiating generic")
        print(methodName)
        print(arguments)
      }
    
      setGeneric(methodName, eval(parse(text = paste(
        'function', arguments, ' standardGeneric("', methodName, '")', sep = ''))))
    }

    # Now create the method
    if(replace) {
      # remove trailing <-
      methodName <- substr(methodName, 1, nchar(methodName)-2) 
      setReplaceMethod(methodName, object@className, method)
    } else setMethod(methodName, object@className, method)
  })

  if(!isGeneric('classHelper.defineMethods'))
    setGeneric('classHelper.defineMethods',
               function(object, methods) standardGeneric('classHelper.defineMethods'))

  # Takes a classHelper object, a class name, and a hash of methods indexed by method name
  # with values the actual lambda functions, and defines the methods with those names.
  setMethod('classHelper.defineMethods', 'classHelper', function(object, methods) {
    lapply(keys(methods), function(method)
      classHelper.defineMethod(object, method, methods[[method]][[1]]))
  })

  setClass('methodHelper',
           representation(
              .DEBUG = 'logical',
              className = 'character',
              classHelper = 'classHelper'
           ),
           prototype(.DEBUG = FALSE))

  setMethod('initialize', 'methodHelper', function(.Object, className, .DEBUG = FALSE) {
    .Object@className <- className
    .Object@.DEBUG <- .DEBUG
    .Object@classHelper <- new('classHelper', className = className, .DEBUG = .DEBUG)
    .Object
  })

  if(!isGeneric('defineMethod<-'))
    setGeneric('defineMethod<-',
               function(object, methodName, value) standardGeneric('defineMethod<-'))

  # This isn't *really* a replace method, but it helps with the notiation:
  # defineMethod(methodHelper, methodName) <- function() { ... }
  # looks nicer than
  # defineMethod(methodHelper, methodName, function() {
  #   ...
  # })
  # especially since the method function is probably many lines
  setReplaceMethod('defineMethod', 'methodHelper', function(object, methodName, value) {
    classHelper.defineMethod(object@classHelper, methodName, value)
    object
  })

  setClass('mixin',
           representation(.mixin_options = 'hash'),
           prototype(.mixin_options = hash()))

  methodHelper <- new('methodHelper', className = 'mixin')

  defineMethod(methodHelper, 'setMixinOptions<-') <-
    function(object, value) {
      if (!(class(value) %in% 'hash')) stop('setMixinOptions<- must take a hash')
      for(key in keys(value)) object@.mixin_options[[key]] <- value[[key]]
      object
    }

  defineMethod(methodHelper, 'getMixinOption') <-
    function(object, key, default = NULL) {
      retval <- object@.mixin_options[[key]][[1]]
      if (is.null(retval) && !is.null(default)) default
      else retval
    }

})()

