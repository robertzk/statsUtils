prettySeq <- function (x) {
  paste("Resample", gsub(" ", "0", format(seq(along = x))), sep = "")
}

zeroVar <- function (x) {
    x <- x[, colnames(x) != ".outcome", drop = FALSE]
    which(apply(x, 2, function(x) length(unique(x)) < 2))
}

nzv <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE) {
    if (is.vector(x))
        x <- matrix(x, ncol = 1)
    freqRatio <- apply(x, 2, function(data) {
        t <- table(data[!is.na(data)])
        if (length(t) <= 1) {
            return(0)
        }
        w <- which.max(t)
        return(max(t, na.rm = TRUE)/max(t[-w], na.rm = TRUE))
    })
    lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
    percentUnique <- 100 * lunique/apply(x, 2, length)
    zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
    if (saveMetrics) {
        out <- data.frame(freqRatio = freqRatio, percentUnique = percentUnique,
            zeroVar = zeroVar, nzv = (freqRatio > freqCut & percentUnique <=
                uniqueCut) | zeroVar)
    }
    else {
        out <- which((freqRatio > freqCut & percentUnique <=
            uniqueCut) | zeroVar)
        names(out) <- NULL
    }
    out
}


#' Data Splitting Functions
#' A series of test/training partitions are created using
#'   'createDataPartition' while 'createResample' creates one or more
#'   bootstrap samples. 'createFolds' splits the data into 'k' groups
#'   while 'createTimeSlices' creates cross-validation sample
#'   information to be used with time series data.
#'
#'  Check out documentation for `caret::createFolds`
#'
#' @seealso \code{nearZeroVar} in the caret package.
#' @param y data.frame.
#' @param times integer.
#' @param p numeric.
#' @param list logical.
#' @param groups numeric.
#' @importFrom plyr dlply .
#' @export
createDataPartition <- function (y, times = 1, p = 0.5, list = TRUE, groups = min(5, length(y))) {
    out <- vector(mode = "list", times)
    if (length(y) < 2)
        stop("y must have at least 2 data points")
    if (groups < 2)
        groups <- 2
    if (is.numeric(y)) {
        y <- cut(y, unique(quantile(y, probs = seq(0, 1, length = groups))),
            include.lowest = TRUE)
    }
    else {
        xtab <- table(y)
        if (any(xtab == 0)) {
            warning(paste("Some classes have no records (", paste(names(xtab)[xtab ==
                0], sep = "", collapse = ", "), ") and these will be ignored"))
            y <- factor(as.character(y))
        }
        if (any(xtab == 1)) {
            warning(paste("Some classes have a single record (",
                paste(names(xtab)[xtab == 1], sep = "", collapse = ", "),
                ") and these will be selected for the sample"))
        }
    }
    subsample <- function(dat, p) {
        if (nrow(dat) == 1) {
            out <- dat$index
        }
        else {
            num <- ceiling(nrow(dat) * p)
            out <- sample(dat$index, size = num)
        }
        out
    }
    for (j in 1:times) {
        tmp <- plyr::dlply(data.frame(y = y, index = seq(along = y)),
            .(y), subsample, p = p)
        tmp <- sort(as.vector(unlist(tmp)))
        out[[j]] <- tmp
    }
    if (!list) {
        out <- matrix(unlist(out), ncol = times)
        colnames(out) <- prettySeq(1:ncol(out))
    }
    else {
        names(out) <- prettySeq(out)
    }
    out
}

#' Identification of near zero variance predictors
#'
#' @seealso \code{nearZeroVar} in the caret package.
#' @param x data.frame.
#' @param freqCut numeric.
#' @param uniqueCut integer.
#' @param saveMetrics logical.
#' @param foreach logical.
#' @param allowParallel logical.
#' @export
nearZeroVar <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, foreach = FALSE, allowParallel = TRUE) {
    if (!foreach)
        return(nzv(x, freqCut = freqCut, uniqueCut = uniqueCut,
            saveMetrics = saveMetrics))
    `%op%` <- getOper(foreach && allowParallel && getDoParWorkers() >
        1)
    if (saveMetrics) {
        res <- foreach(name = colnames(x), .combine = rbind) %op%
            {
                r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut,
                  saveMetrics = TRUE)
                r[, "column"] <- name
                r
            }
        res <- res[, c(5, 1, 2, 3, 4)]
        rownames(res) <- as.character(res$column)
        res$column <- NULL
    }
    else {
        res <- foreach(name = colnames(x), .combine = c) %op%
            {
                nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut,
                  saveMetrics = FALSE)
            }
    }
    res
}

#' Create cross validation folds.
#'
#' @param y vector. A vector of stuff.
#' @param k integer. Number of folds.
#' @param list logical. Whether or not to return a list.
#' @param returnTrain logical. Whether or not to return the train data.
#' @export
createFolds <-
  function(y, k = 10, list = TRUE, returnTrain = FALSE) {
    if(class(y)[1] == "Surv") y <- y[,"time"]
    if(is.numeric(y)) {
      ## Group the numeric data based on their magnitudes
      ## and sample within those groups.

      ## When the number of samples is low, we may have
      ## issues further slicing the numeric data into
      ## groups. The number of groups will depend on the
      ## ratio of the number of folds to the sample size.
      ## At most, we will use quantiles. If the sample
      ## is too small, we just do regular unstratified
      ## CV
      cuts <- floor(length(y)/k)
      if(cuts < 2) cuts <- 2
      if(cuts > 5) cuts <- 5
      breaks <- unique(quantile(y, probs = seq(0, 1, length = cuts)))
      y <- cut(y, breaks, include.lowest = TRUE)
    }

    if(k < length(y)) {
      ## reset levels so that the possible levels and
      ## the levels in the vector are the same
      y <- factor(as.character(y))
      numInClass <- table(y)
      foldVector <- vector(mode = "integer", length(y))

      ## For each class, balance the fold allocation as far
      ## as possible, then resample the remainder.
      ## The final assignment of folds is also randomized.
      for(i in 1:length(numInClass)) {
        ## create a vector of integers from 1:k as many times as possible without
        ## going over the number of samples in the class. Note that if the number
        ## of samples in a class is less than k, nothing is producd here.
        min_reps <- numInClass[i] %/% k
        if(min_reps > 0) {
          spares <- numInClass[i] %% k
          seqVector <- rep(1:k, min_reps)
          ## add enough random integers to get  length(seqVector) == numInClass[i]
          if(spares > 0) seqVector <- c(seqVector, sample(1:k, spares))
          ## shuffle the integers for fold assignment and assign to this classes's data
          foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
        } else {
          ## Here there are less records in the class than unique folds so
          ## randomly sprinkle them into folds.
          foldVector[which(y == names(numInClass)[i])] <- sample(1:k, size = numInClass[i])
        }
      }
    } else foldVector <- seq(along = y)

    if(list) {
      out <- split(seq(along = y), foldVector)
      names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), sep = "")
      if(returnTrain) out <- lapply(out, function(data, y) y[-data], y = seq(along = y))
    } else out <- foldVector
    out
  }

#' Create multi folds.
#'
#' @param y vector. A vector of stuff.
#' @param k integer. Number of folds.
#' @param times integer. Number of times.
#' @return The result of calling \code{\link{createFolds}} on each split.
#' @export
createMultiFolds <- function(y, k = 10, times = 5) {
  if(class(y)[1] == "Surv") y <- y[,"time"]
  prettyNums <- paste("Rep", gsub(" ", "0", format(1:times)), sep = "")
  for(i in 1:times) {
    tmp <- createFolds(y, k = k, list = TRUE, returnTrain = TRUE)
    names(tmp) <- paste("Fold",
                        gsub(" ", "0", format(seq(along = tmp))),
                        ".",
                        prettyNums[i],
                        sep = "")
    out <- if(i == 1) tmp else c(out, tmp)

  }
  out
}
