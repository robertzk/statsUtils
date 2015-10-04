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

