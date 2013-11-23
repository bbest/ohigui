##' Ocean Health Index: Data file format
##' 
##' Simple read/write utility functions for the CSV and RData data file format.
##' 
##' 
##' @aliases ohi.read.csv ohi.write.csv ohi.load ohi.save ohi.loadbin 
##'   ohi.savebin ohi.savetxt ohi.save.pressures ohi.save.resilience 
##'   ohi.save.results ohi.save.status ohi.save.trend
##' @param file Full path to input/output file.
##' @param x A data frame with data to write.
##' @param digits Use to restrict ASCII representation of doubles.
##' @param row.names Do not use them by default
##' @param na.strings Use blanks for NA.
##' @param na Use blanks for NA.
##' @param dir The directory to use.
##' @param method The data file format method.
##' @param envir Environment in which to assign name
##' @param name The variable to which data will be assigned, and used for the 
##'   filename -- e.g., name = 'regions' will look for 'regions.csv', etc.
##' @param xdim The name of a dimension, and expects get(dimension) to return a 
##'   valid data frame with 2 columns: region_code, and dimension -- e.g., 
##'   ('region_code', 'status').
##' @param ... Arguments passed onto read.csv, write.csv, load, save.
##' @return Returns a data.frame with the input data.
##' @seealso \code{\link{read.csv}}, \code{\link{write.csv}}
##' @keywords ohi
##' @export ohi.read.csv ohi.write.csv ohi.load ohi.save ohi.loadbin ohi.savebin
##'   ohi.savetxt ohi.save.pressures ohi.save.resilience ohi.save.results
##'   ohi.save.status ohi.save.trend
##' @examples
##' 
##' \dontrun{
##'   d <- ohi.read.csv('data/regions.csv')
##'   names(d)
##'   head(d)
##'   
##'   d$label <- toupper(d$label)
##'   ohi.write.csv(d, 'data/regions.veryloud.csv')
##' 
##'   status <- data.frame(region_code=d$region_code, status=rnorm(nrow(d)))
##'   ohi.save.status()
##' }
ohi.read.csv <- function(file, na.strings='', row.names=NULL, ...) {
  read.csv(file, na.strings=na.strings, row.names=row.names, ...)
}

ohi.write.csv <- function(x, file, digits=NULL, row.names=F, na='', ...) {
  if (!is.data.frame(x)) {
    d <- as.data.frame(x)
  } else {
    d <- x # deep copy 
  }
  
  if (is.null(digits)) {
    digits <- getOption('digits', 12)
  }
  for (i in 1:ncol(d)) {
    if (typeof(d[,i]) == "double") {
      #
      # strip out any extra precision using round+signif
      #
      # > options(digits=16)
      # > x <- as.double(-0.123456789987654321e-16)
      # > x
      # [1] -1.234567899876543e-17
      # > signif(x, 15)
      # [1] -1.23456789987654e-17
      # > round(x, 15)
      # [1] 0
      # > round(signif(x, 15), 15)
      # [1] 0
      
      # > x <- as.double(0.123456789987654321)
      # > x
      # [1] 0.1234567899876543
      # > signif(x, 15)
      # [1] 0.123456789987654
      # > round(x, 15)
      # [1] 0.123456789987654
      # > round(signif(x, 15), 15)
      # [1] 0.123456789987654
      # > 
      
      d[,i] <- round(signif(d[,i], digits), digits) 
    }
  }
  # write clean CSV files such that real numbers x digits
  write.csv(d, file, na=na, row.names=row.names, ...)
}


# save data 
ohi.save <- function(name, dir='data', method=c('RData', 'csv'), ...) {
  if (getOption('debug', FALSE)) {
    stopifnot(is.character(name) && exists(name))
    stopifnot(is.vector(method))
    stopifnot(any(c('RData','csv') %in% method))
    stopifnot(file.exists(dir))
  }
  
  if ('RData' %in% method) {
    fn <- paste(file.path(dir, name), 'RData', sep='.')
    vcat('saving', name, 'as RData file', fn, '...\n')
    save(list=name, file=fn, ...)            
  }
  if ('csv' %in% method) {
    fn <- paste(file.path(dir, name), 'csv', sep='.')
    vcat('saving', name, 'as CSV file', fn, '...\n')
    ohi.write.csv(get(name), file=fn, ...)
  }
  invisible()
}

# save results in the form of (region_code, dimension) tuples
ohi.save.results <- function(xdim, dir) {
  stopifnot(is.character(xdim) && xdim %in% ohi.model.dimensions && exists(xdim))
  stopifnot(is.data.frame(get(xdim)))
  ohi.save(xdim, dir=dir, method='csv')
}

ohi.save.status <- function(dir='..') { ohi.save.results('status', dir) }
ohi.save.trend <- function(dir='..') { ohi.save.results('trend', dir) }
ohi.save.pressures <- function(dir='..') { ohi.save.results('pressures', dir) }
ohi.save.resilience <- function(dir='..') { ohi.save.results('resilience', dir) }

ohi.savebin <- function(name, dir='data', ...) {
  ohi.save(name, dir, method='RData', ...)
}

ohi.savetxt <- function(name, dir='data', ...) {
  ohi.save(name, dir, method='csv', ...)
}

# load data
ohi.load <- function(name, dir='data', method=c('RData', 'csv'), envir=.GlobalEnv) {
  if (getOption('debug', FALSE)) {
    stopifnot(is.character(name))
    stopifnot(is.vector(method))
    stopifnot(any(c('RData','csv') %in% method))
    stopifnot(file.exists(dir))
  }
  
  # load data using as many methods as provided until success
  for (m in method) {
    if ('RData' == m) {
      fn <- paste(file.path(dir, name), 'RData', sep='.')
      if (file.exists(fn)) {
        vcat('loading', name, 'from RData file', fn, '...\n')
        load(fn, envir=envir)
        invisible(name)
      }
    } else if ('csv' == m) {
      fn <- paste(file.path(dir, name), 'csv', sep='.')
      if (file.exists(fn)) {
        vcat('loading', name, 'from CSV file', fn, '...\n')
        assign(name, ohi.read.csv(fn), envir=envir)
        invisible(name)
      } else {
        warning(paste("File not found:", fn))
      }
    } else {
      warning(paste("Unknown load method:", m))
    }
    
  }
  invisible()
}

ohi.loadbin <- function(name, dir='data', envir=.GlobalEnv) {
  ohi.load(name, dir, method='RData', envir)
}
