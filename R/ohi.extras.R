##' Ocean Health Index: TBD
##' 
##' TBD
##' 
##' 
##' @aliases apply.bycol apply.byrow ends.with mangle ohi.find.file
##' ohi.model.keys ohi.options ohi.write.results pkey scenarios.cases
##' scenarios.compare scenario.to.beta starts.with vcat zstats.load md.table
##' ohi.old.goal.component ohi.goal.to.old.goal.component
##' ohi.old.goal.component.to.goal
##' @references tbd
##' @source tbd
##' @include ohi-vars.R
##' @examples
##' Sys.info()
##' @name ohi.extras
if (getOption('ohi.extras', TRUE)) {
  # backward compatability
  goals <- ohi.goal.all
  goals_subgoals <- ohi.goal.subgoal.all
  goal_subgoals <- ohi.goal.subgoal.unique
  ohi.global.regions.noATA <- ohi.global.regions.eez.noATA
  ohi.global.regions <- ohi.global.regions.eez
  schemes <- ohi.valuesets
  subgoals <- ohi.subgoal.all
  ohi.casestudies <- c('CC'='California Current', 'BR'='Brazil', 'MB'='Mid-Atlantic Bight', 'GL'='Global', 'FJ'='Fiji')
  
  ###
  # utility functions
  vcat <- function(...) {
    if (getOption('verbose', FALSE) || getOption('debug', FALSE)) {
      cat(...)
    }
  }
  
  pkey <- function(..., sep='_') { 
    # make a composite key using character strings
    paste(..., sep=sep) 
  }
  
  mangle <- function(s, sep='') {
    # convert text into legal identifier
    # stopifnot(mode(s) == 'character' && all(nchar(s)>0))
    gsub('[.]+', sep, make.names(s, allow_ = FALSE))
  }
  
  starts.with <- function(x, s) { substr(s, 1, nchar(x)) == x }
  ends.with <- function(x, s) { substr(s, nchar(s)-nchar(x)+1, nchar(s)) == x }
  
  BY.ROW <- 1; BY.COL <- 2
  
  apply.byrow <- function(x, f, ...) { apply(x, BY.ROW, f, ...) }
  apply.bycol <- function(x, f, ...) { apply(x, BY.COL, f, ...) }
  
  
  ###
  # save results in the form of (id, dimension(s)) tuples
  ohi.write.results <- function(x, dir='.', scope='eez', ...) {
    stopifnot(is.data.frame(x))
    results <- x
    names(results) <- tolower(names(results))
    
    stopifnot('id' %in% tolower(names(results)))
    results <- allregions(results, scope)
    
    stopifnot(any(c('value', ohi.model.dimensions) %in% names(results)))
    ohi.write.csv(results, file.path(dir, '_results_by_region.csv'), ...)
  }
  
  ###
  # locate a file in using fs keys
  ohi.find.file <- function(name, type = 'stable', ext = '') {
    fn <- ifelse(nchar(ext) > 0, paste(name, ext, sep='.'), name)
    p <- as.character(
      switch (type,
              model = Sys.getenv("OHI_MODELDIR"),
              stable = Sys.getenv("OHI_STABLEDIR"),
              ingest = Sys.getenv("OHI_INGESTDIR"),
              raw = Sys.getenv("OHI_RAWDIR"),
              tmp = Sys.getenv("OHI_RUNDIR"),
              lib = Sys.getenv("OHI_DISK_LIB")
      )
    )
    file.path(p, fn)
  }
  
  ###
  # scenario runs where "A" decreases and "B" increases, 
  # or vice versa with inverse flag
  #
  # scenario.to.beta() >- 1.0
  # scenario.to.beta('A10') -> 0.9
  # scenario.to.beta('B10') -> 1.1
  # scenario.to.beta('B10', trend=T) -> 1.02
  # scenario.to.beta('A10', inverse=T) -> 1.1
  # scenario.to.beta('B10', inverse=T) -> 0.9
  # scenario.to.beta('B10', inverse=T, trend=T) -> 0.98
  # scenario.to.beta('B300') -> 4.0
  #
  scenario.to.beta <- function(s = NA, inverse=F, trend=F) {
    if (is.na(s) || nchar(s) < 2) {
      1.0
    } else {
      x <- as.numeric(substring(s, 2))/100.0 # X50 -> 0.5
      if (trend) {
        x <- x/5
      }
      if (toupper(substring(s, 1, 1)) == 'A') {
        if (inverse) { 1.0 + x } else { 1.0 - x }
      } else {
        if (inverse) { 1.0 - x } else { 1.0 + x }
      }
    }
  }
  
  # build out scenario cases
  scenarios.cases <- function(k, scenarios=c(NA, 'A', 'B', 'C', 'D', 'E')) {
    # scenarios must have a default (NA)
    stopifnot(sum(is.na(scenarios)) == 1)
    
    cases <- data.frame(scenario=scenarios)
    cases$path[is.na(scenarios)] <- 'cases/default'
    cases$path[!is.na(scenarios)] <- 
      paste('cases/Scenario_', 
            cases$scenario[!is.na(scenarios)], sep='')
    cases
  }
  
  # generate delta of global scores between default and new case
  scenarios.compare <- function(cases, i, out.dir='.', f.diff=NULL) {
    if (is.null(f.diff)) {
      f.diff <- function(a, b) { 
        ifelse(a==0 & b!=0,Inf,((b - a) / a))
      }
    }
    
    # read data
    scorefn <- 'results_global_scores.csv'
    dir.1 <- cases$path[is.na(cases$scenario)]
    dir.2 <- cases$path[[i]]
    fn.1 <- file.path(dir.1, scorefn)
    fn.2 <- file.path(dir.2, scorefn)
    d.1 <- ohi.read.csv(fn.1)
    d.2 <- ohi.read.csv(fn.2)
    
    # load into matrix
    goals <- names(d.1)[-(1:3)]
    m.1 <- as.matrix(d.1[,goals])
    m.2 <- as.matrix(d.2[,goals])
    
    # run comparison
    m.diff <- f.diff(m.1, m.2)
    
    # write results
    d.diff <- as.data.frame(cbind(d.1[,1:3], m.diff))
    fn.diff <- paste(basename(dir.2), '.csv', sep='')
    write.csv(d.diff, file.path(out.dir, fn.diff), row.names=F)
    fn.diff
  }
  
  ###
  # load zonal stats data from either GRASS or ArcGIS format
  zstats.load <- function(fn, csv.format='GRASS', extract='mean') {
    stopifnot(any(c('GRASS','ArcGIS') %in% c(csv.format)))
    
    if (csv.format == 'GRASS') {
      d <- ohi.read.csv(fn, na.strings=c('*','-nan'))
      names(d) <- tolower(names(d))
      r <- data.frame(id=d[,'zone'], value=d[,tolower(extract)], n=d[,'non_null_cells'])
    } else if (csv.format == 'ArcGIS') {
      d <- ohi.read.csv(fn, na.strings='')
      names(d) <- tolower(names(d))
      r <- data.frame(id=d[,'value'], value=d[,extract], n=d[,'count'])
    }
    invisible(return(r))
  }
  
}