##' Ocean Health Index: Goal Model
##' 
##' The goal model function.
##' 
##' 
##' @param id Region identifiers.
##' @param status Status scores.
##' @param trend Trend values for 5 year outlook.
##' @param resilience Resilience scores.
##' @param pressure Pressures scores.
##' @param DISCOUNT Discount multiplier.
##' @param BETA Multiplier used in likely future status calculation.
##' @param default.trend The default trend value (0) if region has NA.
##' @return Returns a data.frame with the input data, and a likely future
##' status and OHI score.
##' @keywords ohi
##' @examples
##' 
##' \dontrun{
##' ## run a model with 50 regions using random data,
##' ## using 5 year 1-percent discount rate and beta=0.67
##' require(ohi)
##' d <- ohi.model.goal(id=1:50, 
##'                     status=runif(50, 0, 1), 
##'                     trend=runif(50, -1, 1), 
##'                     resilience=runif(50, 0, 1), 
##'                     pressure=runif(50, 0, 1), 
##'                     DISCOUNT = (1 + 0.01)^-5,
##'                     BETA = 0.67,
##'                     default.trend = 0.0) 
##' ## view model output
##' names(d)
##' d[,c('id','score','xF')]
##' }
##' 
##' @export ohi.model.goal
ohi.model.goal <- function(id, status, trend, resilience, pressure, 
                           DISCOUNT = 1.0, BETA = 0.67, default.trend = 0.0) {
  #' Goal-level computation function to goal score ("component indicators for
  #' public goals") based on status, trend, resilience, pressure
  #'
  #' Parameters:
  #' @param id is the subregion identifier
     #' @param status (x)
     #' @param trend (t)
     #' @param resilience (r)
     #' @param pressure (p)
     #'
     #' Constants:
     #' @param DISCOUNT is the discount factor (i.e., df = 1 - rate)
     #' @param BETA is the trend dampening constant, aka "beta"
     #'
     #' Flags:
     #'
     #'
     #' Returns:
     #' @return data table with status (x), trend (t), resilience (r), 
     #' pressure (p), plus additional columns for future status (xF)
     #' and the goal score (score).
     
     # verify parameters
     if (getOption('debug', FALSE)) {
       stopifnot(BETA >= 0 && BETA <= 1)
       stopifnot(DISCOUNT >= 0)
     }
     
     # Simplify symbols based on math writeup
     d <- data.frame(id=id, x=status, t=trend, r=resilience, p=pressure)
     
     # replace a trend of NA with a default (0)
     if (!is.null(default.trend) && is.numeric(default.trend) && any(is.na(d$t))) {
       d$t[is.na(d$t)] <- default.trend
     }
     
     # enforce domains
     if (getOption('debug', FALSE)) {
       stopifnot(min(d$x, na.rm=T) >= 0  && max(d$x, na.rm=T) <= 1)   #  [0, 1]
       stopifnot(min(d$t, na.rm=T) >= -1 && max(d$t, na.rm=T) <= 1)   # [-1, 1]
       stopifnot(min(d$r, na.rm=T) >= 0  && max(d$r, na.rm=T) <= 1)   #  [0, 1]
       stopifnot(min(d$p, na.rm=T) >= 0  && max(d$p, na.rm=T) <= 1)   #  [0, 1]
     }
     
     # compute "future" status, using all dimensions
     d$xF <- with(d, (DISCOUNT * (1 + (BETA * t) + ((1-BETA) * (r - p)))) * x)
     # clamp status domain to [0, 1]
     d$xF <- with(d, score.clamp(xF))
     # compute score using formula for individual goal component indicator
     d$score <- with(d, (x + xF)/2)
     # return results
     d
}