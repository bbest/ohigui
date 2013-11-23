##' Ocean Health Index: Global regions
##' 
##' Functions to join with global regions
##' 
##' 
##' @aliases allregions ohi.global.regions.all ohi.global.regions.eez
##' ohi.global.regions.highseas
##' @param d The data frame to join with. If NULL, returns all regions.
##' @param scope 'all' for all global regions, or 'eez' for EEZ/country
##' regions.
##' @return Returns a data.frame containing a left join of regions.
##' @seealso merge
##' @keywords ohi
##' @export allregions
##' @include ohi.file.R
##' @examples
##' d <- data.frame(id=1:50, status=runif(50, 0, 1))
##' allregions(d) # returns status=NA for all but the first 50 regions
##' ohi.load('regions_details')
allregions <- function(d=NULL, scope='all') {
  region.ids <- switch(scope,
                       all=ohi.global.regions.all,
                       eez=ohi.global.regions.eez,
                       eez.noATA=ohi.global.regions.eez.noATA,
                       highseas=ohi.global.regions.highseas)
  
  stopifnot(is.null(region.ids) == FALSE)
  
  # left join with every region
  if (is.null(d)) {
    data.frame(id=region.ids)
  } else {
    merge(allregions(NULL, scope), d, all.x=T)
  }
}
