##' Ocean Health Index: Resilience Model
##' 
##' The resilience model function computes a resilience score for each region 
##' given a weighting matrix for a goal and the individual resilience values.
##' 
##' To calculate Resilience for each goal \emph{g} and region \emph{i} 
##' (\eqn{r(g, i)}) we assess three types of resilience measures \emph{j}: 
##' ecological integrity (\eqn{Y_{E}(g, i)}), goal-specific regulations aimed at
##' addressing ecological pressures (\eqn{G(g, i)}), and social integrity 
##' (\eqn{Y_{S}(g, i)}). The first two measures address ecological resilience 
##' while the third addresses social resilience. When all three aspects are 
##' relevant to a goal, Resilience is calculated for each goal \emph{g} and each
##' region \emph{i}:
##' 
##' \deqn{r(g, i) = \gamma*\left(\frac{Y_{E}(g, i) + G(g, i)}{2}\right) + (1 - 
##' \gamma)*Y_{S}(g, i)}
##' 
##' where each goal \emph{g} is comprised of several resilience layers \emph{j} 
##' where \eqn{w_j} is a configuration-time weight to aggregate across 
##' resilience categories:
##' 
##' \deqn{G(g, i) = \frac{\sum_{j \in g} w_j G(i, j)}{\sum_{j \in g} w_j}} 
##' \deqn{Y_{E}(g, i) = \frac{\sum_{j \in g} Y_{E}(i,j)}{N}} \deqn{Y_{S}(g, i) =
##' \frac{\sum_{j \in g} Y_{S}(i,j)}{N}}
##' 
##' @aliases ohi.model.resilience ohi.model.resilience.matrix 
##'   ohi.resilience.category
##' @param r the resilience value matrix \code{[region_id x layer]}. Each score 
##'   must be a real number between 0 and 1 inclusive, or NA.
##' @param t the typing vector t[layer] where values are from 
##'   \code{ohi.resilience.category}.
##' @param w the weighting matrix of the form \code{[region_id x layer]}. Each 
##'   rank weight must be a real number >= 0, or NA for even weighting.
##' @param w.layers the weighting vector of the form \code{[layer]}. Each rank 
##'   weight must be a real number >= 0, or NA for even weighting.
##' @param gamma the gamma constant for \eqn{r_{i,x}} calculation.
##' @param b a boolean value matrix \code{[region_id x layer]} which is 
##'   \code{TRUE} if the given region_id should include layer, and \code{FALSE} 
##'   otherwise.
##' @return \code{ohi.model.resilience} returns resilience score for each 
##'   region. \code{ohi.model.resilience.matrix} returns a weighting matrix 
##'   suitable for \code{ohi.model.resilience}.
##' @keywords ohi
##' @examples
##' \dontrun{
##' > ohi.resilience.category
##' [1] "environmental" "regulatory"    "social"
##' > b
##'          layer
##' region_id fishing-v1 habitat-combo species-diversity-3nm wgi-all
##'       104       TRUE          TRUE                  TRUE    TRUE
##'       105       TRUE          TRUE                  TRUE    TRUE
##'       106       TRUE          TRUE                  TRUE    TRUE
##'       107       TRUE          TRUE                  TRUE    TRUE
##'       108       TRUE          TRUE                  TRUE    TRUE
##'       109       TRUE          TRUE                  TRUE    TRUE
##'       110       TRUE          TRUE                  TRUE    TRUE
##'       111       TRUE          TRUE                  TRUE    TRUE
##'       112       TRUE          TRUE                  TRUE    TRUE
##'       113       TRUE          TRUE                  TRUE    TRUE
##'       114       TRUE          TRUE                  TRUE    TRUE
##' > w
##'            fishing-v1         habitat-combo species-diversity-3nm 
##'                     2                     2                     1 
##'               wgi-all 
##'                     1 
##' > w < -ohi.model.resilience.matrix(b, w)
##' > w
##'          layer
##' region_id fishing-v1 habitat-combo species-diversity-3nm wgi-all
##'       104          2             2                     1       1
##'       105          2             2                     1       1
##'       106          2             2                     1       1
##'       107          2             2                     1       1
##'       108          2             2                     1       1
##'       109          2             2                     1       1
##'       110          2             2                     1       1
##'       111          2             2                     1       1
##'       112          2             2                     1       1
##'       113          2             2                     1       1
##'       114          2             2                     1       1
##' 
##' > r
##'          layer
##' region_id fishing-v1 habitat-combo species-diversity-3nm wgi-all
##'       104     0.4870        0.4495                0.8679  0.4385
##'       105     0.5162        0.5905                0.8748  0.2460
##'       106     0.4811        0.4051                0.8852  0.6465
##'       107     0.3618        0.2583                0.8260  0.8007
##'       108     0.5322        0.4703                0.9318  0.5579
##'       109     0.5053        0.4703                0.9313  0.5579
##'       110     0.6491        0.5690                0.9239  0.5703
##'       111     0.3629        0.1562                0.9230  0.6375
##'       112     0.5670        0.5000                0.9273  0.5718
##'       113     0.3807        0.2530                0.9339  0.4484
##'       114     0.6508        0.5690                0.9275  0.5703
##' > t
##'            fishing-v1         habitat-combo species-diversity-3nm 
##'          "regulatory"          "regulatory"       "environmental" 
##'               wgi-all 
##'              "social" 
##' 
##' > ohi.model.resilience(r, t, w)
##'    104    105    106    107    108    109    110    111    112    113 
##' 0.5533 0.4800 0.6553 0.6844 0.6372 0.6337 0.6684 0.6144 0.6511 0.5369 
##'    114 
##' 0.6695 
##' }
##' 
##' @export ohi.model.resilience ohi.model.resilience.matrix
##'   ohi.resilience.category
ohi.model.resilience <- function(r, t, w=NA, gamma=0.5) {
  stopifnot(all(is.matrix(r), is.vector(t)))
  stopifnot(all(t %in% ohi.resilience.category))
  stopifnot(all(names(dimnames(r)) == c('region_id', 'layer')))    
  stopifnot(all(dimnames(r)$layer %in% names(t)))
  
  if (missing(w)) {
    w <- rep(1, dim(r)[[2]])
    names(w) <- dimnames(r)$layer
    w <- ohi.model.resilience.matrix(!is.na(r), w)
  } else {
    stopifnot(is.matrix(w))
    stopifnot(all(names(dimnames(w)) == c('region_id', 'layer')))
  }
  
  # verify parameters
  if (getOption('debug', FALSE)) {
    stopifnot(min(r, na.rm=T) >= 0  && max(r, na.rm=T) <= 1)   #  [0, 1]
    stopifnot(min(w, na.rm=T) >= 0  && max(w, na.rm=T) <= 2)   #  [0, 2]
  }
  
  # normalize dimension handles
  stopifnot(all(dimnames(w)$layer %in% dimnames(r)$layer))
  
  # align
  t <- t[dimnames(r)$layer]
  w <- w[dimnames(r)$region_id, dimnames(r)$layer, drop=F]
  stopifnot(all(dimnames(r)$layer == dimnames(w)$layer))
  stopifnot(all(dimnames(r)$layer == names(t)))
  
  # compute by category
  for (k in ohi.resilience.category) {
    l <- paste('r', k, sep='_')
    if (k %in% t) {
      l.r <- r[,names(t)[t == k], drop=F]
      l.mask <- ifelse(!is.na(l.r), 1, NA)
      l.w <- w[,dimnames(l.r)$layer, drop=F]
      l.score <- apply.byrow(l.r*l.w, sum, na.rm=T) / apply.byrow(l.mask*l.w, sum, na.rm=T)
      assign(l, l.score)
    } else {
      assign(l, rep(NA, nrow(r)))
    }
  }
  
  # compute
  scores.e <- apply.byrow(cbind(get('r_environmental'), get('r_regulatory')), mean, na.rm=T)
  scores.s <- get('r_social')
  scores <- apply.byrow(cbind(scores.e, scores.s), weighted.mean, w=c(gamma,1-gamma), na.rm=T)
  names(scores) <- dimnames(r)$region_id
  scores
}

ohi.model.resilience.matrix <- function(b, w.layers=NA) {
  stopifnot(all(names(dimnames(b)) == c('region_id', 'layer')))
  if (missing(w.layers)) {
    w.layers <- rep(1, dim(b)[[2]])
    names(w.layers) <- dimnames(b)$layer
  } else {
    stopifnot(is.vector(w.layers))
  }
  stopifnot(all(dimnames(b)$layer %in% names(w.layers)))
  
  # calculate to preserve dimensionality and NoData values
  ifelse(b,1,NA) * matrix(w.layers[dimnames(b)$layer], nrow=nrow(b), ncol=ncol(b), byrow=T)
}

ohi.resilience.category <- c('environmental', 'regulatory', 'social')
