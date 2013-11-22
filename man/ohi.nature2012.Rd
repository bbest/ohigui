\name{ohi.nature2012}
\alias{ohi.nature2012}
\docType{data}
\title{Ocean Health Index: Global Results}
\description{
This data set contains the scores and model outputs from the Ocean Health Index global study (Halpern et al. 2012).
}
\usage{ohi.nature2012}
\format{A list containing:\enumerate{
  \item \code{regions} is a data frame with the id and label for each reporting region (country EEZs).
  \item \code{global} is a data frame with area-weighted and unweighted global OHI scores, including per-goal scores.
  \item \code{valueset} is a data frame with the 5 value set (weighting combinations) scores per region.
  \item \code{goal} is an array of scores as \emph{goal ~ dimension ~ region}.
  \item \code{labels} is a list to map codes into text labels.
  \item \code{layers} is a list of layers data from the file archive.
  \item \code{countries} is a list of country-level data to aggregate into regions.
}
}
\examples{
options(max.print=10)
options(digits=3)
data(ohi.nature2012, package='ohigui')

## Find the Region ID number for the United States
region_id <- with(ohi.nature2012$regions, id[label == "United States"])
region_id

## Show the Artisanal Fishing Opportunities goal score
with(ohi.nature2012, goal['AO', 'score', region_id])

## Show the status and trend for the Food Provision: Fisheries (FIS) subgoal 
with(ohi.nature2012, goal['FIS', c('status','trend'), region_id])

## Show all goals and dimensions
with(ohi.nature2012, goal[ , , region_id])

## Show the unweighted OHI score for Region 116
subset(ohi.nature2012[['valueset']], id == region_id)[['unweighted']]

## Show the attributes for Region 116
subset(ohi.nature2012[['regions']], id == region_id)

## Show all countries part of Region 116
subset(ohi.nature2012[['countries']][['regions']], region_id == region_id)

## Show data for CP Extent layer for Region 116
subset(ohi.nature2012[['layers']][['data']], 
       layer_id == 'i_cp_extent' & region_id == region_id)
}

\references{
Halpern, BS, C Longo, D Hardy, KL McLeod, JF Samhouri,  SK Katona, K Kleisner, SE Lester,  J O'Leary, M Ranelletti, AA Rosenberg, C Scarborough, LR Selig, BD Best, DR Brumbaugh, FS Chapin III, LB Crowder, KL Daly, SC Doney, C Elfes, MJ Fogarty, SD Gaines, K Jacobsen, LB Karrer, HM Leslie, E Neeley, D Pauly, S Polasky, B Ris, K St. Martin, GS Stone, UR Sumaila, and D Zeller. 2012. \emph{An index to assess the health and benefits of the global ocean}. \bold{Nature} 488, 615--620 (30 August 2012). \href{http://dx.doi.org/10.1038/nature11397}{doi:10.1038/nature11397}.
}
\seealso{\href{http://ohi.nceas.ucsb.edu/data}{File archive}}
\keyword{datasets}
