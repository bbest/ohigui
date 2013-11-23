##' Ocean Health Index: Global variables
##' 
##' Various variable constants for goals, dimensions, labels, etc.
##' 
##' 
##' @name ohi-vars
##' @aliases goals goals_subgoals goal_subgoals ohi.global.regions
##' ohi.global.regions.eez.noATA ohi.global.regions.max
##' ohi.global.regions.noATA ohi.goal.all ohi.goal.labels ohi.goal.subgoal.all
##' ohi.goal.subgoal.unique ohi.labels ohi.model.dimensions ohi.model.labels
##' ohi.subgoal.all ohi.subgoal.parent ohi.valuesets schemes subgoals
##' ohi.casestudies
##' @docType data
##' @format \enumerate{ \item \code{ohi.subgoal.all} is the list of only the
##' subgoals, or the goal if only 1 subgoal.  \item ohi.goal.subgoal.unique is
##' the list of only the subgoals, or the goal if only 1 subgoal.  \item
##' ohi.goal.subgoal.all the union of all goals and subgoals.  \item
##' ohi.subgoal.parent is the list of subgoals and their parent goal.  \item
##' \code{labels} is a list to map codes into text labels. }
##' @keywords datasets
##' @include ohi.R
##' @export goals goals_subgoals goal_subgoals ohi.global.regions
##' ohi.global.regions.eez.noATA ohi.global.regions.max
##' ohi.global.regions.noATA ohi.goal.all ohi.goal.labels ohi.goal.subgoal.all
##' ohi.goal.subgoal.unique ohi.labels ohi.model.dimensions ohi.model.labels
##' ohi.subgoal.all ohi.subgoal.parent ohi.valuesets schemes subgoals
##' ohi.casestudies
##' @examples
##' 
##' ohi.labels[['AO']]
##' ohi.labels[ohi.goal.all]
##' ohi.labels[ohi.goal.subgoal.all]
##' ohi.subgoal.parent[ohi.subgoal.all]
###
# load the *MANUAL* order of the goals and subgoals:
# Refs #40: TR is before LE as per Nature2012 Figure 1.
#
# ohi.goal.all is the list of only the goals
ohi.goal.all <- c('FP','AO','NP','CS','CP','TR','LE','SP','CW','BD')
# ohi.subgoal.all is the list of only the subgoals, or the goal if only 1 subgoal
ohi.subgoal.all <- c('FIS','MAR','LIV','ECO','ICO','LSP','HAB','SPP')
# ohi.goal.subgoal.unique is the list of only the subgoals, or the goal if only 1 subgoal
ohi.goal.subgoal.unique <- c('FIS','MAR','AO','NP','CS','CP','TR','LIV','ECO','ICO','LSP','CW','HAB','SPP')
# ohi.goal.subgoal.all is the cross product of all goals and subgoals
ohi.goal.subgoal.all <- c('FIS','FP','MAR','AO','NP','CS','CP','TR','LIV','LE','ECO','ICO','SP','LSP','CW','HAB','BD','SPP')
ohi.subgoal.parent <- c('FIS'='FP', 'MAR'='FP', 'LIV'='LE', 'ECO'='LE', 'ICO'='SP', 'LSP'='SP', 'HAB'='BD', 'SPP'='BD')

# Labels
ohi.model.keys <- c('r'='region', 'g'='goal', 'd'='dimension', 'c'='component', 'l'='layer')
ohi.model.labels <- c('x'='Status', 't'='Trend', 'p'='Pressures', 'r'='Resilience', 'xF'='Likely Future Status')
ohi.model.dimensions <- make.names(tolower(ohi.model.labels))
# Refs #40: Order here is meaningless -- use alpha sort by goal/subgoal
ohi.goal.labels <- c(
  'AO'='Artisanal Fishing Opportunities',
  'BD'='Biodiversity',
  'CP'='Coastal Protection',
  'CS'='Carbon Storage',
  'CW'='Clean Waters',
  'FP'='Food Provision',
  'LE'='Coastal Livelihoods and Economies',
  'NP'='Natural Products',
  'SP'='Sense of Place',
  'TR'='Tourism and Recreation',
  'ECO'='Economies',
  'FIS'='Fisheries',
  'HAB'='Habitats',
  'ICO'='Iconic Species',
  'LIV'='Livelihoods',
  'LSP'='Lasting Special Places',
  'MAR'='Mariculture',
  'SPP'='Species'
)

# schemes as list of column_name=aster_label
ohi.valuesets <- c('unweighted'='Global',
                   'preservationist'='Preservationist',
                   'extractive'='Extractive',
                   'nonextractive'='Non-extractive',
                   'extreme'='Extractive Extreme')

ohi.labels <- c(ohi.goal.labels, ohi.model.labels, ohi.valuesets)

ohi.pressure.category <- list('environmental'=c('po','hd','fp','sp','cc'), 'social'=c('ss'))