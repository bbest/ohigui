\name{ohi-vars}
\alias{goals}
\alias{goals_subgoals}
\alias{goal_subgoals}
\alias{ohi.global.regions}
\alias{ohi.global.regions.eez.noATA}
\alias{ohi.global.regions.max}
\alias{ohi.global.regions.noATA}
\alias{ohi.goal.all}
\alias{ohi.goal.labels}
\alias{ohi.goal.subgoal.all}
\alias{ohi.goal.subgoal.unique}
\alias{ohi.labels}
\alias{ohi.model.dimensions}
\alias{ohi.model.labels}
\alias{ohi.subgoal.all}
\alias{ohi.subgoal.parent}
\alias{ohi.valuesets}
\alias{schemes}
\alias{subgoals}
\alias{ohi.casestudies}
\docType{data}
\title{Ocean Health Index: Global variables}
\description{
Various variable constants for goals, dimensions, labels, etc.}
\usage{
goals
goals_subgoals
goal_subgoals
ohi.global.regions
ohi.global.regions.eez.noATA
ohi.global.regions.max
ohi.global.regions.noATA
ohi.goal.all
ohi.goal.labels
ohi.goal.subgoal.all
ohi.goal.subgoal.unique
ohi.labels
ohi.model.dimensions
ohi.casestudies
ohi.model.labels
ohi.subgoal.all
ohi.subgoal.parent
ohi.valuesets
schemes
subgoals
}
\format{\enumerate{
  \item \code{ohi.subgoal.all} is the list of only the subgoals, or the goal if only 1 subgoal.
  \item ohi.goal.subgoal.unique is the list of only the subgoals, or the goal if only 1 subgoal.
  \item ohi.goal.subgoal.all the union of all goals and subgoals.
  \item ohi.subgoal.parent is the list of subgoals and their parent goal.
  \item \code{labels} is a list to map codes into text labels.
}
}
\examples{
ohi.labels[['AO']]
ohi.labels[ohi.goal.all]
ohi.labels[ohi.goal.subgoal.all]
ohi.subgoal.parent[ohi.subgoal.all]
}
\keyword{datasets}
