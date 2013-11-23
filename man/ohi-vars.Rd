\docType{data}
\name{ohi-vars}
\alias{goal_subgoals}
\alias{goals}
\alias{goals_subgoals}
\alias{ohi-vars}
\alias{ohi.casestudies}
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
\title{Ocean Health Index: Global variables}
\format{\enumerate{ \item \code{ohi.subgoal.all} is the list of only the
subgoals, or the goal if only 1 subgoal.  \item ohi.goal.subgoal.unique is
the list of only the subgoals, or the goal if only 1 subgoal.  \item
ohi.goal.subgoal.all the union of all goals and subgoals.  \item
ohi.subgoal.parent is the list of subgoals and their parent goal.  \item
\code{labels} is a list to map codes into text labels. }}
\usage{
  ohi.goal.all
}
\description{
  Various variable constants for goals, dimensions, labels,
  etc.
}
\examples{
ohi.labels[['AO']]
ohi.labels[ohi.goal.all]
ohi.labels[ohi.goal.subgoal.all]
ohi.subgoal.parent[ohi.subgoal.all]
}
\keyword{datasets}

