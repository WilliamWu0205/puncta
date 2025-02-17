% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/puncta.R
\name{puncta.stats}
\alias{puncta.stats}
\title{Run regression for puncta data}
\usage{
puncta.stats(
  dset,
  y.var,
  cond.var,
  subsamp.var,
  cond.inc,
  trans.name = "identity",
  y.count = F
)
}
\arguments{
\item{dset}{data.frame of experimental data}

\item{y.var}{name of y variable in dset}

\item{cond.var}{name of condition variable in dset}

\item{subsamp.var}{name of subsample variable for random effect}

\item{cond.inc}{vector of conditions to include in analysis}

\item{trans.name}{name of function to transform y as a string}

\item{y.count}{indicates (T/F) whether y is a count variable}
}
\description{
Run regression for puncta data
}
