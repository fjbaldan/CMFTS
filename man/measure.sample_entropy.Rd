% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropy.R
\name{measure.sample_entropy}
\alias{measure.sample_entropy}
\title{Sample Entropy}
\usage{
measure.sample_entropy(y, M = 2, r = 0.2 * sd(y), package = "")
}
\arguments{
\item{y}{input data}

\item{M}{maximum template length}

\item{r}{matching tolerance}

\item{package}{version of aproximation entropy (pracma or sampenc)}
}
\value{
sample entropy estimates for m<-0,1,...,M-1
}
\description{
Based on: http://www.physionet.org/physiotools/sampen/matlab/1.1/sampenc.m
}
