% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slim_lang.R
\name{setGeneConversion}
\alias{setGeneConversion}
\alias{Chromosome$setGeneConversion}
\alias{.c$setGeneConversion}
\title{SLiM method setGeneConversion}
\usage{
setGeneConversion(
  nonCrossoverFraction,
  meanLength,
  simpleConversionFraction,
  bias
)
}
\arguments{
\item{nonCrossoverFraction}{An object of type numeric or numeric or numeric or
numeric. Must be of length 1 (a singleton). See details for description.}

\item{meanLength}{An object of type numeric or numeric or numeric or numeric.
Must be of length 1 (a singleton). See details for description.}

\item{simpleConversionFraction}{An object of type numeric or numeric or numeric
or numeric. Must be of length 1 (a singleton). See details for description.}

\item{bias}{An object of type numeric or numeric or numeric or numeric. Must
be of length 1 (a singleton). The default value is \code{0}. See details for
description.}
}
\value{
An object of type void.
}
\description{
Documentation for SLiM function \code{setGeneConversion}, which is a method of
the SLiM class \code{Chromosome}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.
}
\details{
This method switches the recombination model to the "double-stranded
break (DSB)” model (if it is not already set to that), and configures the
details of the gene conversion tracts that will therefore be modeled (see
section 1.5.6 for discussion of the "DSB” recombination model). The meanings
and effects of the parameters exactly mirror the initializeGeneConversion()
function; see section 23.1 for details.
}
\section{Copyright}{

This is documentation for a function in the SLiM software, and has been
reproduced from the official manual,
which can be found here: \url{http://benhaller.com/slim/SLiM_Manual.pdf}. This
documentation is
Copyright © 2016–2020 Philipp Messer. All rights reserved. More information
about SLiM can be found
on the official website: \url{https://messerlab.org/slim/}
}

\author{
Benjamin C Haller (\email{bhaller@benhaller.com}) and Philipp W Messer
(\email{messer@cornell.edu})
}
