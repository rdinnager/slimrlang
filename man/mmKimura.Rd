% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slim_lang.R
\name{mmKimura}
\alias{mmKimura}
\alias{SLiMBuiltin$mmKimura}
\alias{.SB$mmKimura}
\title{SLiM method mmKimura}
\usage{
mmKimura(alpha, beta)
}
\arguments{
\item{alpha}{An object of type float or float. Must be of length 1 (a
singleton). See details for description.}

\item{beta}{An object of type float or float. Must be of length 1 (a singleton).
See details for description.}
}
\value{
An object of type string or float or float or float or integer or float
or integer.
}
\description{
Documentation for SLiM function \code{mmKimura}, which is a method of the SLiM
class \code{SLiMBuiltin}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.
}
\details{
Returns a mutation matrix representing a Kimura (1980) model with
transition rate alpha and transversion rate beta: This 2×2 matrix is suitable
for use with initializeGenomicElementType(). Note that the actual mutation rate
produced by this model is alpha+2*beta.
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
