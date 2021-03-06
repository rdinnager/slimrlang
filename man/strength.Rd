% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slim_lang.R
\name{strength}
\alias{strength}
\alias{InteractionType$strength}
\alias{.IT$strength}
\title{SLiM method strength}
\usage{
strength(receiver, exerters)
}
\arguments{
\item{receiver}{An object of type Individual object. Must be of length 1 (a
singleton). See details for description.}

\item{exerters}{An object of type null or Individual object. The default value
is \code{NULL}. See details for description.}
}
\value{
An object of type float.
}
\description{
Documentation for SLiM function \code{strength}, which is a method of the SLiM
class \code{InteractionType}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.
}
\details{
Returns a vector containing the interaction strengths exerted upon
receiver by the individuals in exerters. If exerters is NULL (the default),
then a vector of the interaction strengths exerted by all individuals in the
subpopulation of receiver (including receiver itself) is returned; this case
may be handled much more efficiently than if a vector of all individuals in
the subpopulation is explicitly provided. If the strengths of interactions
exerted by a single individual upon multiple individuals is needed instead
(the inverse of what this method provides), multiple calls to this method will
be necessary, one per pairwise interaction queried; the interaction engine
is not optimized for the inverse case, and so it will likely be quite slow
to compute. If the interaction is reciprocal and sex-symmetric, the opposite
query should provide identical results in a single efficient call (because then
the interactions exerted are equal to the interactions received); otherwise,
the best approach might be to define a second interaction type representing
the inverse interaction that you wish to be able to query efficiently. If the
needed interaction strengths have already been calculated, those cached values
are simply returned. Otherwise, calling this method triggers evaluation of the
needed interactions, including calls to any applicable interaction() callbacks.
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
