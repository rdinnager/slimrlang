% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slim_lang.R
\name{setSubpopulationSize}
\alias{setSubpopulationSize}
\alias{Subpopulation$setSubpopulationSize}
\alias{.P$setSubpopulationSize}
\title{SLiM method setSubpopulationSize}
\usage{
setSubpopulationSize(size)
}
\arguments{
\item{size}{An object of type integer. Must be of length 1 (a singleton). See
details for description.}
}
\value{
An object of type void.
}
\description{
Documentation for SLiM function \code{setSubpopulationSize}, which is a method
of the SLiM class \code{Subpopulation}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.
}
\details{
Set the size of this subpopulation to size individuals. This
will take effect when children are next generated; it does not change the
current subpopulation state. Setting a subpopulation to a size of 0 does
have some immediate effects that serve to disconnect it from the simulation:
the subpopulation is removed from the list of active subpopulations, the
subpopulation is removed as a source of migration for all other subpopulations,
and the symbol representing the subpopulation is undefined. In this case, the
subpopulation itself remains unchanged until children are next generated (at
which point it is deallocated), but it is no longer part of the simulation and
should not be used.
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
