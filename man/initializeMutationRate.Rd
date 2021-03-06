% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slim_lang.R
\name{initializeMutationRate}
\alias{initializeMutationRate}
\alias{Initialize$initializeMutationRate}
\alias{.Init$initializeMutationRate}
\title{SLiM method initializeMutationRate}
\usage{
initializeMutationRate(rates, ends, sex)
}
\arguments{
\item{rates}{An object of type numeric. See details for description.}

\item{ends}{An object of type null or integer. The default value is \code{NULL}.
See details for description.}

\item{sex}{An object of type string. Must be of length 1 (a singleton). The
default value is \code{"*"}. See details for description.}
}
\value{
An object of type void.
}
\description{
Documentation for SLiM function \code{initializeMutationRate}, which is a method
of the SLiM class \code{Initialize}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.
}
\details{
Set the mutation rate per base position per generation along the
chromosome. To be precise, this mutation rate is the expected mean number of
mutations that will occur per base position per generation (per new offspring
genome being generated); note that this is different from how the recombination
rate is defined (see initializeRecombinationRate()). The number of mutations
that actually occurs at a given base position when generating an offspring
genome is, in effect, drawn from a Poisson distribution with that expected
mean (but under the hood SLiM uses a mathematically equivalent but much more
efficient strategy). It is possible for this Poisson draw to indicate that two
or more new mutations have arisen at the same base position, particularly when
the mutation rate is very high; in this case, the new mutations will be added to
the site one at a time, and as always the mutation stacking policy (see section
1.5.3) will be followed. There are two ways to call this function. If the
optional ends parameter is NULL (the default), then rates must be a singleton
value that specifies a single mutation rate to be used along the entire
chromosome. If, on the other hand, ends is supplied, then rates and ends must be
the same length, and the values in ends must be specified in ascending order. In
that case, rates and ends taken together specify the mutation rates to be used
along successive contiguous stretches of the chromosome, from beginning to end;
the last position specified in ends should extend to the end of the chromosome
(i.e. at least to the end of the last genomic element, if not further). For
example, if the following call is made: initializeMutationRate(c(1e-7, 2.5e-8),
c(5000, 9999)); then the result is that the mutation rate for bases 0...5000
(inclusive) will be 1e-7, and the rate for bases 5001...9999 (inclusive) will be
2.5e-8. Note that mutations are generated by SLiM only within genomic elements,
regardless of the mutation rate map. In effect, the mutation rate map given
is intersected with the coverage area of the genomic elements defined; areas
outside of any genomic element are given a mutation rate of zero. There is no
harm in supplying a mutation rate map that specifies rates for areas outside
of the genomic elements defined; that rate information is simply not used.
The overallMutationRate family of properties on Chromosome provide the overall
mutation rate after genomic element coverage has been taken into account, so it
will reflect the rate at which new mutations will actually be generated in the
simulation as configured. If the optional sex parameter is "*" (the default),
then the supplied mutation rate map will be used for both sexes (which is the
only option for hermaphroditic simulations). In sexual simulations sex may be
"M" or "F" instead, in which case the supplied mutation rate map is used only
for that sex (i.e., when generating a gamete from a parent of that sex). In this
case, two calls must be made to initializeMutationRate(), one for each sex, even
if a rate of zero is desired for the other sex; no default mutation rate map
is supplied. In nucleotide-based models, initializeMutationRate() may not be
called. Instead, the desired sequence-based mutation rate(s) should be expressed
in the mutationMatrix parameter to initializeGenomicElementType(). If variation
in the mutation rate along the chromosome is desired, initializeHotspotMap()
should be used.
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
