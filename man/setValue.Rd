% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slim_lang.R
\name{setValue}
\alias{setValue}
\alias{GenomicElementType$setValue}
\alias{.GET$setValue}
\alias{Individual$setValue}
\alias{.I$setValue}
\alias{InteractionType$setValue}
\alias{.IT$setValue}
\alias{Mutation$setValue}
\alias{.M$setValue}
\alias{MutationType$setValue}
\alias{.MT$setValue}
\alias{SLiMSim$setValue}
\alias{.SS$setValue}
\alias{Subpopulation$setValue}
\alias{.P$setValue}
\alias{Substitution$setValue}
\alias{.S$setValue}
\title{SLiM method setValue}
\usage{
setValue(key, value)

setValue(key, value)

setValue(key, value)

setValue(key, value)

setValue(key, value)

setValue(key, value)

setValue(key, value)

setValue(key, value)
}
\arguments{
\item{key}{An object of type string or any but object. Must be of length 1 (a
singleton). See details for description.}

\item{value}{An object of type string or any but object. See details for
description.}
}
\value{
An object of type any but object or void or void or void.

An object of type void.

An object of type void.

An object of type any but object or void or void or void.

An object of type float or any but object or void or void.

An object of type void.

An object of type void.

An object of type any but object or void.
}
\description{
Documentation for SLiM function \code{setValue}, which is a method of the SLiM
class \code{GenomicElementType}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.

Documentation for SLiM function \code{setValue}, which is a method of the SLiM
class \code{Individual}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.

Documentation for SLiM function \code{setValue}, which is a method of the SLiM
class \code{InteractionType}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.

Documentation for SLiM function \code{setValue}, which is a method of the SLiM
class \code{Mutation}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.

Documentation for SLiM function \code{setValue}, which is a method of the SLiM
class \code{MutationType}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.

Documentation for SLiM function \code{setValue}, which is a method of the SLiM
class \code{SLiMSim}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.

Documentation for SLiM function \code{setValue}, which is a method of the SLiM
class \code{Subpopulation}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.

Documentation for SLiM function \code{setValue}, which is a method of the SLiM
class \code{Substitution}.
Note that the R function is a stub, it does not do anything in R (except bring
up this documentation). It will only do
anything useful when used inside a \code{\link{slim_block}} function further
nested in a \code{\link{slim_script}}
function call, where it will be translated into valid SLiM code as part of a
full SLiM script.
}
\details{
Sets a value for the dictionary entry identifier key. The value, which
may be of any type other than object, can be fetched later using getValue().
This dictionary-style functionality is actually provided by the superclass of
GenomicElementType, SLiMEidosDictionary, although that fact is not presently
visible in Eidos since superclasses are not introspectable.

Sets a value for the dictionary entry identifier key. The value, which
may be of any type other than object, can be fetched later using getValue().
This dictionary-style functionality is actually provided by the superclass of
Individual, SLiMEidosDictionary, although that fact is not presently visible in
Eidos since superclasses are not introspectable.

Sets a value for the dictionary entry identifier key. The value, which
may be of any type other than object, can be fetched later using getValue().
This dictionary-style functionality is actually provided by the superclass
of InteractionType, SLiMEidosDictionary, although that fact is not presently
visible in Eidos since superclasses are not introspectable.

Sets a value for the dictionary entry identifier key. The value, which
may be of any type other than object, can be fetched later using getValue().
This dictionary-style functionality is actually provided by the superclass of
Mutation, SLiMEidosDictionary, although that fact is not presently visible in
Eidos since superclasses are not introspectable.

Sets a value for the dictionary entry identifier key. The value, which
may be of any type other than object, can be fetched later using getValue().
This dictionary-style functionality is actually provided by the superclass of
MutationType, SLiMEidosDictionary, although that fact is not presently visible
in Eidos since superclasses are not introspectable.

Sets a value for the dictionary entry identifier key. The value, which
may be of any type other than object, can be fetched later using getValue().
This dictionary-style functionality is actually provided by the superclass of
SLiMSim, SLiMEidosDictionary, although that fact is not presently visible in
Eidos since superclasses are not introspectable.

Sets a value for the dictionary entry identifier key. The value, which
may be of any type other than object, can be fetched later using getValue().
This dictionary-style functionality is actually provided by the superclass of
Subpopulation, SLiMEidosDictionary, although that fact is not presently visible
in Eidos since superclasses are not introspectable.

Sets a value for the dictionary entry identifier key. The value, which
may be of any type other than object, can be fetched later using getValue().
This dictionary-style functionality is actually provided by the superclass of
Substitution, SLiMEidosDictionary, although that fact is not presently visible
in Eidos since superclasses are not introspectable.
}
\section{Copyright}{

This is documentation for a function in the SLiM software, and has been
reproduced from the official manual,
which can be found here: \url{http://benhaller.com/slim/SLiM_Manual.pdf}. This
documentation is
Copyright © 2016–2020 Philipp Messer. All rights reserved. More information
about SLiM can be found
on the official website: \url{https://messerlab.org/slim/}


This is documentation for a function in the SLiM software, and has been
reproduced from the official manual,
which can be found here: \url{http://benhaller.com/slim/SLiM_Manual.pdf}. This
documentation is
Copyright © 2016–2020 Philipp Messer. All rights reserved. More information
about SLiM can be found
on the official website: \url{https://messerlab.org/slim/}


This is documentation for a function in the SLiM software, and has been
reproduced from the official manual,
which can be found here: \url{http://benhaller.com/slim/SLiM_Manual.pdf}. This
documentation is
Copyright © 2016–2020 Philipp Messer. All rights reserved. More information
about SLiM can be found
on the official website: \url{https://messerlab.org/slim/}


This is documentation for a function in the SLiM software, and has been
reproduced from the official manual,
which can be found here: \url{http://benhaller.com/slim/SLiM_Manual.pdf}. This
documentation is
Copyright © 2016–2020 Philipp Messer. All rights reserved. More information
about SLiM can be found
on the official website: \url{https://messerlab.org/slim/}


This is documentation for a function in the SLiM software, and has been
reproduced from the official manual,
which can be found here: \url{http://benhaller.com/slim/SLiM_Manual.pdf}. This
documentation is
Copyright © 2016–2020 Philipp Messer. All rights reserved. More information
about SLiM can be found
on the official website: \url{https://messerlab.org/slim/}


This is documentation for a function in the SLiM software, and has been
reproduced from the official manual,
which can be found here: \url{http://benhaller.com/slim/SLiM_Manual.pdf}. This
documentation is
Copyright © 2016–2020 Philipp Messer. All rights reserved. More information
about SLiM can be found
on the official website: \url{https://messerlab.org/slim/}


This is documentation for a function in the SLiM software, and has been
reproduced from the official manual,
which can be found here: \url{http://benhaller.com/slim/SLiM_Manual.pdf}. This
documentation is
Copyright © 2016–2020 Philipp Messer. All rights reserved. More information
about SLiM can be found
on the official website: \url{https://messerlab.org/slim/}


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

Benjamin C Haller (\email{bhaller@benhaller.com}) and Philipp W Messer
(\email{messer@cornell.edu})

Benjamin C Haller (\email{bhaller@benhaller.com}) and Philipp W Messer
(\email{messer@cornell.edu})

Benjamin C Haller (\email{bhaller@benhaller.com}) and Philipp W Messer
(\email{messer@cornell.edu})

Benjamin C Haller (\email{bhaller@benhaller.com}) and Philipp W Messer
(\email{messer@cornell.edu})

Benjamin C Haller (\email{bhaller@benhaller.com}) and Philipp W Messer
(\email{messer@cornell.edu})

Benjamin C Haller (\email{bhaller@benhaller.com}) and Philipp W Messer
(\email{messer@cornell.edu})

Benjamin C Haller (\email{bhaller@benhaller.com}) and Philipp W Messer
(\email{messer@cornell.edu})
}
