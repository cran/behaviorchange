% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opts.R
\docType{data}
\name{opts}
\alias{opts}
\alias{set}
\alias{get}
\alias{reset}
\title{Options for the behaviorchange package}
\format{
An object of class \code{list} of length 4.
}
\usage{
opts
}
\description{
The \code{behaviorchange::opts} object contains three functions to set, get, and reset
options used by the escalc package. Use \code{behaviorchange::opts$set} to set options,
\code{behaviorchange::opts$get} to get options, or \code{behaviorchange::opts$reset} to reset specific or
all options to their default values.
}
\details{
It is normally not necessary to get or set \code{behaviorchange} options.

The following arguments can be passed:

\describe{
\item{...}{For \code{behaviorchange::opts$set}, the dots can be used to specify the options
to set, in the format \code{option = value}, for example,
\code{EFFECTSIZE_POINTESTIMATE_NAME_IN_DF = "\\n"}. For
\code{behaviorchange::opts$reset}, a list of options to be reset can be passed.}
\item{option}{For \code{behaviorchange::opts$set}, the name of the option to set.}
\item{default}{For \code{behaviorchange::opts$get}, the default value to return if the
option has not been manually specified.}
}

The following options can be set:

\describe{

\item{}{The name of the column
with the effect size values.}

\item{}{The name of the column
with the effect size variance.}

\item{}{The name of the column
with the missing values.}

}
}
\examples{
### Get the default utteranceMarker
behaviorchange::opts$get(complecs_entitySheet);

### Set it to a custom version, so that every line starts with a pipe
behaviorchange::opts$set(complecs_entitySheet = "sheet_with_entities");

### Check that it worked
behaviorchange::opts$get(complecs_entitySheet);

### Reset this option to its default value
behaviorchange::opts$reset(complecs_entitySheet);

### Check that the reset worked, too
behaviorchange::opts$get(complecs_entitySheet);

}
\keyword{datasets}
