\name{out.plain}
\alias{out.plain}

\title{Plain Text Output
}
\description{This function outputs the results of the \code{\link{cat.var}} and \code{\link{cont.var}} functions to the console as a formatted plain text data frame.
}
\usage{
out.plain(tab, colnames)
}

\arguments{
  \item{tab}{A single call to either \code{\link{cat.var}} or \code{\link{cont.var}}, or a data frame produced via \code{\link{rbind}} of multiple calls to these functions. Required.
}
  \item{colnames}{A vector of column names for the table output. Optional.
}
}
\details{
A call to this function is the default output argument to \code{\link{make.table}}.
}
\value{ A plain text data frame of summary statistics outputted to the console.
}
\references{
}
\author{ Erica Wozniak
}
\note{
}

\seealso{
\code{\link{Table1}}
\code{\link{cat.var}}
\code{\link{cont.var}}
\code{\link{out.latex}}
\code{\link{out.html}}
}
\examples{
library(survival)
attach(pbc)
example <- rbind(
  cont.var(var=age, strat=trt, header='Age', ptype='ttest'),
  cat.var(var=sex, strat=trt, rownames=c('Male', 'Female'), header='Sex', ptype='chisq'),
  cont.var(var=bili, strat=trt, header='Serum bilirubin', ptype='ttest')
  )
out.plain(example, colnames=c('Variable', 'D-penicillmain', 'Placebo', 'Overall', 'p-value'))
}
