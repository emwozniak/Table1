\name{out.html}
\alias{out.html}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HTML Output
%%  ~~function to do ... ~~
}
\description{This function utilizes \code{\link{htmlTable}} to output the results of the \code{\link{cat.var}} and \code{\link{cont.var}} functions to the R Viewer as a formatted HTML table, and may be used within a knitr document.
}
\usage{
out.html(tab, colnames)
}

\arguments{
  \item{tab}{A single call to either \code{\link{cat.var}} or \code{\link{cont.var}}, or a data frame produced via \code{\link{rbind}} of multiple calls to these functions. Required.
}
  \item{colnames}{A vector of column names for the table output. Optional.
}
}
\details{
A call to this function is an optional argument to \code{\link{make.table}}.
}
\value{A formatted HTML table in the Viewer or code for inclusion in a knitr document.
}
\references{
}
\author{Erica Wozniak
}
\note{
}

\seealso{
\code{\link{Table1}}
\code{\link{cat.var}}
\code{\link{cont.var}}
\code{\link{out.plain}}
\code{\link{out.latex}}
}
\examples{
library(survival)
library(htmlTable)
attach(pbc)
example <- rbind(
  cont.var(var=age, strat=trt, header='Age', ptype='ttest'),
  cat.var(var=sex, strat=trt, rownames=c('Male', 'Female'), header='Sex', ptype='chisq'),
  cont.var(var=bili, strat=trt, header='Serum bilirubin', ptype='ttest')
  )
out.html(example, colnames=c('Variable', 'D-penicillmain', 'Placebo', 'Overall', 'p-value'))
}