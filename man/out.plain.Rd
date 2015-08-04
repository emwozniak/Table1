\name{out.plain}
\alias{out.plain}

\title{Plain Text Output
}
\description{This function outputs the results of calls to \code{\link{make.table}}, \code{\link{quick.table}}, \code{\link{cat.var}}, and \code{\link{cont.var}} to the console as formatted plain text data frames.
}
\usage{
  out.plain(tab, colnames)
}

\arguments{
  \item{tab}{A single call to either \code{\link{cat.var}} or \code{\link{cont.var}}, or a data frame produced via \code{\link{make.table}} or \code{\link{quick.table}}. Required.
  }
  \item{colnames}{A vector of column names for the table output. Optional.
  }
}

\details{
A call to this function is the default output argument to all package functions that produce summary output.
  }
  
\value{A plain text data frame of summary statistics outputted to the console.
  }
  
%\references{
%}
\author{ Erica Wozniak}
%\note{
%}

\seealso{
  \code{\link{Table1}}
  \code{\link{make.table}}
  \code{\link{quick.table}}
  \code{\link{cat.var}}
  \code{\link{cont.var}}
  \code{\link{stat.col}}
  \code{\link{out.latex}}
  \code{\link{out.html}}
}
\examples{
library(survival)
attach(pbc)

make.table(dat=pbc,
    cat.varlist=c('stage', 'sex'),
    cat.header=c('Stage', 'Sex'),
    cat.rownames=list(c('I', 'II', 'III', 'IV'), c('Male', 'Female')),
    cat.ptype=c('fisher', 'fisher'),   
    cont.varlist=c('bili', 'copper'),
    cont.header=c('Bilirubin', 'Urine copper'),
    cont.ptype=c('wilcox', 'wilcox'), 
    strat=c('trt'),
    #Remove row percents from all categorical variables (list is recycled)
    cat.rmstat=list('row'),
    #Remove missing, min/max from bili and no summary stats from copper
    cont.rmstat=list(c('miss', 'minmax'), 'None'),
    colnames=c('', 'D-penicillamine', 'Placebo', 'Overall', 'p-value')
    )
}
