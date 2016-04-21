\name{out.latex}
\alias{out.latex}

\title{LaTeX Output
}
\description{This function utilizes \code{\link{xtable}} to output summary table results to the console as formatted LaTeX code, and may be used within a Sweave or knitr document.
}
\usage{
  out.latex(tab, colnames, header.style, factor.style, stat.style, vspace)
}

\arguments{
  \item{tab}{A single call to either \code{\link{cat.var}} or \code{\link{cont.var}}, or a data frame produced via \code{\link{rbind}} of multiple calls to these functions. Required.
  }
  \item{colnames}{A vector of column names for the table output. Optional.
  }
  \item{header.style}{Variable name style: "plain", "bold", "italic", or "bolditalic". Optional; defaults to "bold".
  }
  \item{factor.style}{Variable factor levels style: "plain", "bold", "italic", or "bolditalic". Optional; defaults to "bold".
  }
  \item{stat.style}{Summary statistic style: "plain", "bold", "italic", or "bolditalic". Optional; defaults to "plain".
  }
  \item{vspace}{
  TRUE for additional vertical whitespace between variables (default)
  }
}
\details{
  A call to this function is an optional argument to \code{\link{make.table}} and \code{\link{quick.table}} via output='latex'.
  }
  
\value{LaTeX code for a formatted table.}
%\references{
%}
\author{Erica Wozniak}
%\note{
%}

\seealso{
  \code{\link{Table1}}
  \code{\link{make.table}}
  \code{\link{quick.table}}
  \code{\link{cat.var}}
  \code{\link{cont.var}}
  \code{\link{stat.col}}
  \code{\link{out.plain}}
  \code{\link{out.html}}
}
\examples{
library(xtable)

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
    colnames=c('Variable', 'D-penicillamine', 'Placebo', 'Overall', 'p-value'),
    output='latex'
    )

}