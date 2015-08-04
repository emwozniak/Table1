\name{Table1-package}
\alias{Table1-package}
\alias{Table1}

\docType{package}
\title{
  \packageTitle{Table1}
}
\description{
  \packageDescription{Table1}
}
\details{
  \packageDESCRIPTION{Table1}
The main summary functions in this package are \code{\link{make.table}} and \code{\link{quick.table}}. The \code{\link{make.table}} function accepts separate vectorized inputs for categorical and continuous variables and provides numerous options to customize table output, including significance testing. By contrast, \code{\link{quick.table}} allows for very little customization; it is instead intended to generate rapid summaries of datasets with minimal input, dynamically classifying each variable as either categorical or continuous.     
  \packageIndices{Table1}
}

\author{
\packageAuthor{Table1}

Maintainer: \packageMaintainer{Table1}
}
%\references{
%}

\note{
You must \code{\link{attach}} and clean (e.g., replace blank entries with NAs) the data frame of interest before applying these functions. \cr
\cr
Working installations of \code{\link{xtable}} and \code{\link{htmlTable}} are required for LaTeX and HTML output, respectively.
}

\keyword{ package }
\seealso{
  \code{\link{make.table}}
  \code{\link{quick.table}}
  \code{\link{cat.var}}
  \code{\link{cont.var}}
  \code{\link{stat.col}}
  \code{\link{out.plain}}
  \code{\link{out.latex}}
  \code{\link{out.html}}
}
\examples{
library(survival)
library(htmlTable)
attach(pbc)

#Plain text table generated with make.table using vectorized inputs
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
    
#HTML table generated with quick.table 
library(htmlTable)
quick.table(dat=pbc,
    strat='sex',
    colnames=c('', 'Male', 'Female', 'Overall'),
    #See www.color-hex.com for codes
    output='html',
    stripe.col='#d0f2b0'
    )
}
