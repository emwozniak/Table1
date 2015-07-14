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
%The DESCRIPTION file:
%\packageDESCRIPTION{Table1}
%\packageIndices{Table1}
To construct a table manually, use the \code{\link{cont.var}} and \code{\link{cat.var}} functions to produce summary statistics for continuous and categorical variables, respectively. Create a data frame of these results with \code{\link{rbind}}, and output the results as plain text, LaTeX, or HTML using \code{\link{out.plain}}, \code{\link{out.latex}}, or \code{\link{out.html}}, respectively. This process is automated with the \code{\link{make.table}} wrapper function.
}

\author{
\packageAuthor{Table1}

Maintainer: \packageMaintainer{Table1}
}
\references{
}

\note{
You must \code{\link{attach}} the data frame of interest before applying these functions. \cr
\cr
Working installations of \code{\link{xtable}} and \code{\link{htmlTable}} are required for LaTeX or HTML output. \cr
\cr
The options to stratify by more than one variable, or to apply these functions to multiple variables at a time, are available through the \code{\link{make.table}} function.
}

\keyword{ package }
\seealso{
\code{\link{make.table}}
\code{\link{cat.var}}
\code{\link{cont.var}}
\code{\link{out.plain}}
\code{\link{out.latex}}
\code{\link{out.html}}
}
\examples{
library(survival)
library(htmlTable)
attach(pbc)

#Table generated with make.table using vectorized inputs (recommended use)
make.table(dat=pbc,
    cat.varlist=c('stage', 'trt'),
    cat.header=c('Stage', 'Treatment'),
    cat.rownames=list(c('I', 'II', 'III', 'IV'), c('D-penicillmain', 'Placebo')),
    cat.ptype=c('fisher', 'fisher'),   
    cont.varlist=c('bili', 'copper'),
    cont.header=c('Bilirubin', 'Urine copper'),
    cont.ptype=c('wilcox', 'wilcox'), 
    strat=c('sex'),
    colnames=c('', 'Male', 'Female', 'Overall', 'p-value'),
    output='html'
    )
    
#Table generated manually with multiple function calls
example <- rbind(
  cont.var(var=age, strat=trt, header='Age', ptype='ttest'),
  cat.var(var=sex, strat=trt, rownames=c('Male', 'Female'), header='Sex', ptype='chisq'),
  cont.var(var=bili, strat=trt, header='Serum bilirubin', ptype='ttest')
  )
out.html(example, colnames=c('Variable', 'D-penicillmain', 'Placebo', 'Overall', 'p-value'))
}
