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
Use the \code{\link{cont.var}} and \code{\link{cat.var}} functions to produce summary statistics for continuous and categorical variables, respectively. Create a data frame of these results with rbind, and output the results as plain text, LaTeX, or HTML using \code{\link{out.plain}}, \code{\link{out.latex}}, or \code{\link{out.html}}.
}
\author{
\packageAuthor{Table1}

Maintainer: \packageMaintainer{Table1}
}
\references{
}

\keyword{ package }
\seealso{

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
