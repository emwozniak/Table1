\name{cont.var}
\alias{cont.var}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Continuous Variable Summary
%%  ~~function to do ... ~~
}
\description{
This function computes mean (SD), median (IQR), Q1/Q3, min/max, and a count of missings for a continuous variable input, optionally stratified by another variable. Paired and unpaired t-tests, Wilcoxon rank sum, Wilcoxon signed-rank, one-way between-subjects ANOVA, and Kruskal-Wallis tests are currently supported for the addition of p-values to tables.
}
\usage{
cont.var(var, strat, dec, header, ptype, pname)
}
\arguments{
  \item{var}{
  Continuous variable input. Required.  
}
  \item{strat}{
  Column stratifying variable. Optional.
}
  \item{dec}{
  Number of decimal places on summary statistics. Optional; defaults to 2.
}
  \item{header}{
  Character vector providing a short variable description. Optional; defaults to variable name.
}
  \item{ptype}{
  Statistical test for p-value. Optional; defaults to none. \cr
    * 'anova' - One-way between-subjects ANOVA \cr
    * 'kruskal' - Kruskal-Wallis test \cr
    * 'ttest' - Student's t-test \cr
    * 'ttest.pair' - Paired t-test \cr
    * 'wilcox' - Wilcoxon rank sum test \cr
    * 'wilcox.pair' - Wilcoxon signed-rank test 
}
  \item{pname}{
  If TRUE then the name of the statistical test used will be printed below the p-value. Optional; defaults to FALSE.
  }
}
\details{Note that warnings and errors beyond those automatically provided by some base R functions are not in place for the improper use of significance testing. For example, if a t-test is requested with more than two strata, the code will execute and the returned p-value will reflect a test of only the first two strata.
}
\value{
Returns a data frame of continuous summary statistics.
}
\references{
}
\author{Erica Wozniak
}
\note{
}

\seealso{
\code{\link{Table1}}
\code{\link{make.table}}
\code{\link{cat.var}}
\code{\link{out.plain}}
\code{\link{out.latex}}
\code{\link{out.html}}
}
\examples{
library(survival)
attach(pbc)
cont.var(var=age,
         strat=sex,
         header='Age',
         ptype='ttest')
}