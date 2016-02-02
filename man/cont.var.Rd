\name{cont.var}
\alias{cont.var}

\title{Continuous Variable Summary}

\description{
This function computes mean (SD), median (IQR), Q1/Q3, min/max, and a count of missings for a continuous         variable input, optionally stratified by another variable, and with the option to compute p-values.
}

\usage{
cont.var(var, strat, dec, header, ptype, pname, cont.rmstat)
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
  Statistical test for p-value. Optional; defaults to none. See \code{\link{stat.col}} for a complete list of options. \cr
      \describe{
          \item{'anova'}{One-way between-subjects ANOVA}
          \item{'kruskal'}{Kruskal-Wallis test}
          \item{'ttest'}{Student's t-test}
          \item{'ttest.pair'}{Paired t-test}
          \item{'wilcox'}{Wilcoxon rank sum test}
          \item{'wilcox.pair'}{Wilcoxon signed-rank test}    
      }
  }
  \item{pname}{
  If TRUE then the name of the statistical test used will be printed below the p-value. Optional; defaults to FALSE.
  }
  \item{cont.rmstat}{
  Vector naming the statistics that should be removed from the summary data frame. Optional; defaults to none.
      \describe{
          \item{'count'}{Remove overall count}
          \item{'meansd'}{Remove mean and standard deviation}
          \item{'mediqr'}{Remove median and interquartile range}
          \item{'q1q3'}{Remove 25th and 75th percentiles}
          \item{'minmax'}{Remove minimum and maximum}
          \item{'miss'}{Remove summary of missing values}
      }  
  }
  \item{vspace} {
  TRUE for additional vertical whitespace between variables (default)
  }
  \item{output} {
  Output type passed from \code{\link{make.table}}.
  }
}

\details{Note that warnings and errors beyond those automatically provided by some base R functions are not in place for the improper use of significance testing. For example, if a t-test is requested with more than two strata, the code will execute and the returned p-value will reflect a test of only the first two strata.
}

\value{Returns a data frame of continuous summary statistics.}

\author{Erica Wozniak}

\seealso{
  \code{\link{Table1}}
  \code{\link{cat.var}}
  \code{\link{make.table}}
  \code{\link{quick.table}}
  \code{\link{stat.col}}
  \code{\link{out.plain}}
  \code{\link{out.latex}}
  \code{\link{out.html}}
}

\examples{
library(survival)
attach(pbc)

##Default output 
cont.var(var=age,
         strat=sex
         )
         
##Naming options and p-value         
cont.var(var=age,
         strat=sex,
         header='Age',
         ptype='ttest',
         pname=TRUE
         )
         
##Remove min/max and missing statistics
cont.var(var=age,
         strat=sex,
         header='Age',
         ptype='ttest',
         pname=TRUE,
         cont.rmstat=c('minmax', 'miss')
         )
}