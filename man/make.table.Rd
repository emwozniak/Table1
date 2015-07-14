\name{make.table}
\alias{make.table}

\title{Make Summary Table}

\description{This function produces summary statistics for categeorical and continuous variables and combines the results into a data frame for plain text, HTML, or LaTeX output, serving as a wrapper to apply vectorized inputs to \code{\link{cat.var}} and \code{\link{cont.var}}.
}
\usage{
make.table(dat, cat.varlist, cat.header, cat.rownames, cat.ptype,
cont.varlist, cont.header, cont.ptype, strat, dec, pname, 
colnames, output)
}

\arguments{
  \item{dat}{
  Data frame input of all variables used for the summary table. Required.
  }
  \item{cat.varlist}{
  Vector of categorical variable names. Optional; defaults to NULL.
  }
  \item{cat.header}{
  Ordered vector of short descriptions for each categorical variable. Optional; defaults to variable names.
  }
  \item{cat.rownames}{
  Ordered list of vectors providing row names for each level of each vector found in cat.varlist. Optional; defaults to the coded value of each level.
  }
  \item{cat.ptype}{
  Ordered vector of tests requested to add p-values to table for categorical variables. Optional; defaults to no p-values. \cr
    * 'chisq' - Chi-square test \cr
    * 'fisher' - Fisher's exact test \cr
    * 'mcnemar' - McNemar's test
  }
  \item{cont.varlist}{
  Vector of continuous variable names. Optional; defaults to NULL.

  }
  \item{cont.header}{
  Ordered vector of short descriptions for each continuous variable. Optional; defaults to variable name.
  }
  \item{cont.ptype}{
    Ordered vector of tests requested to add p-values to table for continuous variables. Optional; defaults to no p-values. \cr
    * 'anova' - One-way between-subjects ANOVA \cr
    * 'kruskal' - Kruskal-Wallis test \cr
    * 'ttest' - t-test \cr
    * 'ttest.pair' - Paired t-test \cr
    * 'wilcox' - Wilcoxon rank sum test \cr
    * 'wilcox.pair' - Wilcoxon signed rank test

  }
  \item{strat}{
  Vector of one or more stratifying variables. Optional; defaults to no stratification.
  }
  \item{dec}{
  Integer value for number of decimal places to be printed for each summary statistic. Optional; defaults to 2.
  }
  \item{pname}{
  Logical indicator to print statistical test used beneath each p-value. Optional; defaults to FALSE.
  }
  \item{colnames}{
  Ordered vector of column names for table. Optional; defaults to levels of strata variable(s).
  }
  \item{output}{
  Choice of formatted table output: 'html', 'latex', or 'plain'. Optional; defaults to plain text printed in console.
  }
}

\value{
Returns a table of summary statistics for specified categorical and continuous variables in the requested output format. The plain text default returns a data frame to the console.
}

\author{
Erica Wozniak
}

\note{
A few miscellaneous notes: \cr
\cr
The data frame must be attached (see \code{\link{attach}}) prior to use of the function.
\cr
\cr
Table output to HTML and LaTeX require the package dependencies \code{\link{htmlTable}} and \code{\link{xtable}}
\cr
\cr
The user must ensure that no variable names share the name of a base R function; otherwise, the program will throw an error. 
\cr
\cr
By default, the final summary table is rearranged such that the variables in the table are in the same order as in the raw data set (dat=). 
\cr
\cr
If missing values occur in stratifying variables (strat=), then the observations with missings will be removed from the table, and a comment will be printed indicating the total number removed. If multiple stratifying variables are provided, look to the console for a summary of total missing observations for each variable.
\cr
\cr
A table may still be constructed by applying \code{\link{rbind}} to individual calls to \code{\link{cat.var}} and \code{\link{cont.var}}, and generating HTML or LaTeX output using \code{\link{out.html}} or \code{\link{out.latex}}; \code{\link{make.table}} serves as a wrapper for these functions.
}


\seealso{
\code{\link{Table1}}
\code{\link{cat.var}}
\code{\link{cont.var}}
\code{\link{out.plain}}
\code{\link{out.latex}}
\code{\link{out.html}}
}

\examples{
library(survival)
attach(pbc)
make.table(dat=pbc,
    cat.varlist=c('stage', 'trt'),
    cat.header=c('Stage', 'Treatment'),
    cat.rownames=list(c('I', 'II', 'III', 'IV'), c('D-penicillmain', 'Placebo')),
    cat.ptype=c('fisher', 'fisher'),   
    cont.varlist=c('bili', 'copper'),
    cont.header=c('Bilirubin', 'Urine copper'),
    cont.ptype=c('wilcox', 'wilcox'), 
    strat=c('sex'),
    colnames=c('', 'Male', 'Female', 'Overall', 'p-value')
    )
}
