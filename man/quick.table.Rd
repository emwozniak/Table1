\name{quick.table}
\alias{quick.table}

\title{Quick Summary Table}

\description{Similarly to \code{\link{make.table}}, this function produces summary statistics for categeorical and continuous variables and combines the results into a data frame for plain text, HTML, or LaTeX output. However, minimal input (a data frame only) is required to construct the table, as this function dynamically classifies variables as either categorical or continuous. This function is ideal for rapid summaries of data frames, but \code{\link{make.table}} should be used instead if custom labeling and significance testing are desired.
}
\usage{
quick.table(dat, strat, dec, colnames, output, classlim)
}

\arguments{
  \item{dat}{
  Data frame input of all variables used for the summary table. Required.
  }
  \item{strat}{
  Character vector of names of one or more stratifying variables. Optional; defaults to no stratification.
  }
  \item{dec}{
    Integer value for number of decimal places to be printed for each summary statistic. Optional; defaults to 2.
  }
  \item{colnames}{
  Ordered vector of column names for table. Optional; defaults to levels of strata variable(s).
  }
  \item{output}{
  Choice of formatted table output: 'html', 'latex', or 'plain'. Optional; defaults to plain text printed in console.
  }
  \item{classlim}{
  Integer classification threshold for categorical and continuous variables. Optional; defaults to numeric variables with 6 or more levels being classified as continuous, and categorical otherwise. All non-numeric variables are classified as categorical.
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
Table output to HTML and LaTeX require the package dependencies \code{\link{htmlTable}} and \code{\link{xtable}}
\cr
\cr
The user must ensure that no variables share the names of any base R functions. This function throws out such variables with a warning to continue compiling.
\cr
\cr
By default, the final summary table is rearranged such that the variables in the table are in the same order as in the raw data set (dat=). 
\cr
\cr
If missing values occur in stratifying variables (strat=), then the observations with missings will be removed from the table, and a comment will be printed indicating the total number removed. If multiple stratifying variables are provided, look to the console for a summary of total missing observations for each variable.
\cr
\cr
For additional labeling and significance testing options, see \code{\link{make.table}}.
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
quick.table(dat=pbc,
    strat='sex',
    colnames=c('', 'Male', 'Female', 'Overall')
    )
}
