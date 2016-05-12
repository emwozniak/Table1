\name{cat.var}
\alias{cat.var}

\title{Categorical Variable Summary}

\description{This function produces counts, row percentages, column percentages, and counts of missing values for each level of a categorical variable input, optionally stratified by another variable, and with the option to compute p-values. 
}
\usage{
cat.var(var, strat, dec, rownames, header, ptype, pname, cat.rmstat, vspace, output)
}

\arguments{
  \item{var}{
  Categorical variable input. Required.
  }
  \item{strat}{
  Column stratifying variable. Optional.
  }
  \item{dec}{
  Number of decimal places on summary statistics. Optional; defaults to 2.
  }
  \item{rownames}{
  Ordered character vector of row names for each level of 'var'. Optional; defaults to the value of each factor level.
  }
  \item{header}{
  Character vector providing a short variable description. Optional; defaults to variable name.
  }
  \item{ptype}{
  Statistical test for p-value. Optional; defaults to none. See \code{\link{stat.col}} for a complete list of options.
  }
  \item{pname}{
  If TRUE then the name of the statistical test used will be printed below the p-value. Optional; defaults to FALSE.
  }
  \item{cat.rmstat}{
  Vector naming the statistics that should be removed from the summary data frame. Optional; defaults to none. \cr
      \describe{
        \item{"count"}{Remove overall count/percent}
        \item{"row"}{Remove row percents, leaving only column percents}
        \item{"col"}{Remove column percents, leaving only row percents}
        \item{"miss"}{Remove summary of missing values}    
      }
  }
  \item{vspace}{
  TRUE for additional vertical whitespace between variables (default).
  }
  \item{output}{
  Output type passed from \code{\link{make.table}}.
  }
}

\value{Returns a data frame of categorical summary statistics.}

\author{Erica Wozniak}

\seealso{
  \code{\link{Table1}}
  \code{\link{cont.var}}
  \code{\link{make.table}}
  \code{\link{quick.table}}
  \code{\link{stat.col}}
  \code{\link{out.plain}}
  \code{\link{out.latex}}
  \code{\link{out.html}}
}

\examples{
library(survival)

# Default output
cat.var(var   = hepato, 
        strat = sex
        )
        
# Naming options and p-value
cat.var(var      = hepato, 
        strat    = sex, 
        rownames = c("Not present", "Present"),
        header   = c("Hepatomegaly"),
        ptype    = "chisq"
        )
        
# Remove row percents, missing values, and name of statistical test used
cat.var(var        = hepato, 
        strat      = sex, 
        rownames   = c("Not present", "Present"),
        header     = c("Hepatomegaly"),
        ptype      = "chisq",
        cat.rmstat = c("row", "miss"),
        pname      = FALSE
        )       

}
