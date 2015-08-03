\name{cat.var}
\alias{cat.var}

\title{Categorical Variable Summary}

\description{This function produces counts, row percentages, column percentages, and counts of missing values for each level of a categorical variable input, optionally stratified by another variable, and with the option to compute p-values. 
}
\usage{
cat.var(var, strat, dec, rownames, header, ptype, pname, cat.rmstat)
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
  Ordered character vector of row names for each level of 'var'. Optional; defaults to the value of each level.
  }
  \item{header}{
  Character vector providing a short variable description. Optional; defaults to variable name.
  }
  \item{ptype}{
  Statistical test for p-value. Optional; defaults to none. See \code{\link{stat.col}} for a complete list of options.\cr
      \describe{
        \item{'chisq'}{Chi-square test}
        \item{'fisher'}{Fisher's exact test}
        \item{'mcnemar'}{McNemar's test}
      }
  }
  \item{pname}{
  If TRUE then the name of the statistical test used will be printed below the p-value. Optional; defaults to FALSE.
  }
  \item{cat.rmstat}{
  Vector naming the statistics that should be removed from the summary data frame. Optional; defaults to none. \cr
      \describe{
        \item{'count'}{Remove overall count/percent}
        \item{'row'}{Remove row percents, leaving only column percents}
        \item{'col'}{Remove column percents, leaving only row percents}
        \item{'miss'}{Remove summary of missing values}    
      }
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
attach(pbc)

##Default output
cat.var(var=hepato, 
        strat=sex
        )
        
##Naming options and p-value
cat.var(var=hepato, 
        strat=sex, 
        rownames=c('Not present', 'Present'),
        header=c('Hepatomegaly'),
        ptype='chisq',
        pname=TRUE
        )
        
##Remove row percents and tabulated missing values
cat.var(var=hepato, 
        strat=sex, 
        rownames=c('Not present', 'Present'),
        header=c('Hepatomegaly'),
        ptype='chisq',
        cat.rmstat=c('row', 'miss')
        )       

}
