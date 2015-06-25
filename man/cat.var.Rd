\name{cat.var}
\alias{cat.var}

\title{Categorical Variable Summary}

\description{This function produces counts, row percentages, column percentages, and counts of missing values for each level of a categorical variable input, optionally stratified by another variable. Chi-square, Fisher's exact, and McNemar's tests are currently supported for the addition of p-values to tables.
}
\usage{
cat.var(var, strat, dec, rownames, header, ptype, pname)
%The full call with defaults is given as follows:
%cat.var(var, strat = NULL, dec = 2, rownames = as.vector(levels(as.factor(var))), header = deparse%(substitute(var)), ptype = "None", pname = FALSE)
}

\arguments{
  \item{var}{
  Categorical variable input (required)
  }
  \item{strat}{
  Variable to stratify by across columns (optional)
  }
  \item{dec}{
  Number of decimal places on summary statistics (optional; defaults to 2)
  }
  \item{rownames}{
  Vector of row names for each level of 'var' (optional; defaults to the value of each level)
  }
  \item{header}{
  Vector of a short variable description (optional; defaults to variable name)
  }
  \item{ptype}{
  Statistical test for p-value (optional; defaults to none) \cr
    'chisq' - Chi-square test \cr
    'fisher' - Fisher's exact test \cr
    'mcnemar' - McNemar's test
  }
  \item{pname}{
  If TRUE then the name of the statistical test will be printed below the p-value (optional; defaults to FALSE)
  }
}

\value{
Returns a data frame of categorical summary statistics.
}

\author{
Erica Wozniak
}


\seealso{
\code{\link{Table1}}
\code{\link{cont.var}}
\code{\link{out.plain}}
\code{\link{out.latex}}
\code{\link{out.html}}
}

\examples{
library(survival)
attach(pbc)
cat.var(var=hepato, 
        strat=sex, 
        rownames=c('Not present', 'Present'),
        header=c('Hepatomegaly'),
        ptype='chisq')
}
