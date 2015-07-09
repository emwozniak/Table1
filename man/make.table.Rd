\name{make.table}
\alias{make.table}

\title{Make Summary Table}

\description{This function produces summary statistics for 
}
\usage{
make.table(dat, cat.varlist, cat.header, cat.rownames, cat.ptype,
cont.varlist, cont.header, cont.ptype, strat, dec, pname, 
colnames, output)
%The full call with defaults is given as follows:
%make.table(dat, 
    %cat.varlist=NULL, 
    %cat.header=names(sapply(cat.varlist, FUN=get, simplify=F, USE.NAMES=T)),
    %cat.rownames=lapply(sapply(cat.varlist, FUN=get, simplify=F, USE.NAMES=T), FUN=function(x) 
                         %as.vector(levels(as.factor(x)))),
    %cat.ptype='None',
                       
    %cont.varlist=NULL, 
    %cont.header=names(sapply(cont.varlist, FUN=get, simplify=F, USE.NAMES=T)),
    %cont.ptype='None',
                       
    %strat=NULL,
    %dec=2,
    %pname=FALSE,
    %colnames=NULL,
    %output='plain'
    %)
}

\arguments{
  \item{dat}{
  Data frame input of all variables used for the summary table (required)
  }
  \item{cat.varlist}{
  Vector of categorical variable names (optional; defaults to NULL)
  }
  \item{cat.header}{
  Ordered vector of short descriptions for each categorical variable (optional; defaults to variable name)
  }
  \item{cat.rownames}{
  Ordered list of vectors of row names for each level of each vector in cat.varlist (optional; defaults to the coded value of each level)
  }
  \item{cat.ptype}{
  Ordered vector of tests requested to add p-values to table for categorical variables (optional; defaults to no p-values) \cr
    'chisq' - Chi-square test \cr
    'fisher' - Fisher's exact test \cr
    'mcnemar' - McNemar's test
  }
  \item{cont.varlist}{
  Vector of continuous variable names (optional; defaults to NULL)

  }
  \item{cont.header}{
  Ordered vector of short descriptions for each continuous variable (optional; defaults to variable name)
  }
  \item{cont.ptype}{
    Ordered vector of tests requested to add p-values to table for continuous variables (optional; defaults to no p-values) \cr
    'anova' - One-way between-subjects ANOVA \cr
    'kruskal' - Kruskal-Wallis test \cr
    'ttest' - t-test \cr
    'ttest.pair' - Paired t-test \cr
    'wilcox' - Wilcoxon rank sum test \cr
    'wilcox.pair' - Wilcoxon signed rank test

  }
  \item{strat}{
  Vector of one or more stratifying variables (optional; defaults to no stratification)
  }
  \item{dec}{
  Integer value for number of decimal places to be printed for each summary statistic (optional; defaults to 2)
  }
  \item{pname}{
  Logical indicator to print statistical test used beneath each p-value (optional; defaults to FALSE)
  }
  \item{colnames}{
  Ordered vector of column names for table (optional; defaults to levels of strata variable(s))
  }
  \item{output}{
  Choice of formatted table output: 'html', 'latex', or 'plain' (optional; defaults to plain text printed in console)
  }
}

\value{
Returns a table of summary statistics for specified categorical and continuous variables in the requested output format.
}

\author{
Erica Wozniak
}

\note{
A few miscellaneous notes: \cr
*The user must ensure that no variable names share the name of a base R function; otherwise, the program will throw an error. \cr
*By default, the final summary table is rearranged such that the variables in the table are in the same order as in the raw data set (dat=). \cr
*If missing values occur in stratifying variables (strat=), then the observations with missings will be removed from the table, and a comment will be printed indicating the total number removed. If multiple stratifying variables are provided, look to the console for a summary of total missing observations for each variable.
*The make.table function provides the option for vectorized calls to cat.var and cont.var, as well as the option to call out.plain, out.html, or out.latex for ease of output; these individual functions are not deprecated. That is, a table may still be constructed by applying \code{\link{rbind}} to individual calls to \code{\link{cat.var}} and \code{\link{cont.var}}, and generating HTML or LaTeX output using \code{\link{out.html}} or \code{\link{out.latex}}.

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
