\name{stat.col}
\alias{stat.col}

\title{Statistical Testing}

\description{This function produces vectors of p-values and tests for addition via ptype= to a summary table generated with \code{\link{make.table}}. P-values are rounded to three decimal places or formatted as '<0.001' as appropriate. The name of the test is printed if requested with pname=TRUE.
}
\usage{
stat.col(var, strat, ptype, pname, output)
}

\arguments{
  \item{var}{
  Variable input. Required.
  }
  \item{strat}{
  Column stratifying variable. Required for two-sample tests.
  }
  \item{ptype}{
  Test requested to compute p-value. Required.
      \describe{
        \item{'t.oneway'}{One-sample t-test}
        \item{'wilcox.oneway'}{One-sample test for median=0}
        \item{'prop.oneway'}{Binomial test for equal proportions (prop=0.5)}
        
        \item{'chisq'}{Chi-square test}
        \item{'fisher'}{Fisher's exact test}
                
        \item{'ttest'}{t-test}
        \item{'anova'}{One-way ANOVA}
        \item{'kruskal'}{Kruskal-Wallis test}
        \item{'wilcox'}{Wilcoxon rank-sum test}
        
        \item{'ttest.pair'}{Paired t-test}
        \item{'wilcox.pair'}{Wilcoxon signed-rank test}
        \item{'mcnemar'}{McNemar's test}
      }
  }
  \item{pname}{
  If TRUE then the name of the statistical test used will be printed below the p-value. Optional; defaults to FALSE.
  }
  \item{output}{
  Type of output, passed from \code{\link{make.table}} or \code{\link{quick.table}}. Optional; defaults to "plain".
  }
}

\value{Returns a vector with a p-value and the name of the test used, if requested, to be added to a summary data frame.}

\author{Erica Wozniak}

\seealso{
  \code{\link{Table1}}
  \code{\link{make.table}}
  \code{\link{quick.table}}
  \code{\link{cat.var}}
  \code{\link{cont.var}}
  \code{\link{out.plain}}
  \code{\link{out.latex}}
  \code{\link{out.html}}
}

\examples{
library(survival)
attach(pbc)

##Default output
stat.col(var=bili, 
         strat=sex, 
         ptype='ttest')
        
##Print test
stat.col(var=bili, 
         strat=sex, 
         ptype='ttest',
         pname=TRUE)
        
##The ptype= and pname= options in summary functions call stat.col
#Results are automatically added to the summary table
cat.var(var=hepato, 
        strat=sex, 
        rownames=c('Not present', 'Present'),
        header=c('Hepatomegaly'),
        ptype='chisq',
        pname=TRUE
        )       
}
