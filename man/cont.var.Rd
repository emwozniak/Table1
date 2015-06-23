\name{cont.var}
\alias{cont.var}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Continuous Variable Summary
%%  ~~function to do ... ~~
}
\description{
This function produces mean (SD), median (IQR), Q1/Q3, min/max, and a count of missings for a continuous variable input, optionally stratified by another variable. Paired and unpaired t-tests, Wilcoxon rank sum, Wilcoxon signed-rank, one-way between-subjects ANOVA, and Kruskal-Wallis tests are currently supported for the addition of p-values to tables.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cont.var(var, strat, dec, header, ptype, pname)
%cont.var(var, strat = NULL, dec = 2, header = deparse(substitute(var)), ptype = "None", pname = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
  Continuous variable input (required)  
%%     ~~Describe \code{var} here~~
}
  \item{strat}{
  Variable to stratify by across columns (optional)
%%     ~~Describe \code{strat} here~~
}
  \item{dec}{
  Number of decimal places on summary statistics (optional; defaults to 2)
%%     ~~Describe \code{dec} here~~
}
  \item{header}{
  Short variable description (optional; defaults to variable name)
%%     ~~Describe \code{header} here~~
}
  \item{ptype}{
  Statistical test for p-value (optional; defaults to none) \cr
    'anova' - One-way between-subjects ANOVA \cr
    'kruskal' - Kruskal-Wallis test \cr
    'ttest' - Student's t-test \cr
    'ttest.pair' - Paired t-test \cr
    'wilcox' - Wilcoxon rank sum test \cr
    'wilcox.pair' - Wilcoxon signed-rank test 
  
%%     ~~Describe \code{ptype} here~~
}
  \item{pname}{
  If TRUE then the name of the statistical test will be printed below the p-value (optional; defaults to FALSE)
%%     ~~Describe \code{pname} here~~
}
}
\details{ Note: Warnings are not in place for the improper use of significance testing. For example, if a t-test is requested with more than two strata, the code will execute and the returned p-value will reflect a test of only the first two strata.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame of continuous summary statistics.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Erica Wozniak
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Table1}}
\code{\link{cat.var}}
\code{\link{out.plain}}
\code{\link{out.latex}}
\code{\link{out.html}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(survival)
attach(pbc)
cont.var(var=age,
         strat=sex,
         header='Age',
         ptype='ttest')

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ univar }
%\keyword{ misc }% __ONLY ONE__ keyword per line
}