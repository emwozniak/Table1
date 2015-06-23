\name{cat.var}
\alias{cat.var}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Categorical Variable Summary
%%  ~~function to do ... ~~
}
\description{This function produces counts, row percentages, column percentages, and counts of missing values for each level of a categorical variable input, optionally stratified by another variable. Chi-square, Fisher's exact, and McNemar's tests are currently supported for the addition of p-values to tables.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cat.var(var, strat, dec, rownames, header, ptype, pname)
%cat.var(var, strat = NULL, dec = 2, rownames = as.vector(levels(as.factor(var))), header = deparse%(substitute(var)), ptype = "None", pname = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
  Categorical variable input (required)
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
  \item{rownames}{
  Vector of row names for each level of 'var' (optional; defaults to the value of each level)
%%     ~~Describe \code{rownames} here~~
}
  \item{header}{
  Vector of a short variable description (optional; defaults to variable name)
%%     ~~Describe \code{header} here~~
}
  \item{ptype}{
  Statistical test for p-value (optional; defaults to none) \cr
    'chisq' - Chi-square test \cr
    'fisher' - Fisher's exact test \cr
    'mcnemar' - McNemar's test
%%     ~~Describe \code{ptype} here~~
}
  \item{pname}{
  If TRUE then the name of the statistical test will be printed below the p-value (optional; defaults to FALSE)
%%     ~~Describe \code{pname} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame of categorical summary statistics.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Erica Wozniak
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Table1}}
\code{\link{cont.var}}
\code{\link{out.plain}}
\code{\link{out.latex}}
\code{\link{out.html}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ univar }
%\keyword{ misc }% __ONLY ONE__ keyword per line