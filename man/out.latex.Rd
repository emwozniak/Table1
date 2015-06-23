\name{out.latex}
\alias{out.latex}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{LaTeX Output
%%  ~~function to do ... ~~
}
\description{This function utilizes xtable to output the results of the cat.var and cont.var functions to the console as formatted LaTeX code, and may be used within a Sweave or knitr document.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
out.latex(tab, colnames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{A single call to either cat.var or cont.var, or a table produced by rbind of multiple calls to these functions (required)
%%     ~~Describe \code{tab} here~~
}
  \item{colnames}{A vector of column names for the table output (optional)
%%     ~~Describe \code{colnames} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{LaTeX code for a formatted table.
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
\code{\link{cont.var}}
\code{\link{out.plain}}
\code{\link{out.html}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(survival)
library(xtable)
attach(pbc)
example <- rbind(
  cont.var(var=age, strat=trt, header='Age', ptype='ttest'),
  cat.var(var=sex, strat=trt, rownames=c('Male', 'Female'), header='Sex', ptype='chisq'),
  cont.var(var=bili, strat=trt, header='Serum bilirubin', ptype='ttest')
  )
out.latex(example, colnames=c('Variable', 'D-penicillmain', 'Placebo', 'Overall', 'p-value'))

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
}