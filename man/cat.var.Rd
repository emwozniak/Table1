\name{cat.var}
\alias{cat.var}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cat.var(var, strat = NULL, dec = 2, rownames = as.vector(levels(as.factor(var))), header = deparse(substitute(var)), ptype = "None", pname = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{strat}{
%%     ~~Describe \code{strat} here~~
}
  \item{dec}{
%%     ~~Describe \code{dec} here~~
}
  \item{rownames}{
%%     ~~Describe \code{rownames} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{ptype}{
%%     ~~Describe \code{ptype} here~~
}
  \item{pname}{
%%     ~~Describe \code{pname} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (var, strat = NULL, dec = 2, rownames = as.vector(levels(as.factor(var))), 
    header = deparse(substitute(var)), ptype = "None", pname = FALSE) 
{
    if (is.null(strat)) {
        tot <- as.matrix(table(var))
        tot[] <- paste0(tot, paste0(" (", format(round((tot/rowSums(tot)) * 
            100, dec), nsmall = dec), ")", paste0(" (", format(round((tot/colSums(tot)) * 
            100, dec), nsmall = dec), ")")))
        miss <- length(which(is.na(var) == T))
        out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 
            1), tot, miss)), as.character)
        out <- replace(out, is.na(out), "")
        out <- cbind(as.vector(c(paste(header, "     "), paste0("   ", 
            rownames), "   Missing")), out)
        rownames(out) <- NULL
        colnames(out) <- c("Variable", "Overall")
    }
    else {
        if (!is.null(strat)) {
            cat <- as.matrix(table(var, as.factor(strat)))
            tot <- as.matrix(table(var))
            cat[] <- paste0(cat, paste0(" (", format(round((cat/rowSums(cat)) * 
                100, dec), nsmall = dec), ")", paste0(" (", format(round((cat/colSums(cat)) * 
                100, dec), nsmall = dec), ")")))
            tot[] <- paste0(tot, paste0(" (", format(round((tot/rowSums(tot)) * 
                100, dec), nsmall = dec), ")", paste0(" (", format(round((tot/colSums(tot)) * 
                100, dec), nsmall = dec), ")")))
            miss <- c(aggregate(var, list(strat), function(x) {
                sum(is.na(x))
            })[, -1], sum(aggregate(var, list(strat), function(x) {
                sum(is.na(x))
            })[, -1]))
            out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 
                1), cbind(cat, tot), miss)), as.character)
            out <- replace(out, is.na(out), "")
            out <- cbind(as.vector(c(paste(header, "     "), 
                paste0("   ", rownames), "   Missing")), out)
            rownames(out) <- NULL
            colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
                "Overall")
        }
    }
    if (ptype == "chisq") {
        p <- chisq.test(var, strat)$p.value
        if (p >= 1e-04 & pname == TRUE) {
            p.col <- c(format(round(p, 4), nsmall = 4), "Chi-square", 
                rep(NA, length(levels(as.factor(var)))))
        }
        else {
            if (p < 1e-04 & pname == TRUE) {
                p.col <- c("<0.0001", "Chi-square", rep(NA, length(levels(as.factor(var)))))
            }
            else {
                if (p >= 1e-04 & pname == FALSE) {
                  p.col <- c(format(round(p, 4), nsmall = 4), 
                    rep(NA, length(levels(as.factor(var))) + 
                      1))
                }
                else {
                  if (p < 1e-04 & pname == FALSE) {
                    p.col <- c("<0.0001", rep(NA, length(levels(as.factor(var))) + 
                      1))
                  }
                }
            }
        }
        out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 
            1), cbind(cat, tot), miss)), p.col), as.character)
        out <- replace(out, is.na(out), "")
        out <- cbind(as.vector(c(paste(header, "     "), paste0("   ", 
            rownames), "   Missing")), out)
        rownames(out) <- NULL
        colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
            "Overall", "p-value")
    }
    else {
        if (ptype == "fisher") {
            p <- fisher.test(var, strat)$p.value
            if (p >= 1e-04 & pname == TRUE) {
                p.col <- c(format(round(p, 4), nsmall = 4), "Fisher exact", 
                  rep(NA, length(levels(as.factor(var)))))
            }
            else {
                if (p < 1e-04 & pname == TRUE) {
                  p.col <- c("<0.0001", "Fisher exact", rep(NA, 
                    length(levels(as.factor(var)))))
                }
                else {
                  if (p >= 1e-04 & pname == FALSE) {
                    p.col <- c(format(round(p, 4), nsmall = 4), 
                      rep(NA, length(levels(as.factor(var))) + 
                        1))
                  }
                  else {
                    if (p < 1e-04 & pname == FALSE) {
                      p.col <- c("<0.0001", rep(NA, length(levels(as.factor(var))) + 
                        1))
                    }
                  }
                }
            }
            out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 
                1), cbind(cat, tot), miss)), p.col), as.character)
            out <- replace(out, is.na(out), "")
            out <- cbind(as.vector(c(paste(header, "     "), 
                paste0("   ", rownames), "   Missing")), out)
            rownames(out) <- NULL
            colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
                "Overall", "p-value")
        }
        else {
            if (ptype == "mcnemar") {
                p <- mcnemar.test(var, strat)$p.value
                if (p >= 1e-04 & pname == TRUE) {
                  p.col <- c(format(round(p, 4), nsmall = 4), 
                    "McNemar", rep(NA, length(levels(as.factor(strat)))))
                }
                else {
                  if (p < 1e-04 & pname == TRUE) {
                    p.col <- c("<0.0001", "McNemar", rep(NA, 
                      length(levels(as.factor(strat)))))
                  }
                  else {
                    if (p >= 1e-04 & pname == FALSE) {
                      p.col <- c(format(round(p, 4), nsmall = 4), 
                        rep(NA, length(levels(as.factor(strat))) + 
                          1))
                    }
                    else {
                      if (p < 1e-04 & pname == FALSE) {
                        p.col <- c("<0.0001", rep(NA, length(levels(as.factor(strat))) + 
                          1))
                      }
                    }
                  }
                }
                out <- sapply(data.frame(cbind(rbind(rep(NA, 
                  length(levels(as.factor(strat))) + 1), cbind(cat, 
                  tot), miss)), p.col), as.character)
                out <- replace(out, is.na(out), "")
                out <- cbind(as.vector(c(paste(header, "     "), 
                  paste0("   ", rownames), "   Missing")), out)
                rownames(out) <- NULL
                colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
                  "Overall", "p-value")
            }
        }
    }
    return(data.frame(out))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
