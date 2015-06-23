\name{cont.var}
\alias{cont.var}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cont.var(var, strat = NULL, dec = 2, header = deparse(substitute(var)), ptype = "None", pname = FALSE)
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
function (var, strat = NULL, dec = 2, header = deparse(substitute(var)), 
    ptype = "None", pname = FALSE) 
{
    if (is.null(strat)) {
        mean.sd.tot <- paste(format(round(mean(var, na.rm = T), 
            dec), nsmall = dec), paste("(", format(round(sd(var, 
            na.rm = T), dec), nsmall = dec), ")", sep = ""))
        med.iqr.tot <- paste(format(round(median(var, na.rm = T), 
            dec), nsmall = dec), paste("(", format(round(IQR(var, 
            na.rm = T), dec), nsmall = dec), ")", sep = ""))
        q1.q3.tot <- paste(format(round(quantile(var, 0.25, na.rm = T), 
            dec), nsmall = 2), ", ", format(round(quantile(var, 
            0.75, na.rm = T), dec)), sep = "")
        min.max.tot <- paste(format(round(min(var, na.rm = T), 
            dec), nsmall = dec), ", ", format(round(max(var, 
            na.rm = T), 2)), sep = "")
        miss.tot <- length(which(is.na(var) == T))
        out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 
            1), mean.sd.tot, med.iqr.tot, q1.q3.tot, min.max.tot, 
            miss.tot)), as.character)
        out <- replace(out, is.na(out), "")
        out <- cbind(as.vector(c(paste(header, "     "), "   Mean (SD)", 
            "   Median (IQR)", "   Q1, Q3", "   Min, Max", "   Missing")), 
            out)
        rownames(out) <- NULL
        colnames(out) <- c("Variable", "Overall")
    }
    else {
        if (!is.null(strat)) {
            mean.sd <- format(round(aggregate(var, list(strat), 
                mean, na.rm = T)[, -1], dec), nsmall = dec)
            mean.sd[] <- paste0(mean.sd, paste0(" (", (format(round(aggregate(var, 
                list(strat), sd, na.rm = T)[, -1], dec), nsmall = dec)), 
                ")"))
            med.iqr <- format(round(aggregate(var, list(strat), 
                median, na.rm = T)[, -1], dec), nsmall = dec)
            med.iqr[] <- paste0(med.iqr, paste0(" (", (format(round(aggregate(var, 
                list(strat), IQR, na.rm = T)[, -1], dec), nsmall = dec)), 
                ")"))
            q1.q3 <- format(round(aggregate(var, list(strat), 
                quantile, 0.25, na.rm = T)[, -1], dec), nsmall = dec)
            q1.q3[] <- paste0(q1.q3, paste0(", ", (format(round(aggregate(var, 
                list(strat), quantile, 0.75, na.rm = T)[, -1], 
                dec), nsmall = dec))))
            min.max <- format(round(aggregate(var, list(strat), 
                min, na.rm = T)[, -1], dec), nsmall = dec)
            min.max[] <- paste0(min.max, paste0(", ", (format(round(aggregate(var, 
                list(strat), max, na.rm = T)[, -1], dec), nsmall = dec))))
            miss <- aggregate(var, list(strat), function(x) {
                sum(is.na(x))
            })[, -1]
            cont <- rbind(mean.sd, med.iqr, q1.q3, min.max, miss)
            mean.sd.tot <- paste(format(round(mean(var, na.rm = T), 
                dec), nsmall = dec), paste("(", format(round(sd(var, 
                na.rm = T), dec), nsmall = dec), ")", sep = ""))
            med.iqr.tot <- paste(format(round(median(var, na.rm = T), 
                dec), nsmall = dec), paste("(", format(round(IQR(var, 
                na.rm = T), dec), nsmall = dec), ")", sep = ""))
            q1.q3.tot <- paste(format(round(quantile(var, 0.25, 
                na.rm = T), dec), nsmall = dec), ", ", format(round(quantile(var, 
                0.75, na.rm = T), dec), nsmall = dec), sep = "")
            min.max.tot <- paste(format(round(min(var, na.rm = T), 
                dec), nsmall = dec), ", ", format(round(max(var, 
                na.rm = T), dec), nsmall = dec), sep = "")
            miss.tot <- length(which(is.na(var) == T))
            tot <- rbind(mean.sd.tot, med.iqr.tot, q1.q3.tot, 
                min.max.tot, miss.tot)
            out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 
                1), cbind(cont, tot))), as.character)
            out <- replace(out, is.na(out), "")
            out <- cbind(as.vector(c(paste(header, "     "), 
                "   Mean (SD)", "   Median (IQR)", "   Q1, Q3", 
                "   Min, Max", "   Missing")), out)
            rownames(out) <- NULL
            colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
                "Overall")
        }
    }
    if (ptype == "ttest") {
        p <- t.test(var[strat == levels(as.factor(strat))[1]], 
            var[strat == levels(as.factor(strat))[2]])$p.value
        if (p >= 1e-04 & pname == TRUE) {
            p.col <- c(format(round(p, 4), nsmall = 4), "t-test", 
                rep(NA, 4))
        }
        else {
            if (p < 1e-04 & pname == TRUE) {
                p.col <- c("<0.0001", "t-test", rep(NA, 4))
            }
            else {
                if (p >= 1e-04 & pname == FALSE) {
                  p.col <- c(format(round(p, 4), nsmall = 4), 
                    rep(NA, 5))
                }
                else {
                  if (p < 1e-04 & pname == FALSE) {
                    p.col <- c("<0.0001", rep(NA, 5))
                  }
                }
            }
        }
        out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 
            1), cbind(cont, tot)), p.col)), as.character)
        out <- replace(out, is.na(out), "")
        out <- cbind(as.vector(c(paste(header, "     "), "   Mean (SD)", 
            "   Median (IQR)", "   Q1, Q3", "   Min, Max", "   Missing")), 
            out)
        rownames(out) <- NULL
        colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
            "Overall", "p-value")
    }
    else {
        if (ptype == "ttest.pair") {
            p <- t.test(var[strat == levels(as.factor(strat))[1]], 
                var[strat == levels(as.factor(strat))[2]], paired = TRUE)$p.value
            if (p >= 1e-04 & pname == TRUE) {
                p.col <- c(format(round(p, 4), nsmall = 4), "Paired t-test", 
                  rep(NA, 4))
            }
            else {
                if (p < 1e-04 & pname == TRUE) {
                  p.col <- c("<0.0001", "Paired t-test", rep(NA, 
                    4))
                }
                else {
                  if (p >= 1e-04 & pname == FALSE) {
                    p.col <- c(format(round(p, 4), nsmall = 4), 
                      rep(NA, 5))
                  }
                  else {
                    if (p < 1e-04 & pname == FALSE) {
                      p.col <- c("<0.0001", rep(NA, 5))
                    }
                  }
                }
            }
            out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 
                1), cbind(cont, tot)), p.col)), as.character)
            out <- replace(out, is.na(out), "")
            out <- cbind(as.vector(c(paste(header, "     "), 
                "   Mean (SD)", "   Median (IQR)", "   Q1, Q3", 
                "   Min, Max", "   Missing")), out)
            rownames(out) <- NULL
            colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
                "Overall", "p-value")
        }
        else {
            if (ptype == "wilcox") {
                p <- wilcox.test(var[strat == levels(as.factor(strat))[1]], 
                  var[strat == levels(as.factor(strat))[2]])$p.value
                if (p >= 1e-04 & pname == TRUE) {
                  p.col <- c(format(round(p, 4), nsmall = 4), 
                    "Wilcoxon rank sum", rep(NA, 4))
                }
                else {
                  if (p < 1e-04 & pname == TRUE) {
                    p.col <- c("<0.0001", "Wilcoxon rank sum", 
                      rep(NA, 4))
                  }
                  else {
                    if (p >= 1e-04 & pname == FALSE) {
                      p.col <- c(format(round(p, 4), nsmall = 4), 
                        rep(NA, 5))
                    }
                    else {
                      if (p < 1e-04 & pname == FALSE) {
                        p.col <- c("<0.0001", rep(NA, 5))
                      }
                    }
                  }
                }
                out <- sapply(data.frame(cbind(rbind(rep(NA, 
                  length(levels(as.factor(strat))) + 1), cbind(cont, 
                  tot)), p.col)), as.character)
                out <- replace(out, is.na(out), "")
                out <- cbind(as.vector(c(paste(header, "     "), 
                  "   Mean (SD)", "   Median (IQR)", "   Q1, Q3", 
                  "   Min, Max", "   Missing")), out)
                rownames(out) <- NULL
                colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
                  "Overall", "p-value")
            }
            else {
                if (ptype == "wilcox.pair") {
                  p <- wilcox.test(var[strat == levels(as.factor(strat))[1]], 
                    var[strat == levels(as.factor(strat))[2]], 
                    paired = TRUE)$p.value
                  if (p >= 1e-04 & pname == TRUE) {
                    p.col <- c(format(round(p, 4), nsmall = 4), 
                      "Paired Wilcoxon", rep(NA, 4))
                  }
                  else {
                    if (p < 1e-04 & pname == TRUE) {
                      p.col <- c("<0.0001", "Paired Wilcoxon", 
                        rep(NA, 4))
                    }
                    else {
                      if (p >= 1e-04 & pname == FALSE) {
                        p.col <- c(format(round(p, 4), nsmall = 4), 
                          rep(NA, 5))
                      }
                      else {
                        if (p < 1e-04 & pname == FALSE) {
                          p.col <- c("<0.0001", rep(NA, 5))
                        }
                      }
                    }
                  }
                  out <- sapply(data.frame(cbind(rbind(rep(NA, 
                    length(levels(as.factor(strat))) + 1), cbind(cont, 
                    tot)), p.col)), as.character)
                  out <- replace(out, is.na(out), "")
                  out <- cbind(as.vector(c(paste(header, "     "), 
                    "   Mean (SD)", "   Median (IQR)", "   Q1, Q3", 
                    "   Min, Max", "   Missing")), out)
                  rownames(out) <- NULL
                  colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
                    "Overall", "p-value")
                }
                else {
                  if (ptype == "kruskal") {
                    p <- kruskal.test(var, strat)$p.value
                    if (p >= 1e-04 & pname == TRUE) {
                      p.col <- c(format(round(p, 4), nsmall = 4), 
                        "Kruskal-Wallis", rep(NA, 4))
                    }
                    else {
                      if (p < 1e-04 & pname == TRUE) {
                        p.col <- c("<0.0001", "Kruskal-Wallis", 
                          rep(NA, 4))
                      }
                      else {
                        if (p >= 1e-04 & pname == FALSE) {
                          p.col <- c(format(round(p, 4), nsmall = 4), 
                            rep(NA, 5))
                        }
                        else {
                          if (p < 1e-04 & pname == FALSE) {
                            p.col <- c("<0.0001", rep(NA, 5))
                          }
                        }
                      }
                    }
                    out <- sapply(data.frame(cbind(rbind(rep(NA, 
                      length(levels(as.factor(strat))) + 1), 
                      cbind(cont, tot)), p.col)), as.character)
                    out <- replace(out, is.na(out), "")
                    out <- cbind(as.vector(c(paste(header, "     "), 
                      "   Mean (SD)", "   Median (IQR)", "   Q1, Q3", 
                      "   Min, Max", "   Missing")), out)
                    rownames(out) <- NULL
                    colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
                      "Overall", "p-value")
                  }
                  else {
                    if (ptype == "anova") {
                      p <- summary(aov(var ~ strat))[[1]][["Pr(>F)"]][[1]]
                      if (p >= 1e-04 & pname == TRUE) {
                        p.col <- c(format(round(p, 4), nsmall = 4), 
                          "ANOVA", rep(NA, 4))
                      }
                      else {
                        if (p < 1e-04 & pname == TRUE) {
                          p.col <- c("<0.0001", "ANOVA", rep(NA, 
                            4))
                        }
                        else {
                          if (p >= 1e-04 & pname == FALSE) {
                            p.col <- c(format(round(p, 4), nsmall = 4), 
                              rep(NA, 5))
                          }
                          else {
                            if (p < 1e-04 & pname == FALSE) {
                              p.col <- c("<0.0001", rep(NA, 5))
                            }
                          }
                        }
                      }
                      out <- sapply(data.frame(cbind(rbind(rep(NA, 
                        length(levels(as.factor(strat))) + 1), 
                        cbind(cont, tot)), p.col)), as.character)
                      out <- replace(out, is.na(out), "")
                      out <- cbind(as.vector(c(paste(header, 
                        "     "), "   Mean (SD)", "   Median (IQR)", 
                        "   Q1, Q3", "   Min, Max", "   Missing")), 
                        out)
                      rownames(out) <- NULL
                      colnames(out) <- c("Variable", as.vector(levels(as.factor(strat))), 
                        "Overall", "p-value")
                    }
                  }
                }
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
