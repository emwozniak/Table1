###############
# Categorical #
###############

cat.var <- function(var, 
                    strat=NULL, 
                    dec=2, 
                    rownames=as.vector(levels(as.factor(var))),
                    header=deparse(substitute(var)),
                    ptype='None',
                    pname=FALSE) {
  if (is.null(strat)) {
    #Construct categorical summary with no strata
    #Column percents only
    n <- length(which(is.na(var)==FALSE))
    tot <- as.matrix(table(var))
    tot[] <- paste0(tot, 
                    #paste0(' (', 
                    #format(round((tot/rowSums(tot))*100, dec), 
                    #nsmall=dec), '%)',
                    paste0(' (', 
                           format(round((tot/colSums(tot))*100, dec), 
                                  nsmall=dec), '%)'))
    #)
    miss <- length(which(is.na(var)==T))
    out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                                   n, 
                                   rep(NA, length(levels(as.factor(strat))) + 1),
                                   tot, 
                                   miss),
                             row.names=NULL),
                  as.character)
    out <- replace(out, is.na(out), '')
    out <- cbind(as.vector(c(paste(header, '     '), '  Count', '  (%)',  
                             paste0('  ', rownames), '  Missing')), out)
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 'Overall')
  }
  else {
    if (!is.null(strat)) {
      #Construct categorical summary with strata
      #Row and column percents
      cat <- as.matrix(table(var, as.factor(strat)))
      tot <- as.matrix(apply(cat, 1, sum))
      n <- as.matrix(apply(cat, 2, sum))
      n[] <- paste0(n,
                    paste0(' (',
                           format(round((n/sum(n))*100, dec),
                                  nsmall=dec), '%)'
                    )
      )
      n <- cbind(t(n), apply(tot, 2, sum))
      cat[] <- paste0(cat, 
                      paste0(' (', 
                             format(round((cat/rowSums(cat))*100, dec), 
                                    nsmall=dec), '%)',
                             paste0(' (', 
                                    format(round((cat/colSums(cat))*100, dec), 
                                           nsmall=dec), '%)'))
      )
      tot[] <- paste0(tot, 
                      paste0(' (', 
                             format(round((tot/rowSums(tot))*100, dec), 
                                    nsmall=dec), '%)',
                             paste0(' (', 
                                    format(round((tot/colSums(tot))*100, dec), 
                                           nsmall=dec), '%)'))
      )
      miss <- c(aggregate(var, list(strat), function(x) {sum(is.na(x))})[,-1],
                sum(aggregate(var, list(strat), function(x) {sum(is.na(x))})[,-1]))
      out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                                     n,
                                     rep(NA, length(levels(as.factor(strat))) + 1),
                                     cbind(cat, tot), 
                                     miss),
                               row.names=NULL),
                    as.character)
      out <- replace(out, is.na(out), '')
      out <- cbind(as.vector(c(paste(header, '     '), '  Count (%)', '  (Row %)(Col %)', 
                               paste0('  ', rownames), '  Missing')), out)
      rownames(out) <- NULL
      colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall')
    }
  }
  if (ptype=='chisq') {
    p <- chisq.test(var, strat)$p.value
    if (p>=0.0001 & pname==TRUE) {
      p.col <- c(signif(p, 3), 'Chi-square', rep(NA, length(levels(as.factor(var))) + 2))
    }
    else {
      if (p<0.0001 & pname==TRUE) {
        p.col <- c('<0.0001', 'Chi-square', rep(NA, length(levels(as.factor(var))) + 2))
      }
      else {
        if (p>=0.0001 & pname==FALSE) {
          p.col <- c(signif(p, 3), rep(NA, length(levels(as.factor(var))) + 3)) 
        }
        else {
          if (p<0.0001 & pname==FALSE) {
            p.col <- c('<0.0001', rep(NA, length(levels(as.factor(var))) + 3))
          }
        }
      }
    }      
    out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                                         n,
                                         rep(NA, length(levels(as.factor(strat))) + 1),
                                         cbind(cat, tot), 
                                         miss),
                                   p.col),
                             row.names=NULL),
                  as.character)
    out <- replace(out, is.na(out), '')
    out <- cbind(as.vector(c(paste(header, '     '), '  Count (%)', '  (Row %)(Col %)', 
                             paste0('   ', rownames), '  Missing')), out)
    rownames(out) <- NULL
    colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
  }
  else {
    if (ptype=='fisher') {
      p <- fisher.test(var, strat)$p.value
      if (p>=0.0001 & pname==TRUE) {
        p.col <- c(signif(p, 3), 'Fisher exact', rep(NA, length(levels(as.factor(var))) + 2))
      }
      else {
        if (p<0.0001 & pname==TRUE) {
          p.col <- c('<0.0001', 'Fisher exact', rep(NA, length(levels(as.factor(var))) + 2))
        }
        else {
          if (p>=0.0001 & pname==FALSE) {
            p.col <- c(signif(p, 3), rep(NA, length(levels(as.factor(var))) + 3))
          }
          else {
            if (p<0.0001 & pname==FALSE) {
              p.col <- c('<0.0001', rep(NA, length(levels(as.factor(var))) + 3))
            }
          }
        }
      }      
      out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                                           n,
                                           rep(NA, length(levels(as.factor(strat))) + 1),
                                           cbind(cat, tot), 
                                           miss),
                                     p.col),
                               row.names=NULL),
                    as.character)
      out <- replace(out, is.na(out), '')
      out <- cbind(as.vector(c(paste(header, '     '), '  Count (%)', '  (Row %)(Col %)', paste0('   ', rownames), '   Missing')), out)
      rownames(out) <- NULL
      colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
    }
    else {
      if (ptype=='mcnemar') {
        p <- mcnemar.test(var, strat)$p.value
        if (p>=0.0001 & pname==TRUE) {
          p.col <- c(signif(p, 3), 'McNemar', rep(NA, length(levels(as.factor(strat))) + 2))
        }
        else {
          if (p<0.0001 & pname==TRUE) {
            p.col <- c('<0.0001', 'McNemar', rep(NA, length(levels(as.factor(strat))) + 2))
          }
          else {
            if (p>=0.0001 & pname==FALSE) {
              p.col <- c(signif(p, 3), rep(NA, length(levels(as.factor(strat))) + 3))
            }
            else {
              if (p<0.0001 & pname==FALSE) {
                p.col <- c('<0.0001', rep(NA, length(levels(as.factor(strat))) + 3))
              }
            }
          }
        }      
        out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                                             n,
                                             rep(NA, length(levels(as.factor(strat))) + 1),
                                             cbind(cat, tot), 
                                             miss),
                                       p.col),
                                 row.names=NULL),
                      as.character)
        out <- replace(out, is.na(out), '')
        out <- cbind(as.vector(c(paste(header, '     '), '  Count (%)', '  (Row %)(Col %)', 
                                 paste0('   ', rownames), '   Missing')), out)
        rownames(out) <- NULL
        colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
      }
    }
  }
  return(data.frame(out))
}

##############
# Continuous #
##############

cont.var <- function(var, 
                     strat=NULL, 
                     dec=2, 
                     header=deparse(substitute(var)), 
                     ptype='None',
                     pname=FALSE) {
  if (is.null(strat)) {
    mean.sd.tot <- paste(
      format(
        round(mean(var, na.rm=T), dec), 
        nsmall=dec), 
      paste('(', 
            format(
              round(sd(var, na.rm=T), dec), 
              nsmall=dec), ')', 
            sep=''))
    med.iqr.tot <- paste(
      format(
        round(median(var, na.rm=T), dec), 
        nsmall=dec), 
      paste('(', 
            format(
              round(IQR(var, na.rm=T), dec), 
              nsmall=dec), ')', 
            sep=''))
    q1.q3.tot <- paste(
      format(
        round(quantile(var, 0.25, na.rm=T), dec), 
        nsmall=2), ', ',
      format(
        round(quantile(var, 0.75, na.rm=T), dec)), 
      sep='')
    min.max.tot <- paste(
      format(
        round(min(var, na.rm=T), dec), 
        nsmall=dec), ', ',
      format(round(max(var, na.rm=T), 2)), 
      sep='')
    miss.tot <- length(which(is.na(var)==T))
    out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                  mean.sd.tot, med.iqr.tot, q1.q3.tot, min.max.tot, miss.tot)),
                  as.character)
    out <- replace(out, is.na(out), '')
    out <- cbind(as.vector(c(paste(header, '     '), '   Mean (SD)', '   Median (IQR)', 
                             '   Q1, Q3', '   Min, Max', '   Missing')), out)
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 'Overall')
  }
  else {
    if (!is.null(strat)) {
      mean.sd <- format(
        round(aggregate(var, list(strat), mean, na.rm=T)[,-1], dec), 
        nsmall=dec)
      mean.sd[] <- paste0(mean.sd, paste0(' (', 
                      (format(
                          round(aggregate(var, list(strat), sd, na.rm=T)[,-1], dec), 
                       nsmall=dec)),
                   ')'))
      med.iqr <- format(
                    round(aggregate(var, list(strat), median, na.rm=T)[,-1], dec), 
                 nsmall=dec)
      med.iqr[] <- paste0(med.iqr, paste0(' (', 
                      (format(
                          round(aggregate(var, list(strat), IQR, na.rm=T)[,-1], dec), 
                       nsmall=dec)), 
                   ')'))
      q1.q3 <- format(
                  round(aggregate(var, list(strat), quantile, 0.25, na.rm=T)[,-1], dec), 
               nsmall=dec)
      q1.q3[] <- paste0(q1.q3, paste0(', ', 
                    (format(
                        round(aggregate(var, list(strat), quantile, 0.75, na.rm=T)[,-1], dec), 
                    nsmall=dec))))
      min.max <- format(
                    round(aggregate(var, list(strat), min, na.rm=T)[,-1], dec), 
                 nsmall=dec)
      min.max[] <- paste0(min.max, paste0(', ', 
                      (format(
                          round(aggregate(var, list(strat), max, na.rm=T)[,-1], dec), 
                      nsmall=dec))))
      miss <- aggregate(var, list(strat), function(x) {sum(is.na(x))})[,-1]
      cont <- rbind(mean.sd, med.iqr, q1.q3, min.max, miss)
      
      mean.sd.tot <- paste(
                        format(
                          round(mean(var, na.rm=T), dec), 
                        nsmall=dec), 
                     paste('(', 
                        format(
                          round(sd(var, na.rm=T), dec), 
                        nsmall=dec), ')', 
                     sep=''))
      med.iqr.tot <- paste(
                        format(
                          round(median(var, na.rm=T), dec), 
                        nsmall=dec), 
                     paste('(', 
                        format(
                          round(IQR(var, na.rm=T), dec), 
                        nsmall=dec), ')', 
                     sep=''))
      q1.q3.tot <- paste(
                     format(
                        round(quantile(var, 0.25, na.rm=T), dec), 
                     nsmall=dec), ', ',
                     format(
                        round(quantile(var, 0.75, na.rm=T), dec),
                     nsmall=dec), 
                   sep='') 
      min.max.tot <- paste(
                        format(
                          round(min(var, na.rm=T), dec), 
                        nsmall=dec), ', ',
                        format(
                          round(max(var, na.rm=T), dec),
                        nsmall=dec), 
                     sep='')
      miss.tot <- length(which(is.na(var)==T))
      tot <- rbind(mean.sd.tot, med.iqr.tot, q1.q3.tot, min.max.tot, miss.tot)
      
      out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                    cbind(cont, tot))),
                    as.character)
      out <- replace(out, is.na(out), '')
      out <- cbind(as.vector(c(paste(header, '     '), '   Mean (SD)', '   Median (IQR)', 
                               '   Q1, Q3', '   Min, Max', '   Missing')), out)
      rownames(out) <- NULL
      colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall')
    }
  }
  if (ptype=='ttest') {
    p <- t.test(var[strat==levels(as.factor(strat))[1]], var[strat==levels(as.factor(strat))[2]])$p.value
    if (p>=0.0001 & pname==TRUE) {
      p.col <- c(format(round(p, 4), nsmall=4), 't-test', rep(NA, 4))
    }
    else {
      if (p<0.0001 & pname==TRUE) {
        p.col <- c('<0.0001', 't-test', rep(NA, 4))
      }
      else {
        if (p>=0.0001 & pname==FALSE) {
          p.col <- c(format(round(p, 4), nsmall=4), rep(NA, 5))
        }
        else {
          if (p<0.0001 & pname==FALSE) {
            p.col <- c('<0.0001', rep(NA, 5))
          }
        }
      }
    }      
    out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                  cbind(cont, tot)), p.col)), as.character)
    out <- replace(out, is.na(out), '')
    out <- cbind(as.vector(c(paste(header, '     '), '   Mean (SD)', '   Median (IQR)', 
                             '   Q1, Q3', '   Min, Max', '   Missing')), out)
    rownames(out) <- NULL
    colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
  }
  else {
    if (ptype=='ttest.pair') {
      p <- t.test(var[strat==levels(as.factor(strat))[1]], var[strat==levels(as.factor(strat))[2]],
                  paired=TRUE)$p.value
      if (p>=0.0001 & pname==TRUE) {
        p.col <- c(format(round(p, 4), nsmall=4), 'Paired t-test', rep(NA, 4))
      }
      else {
        if (p<0.0001 & pname==TRUE) {
          p.col <- c('<0.0001', 'Paired t-test', rep(NA, 4))
        }
        else {
          if (p>=0.0001 & pname==FALSE) {
            p.col <- c(format(round(p, 4), nsmall=4), rep(NA, 5))
          }
          else {
            if (p<0.0001 & pname==FALSE) {
              p.col <- c('<0.0001', rep(NA, 5))
            }
          }
        }
      }      
      out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                    cbind(cont, tot)), p.col)), as.character)
      out <- replace(out, is.na(out), '')
      out <- cbind(as.vector(c(paste(header, '     '), '   Mean (SD)', '   Median (IQR)', 
                               '   Q1, Q3', '   Min, Max', '   Missing')), out)
      rownames(out) <- NULL
      colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')

    }
    else {
      if (ptype=='wilcox') {
        p <- wilcox.test(var[strat==levels(as.factor(strat))[1]], var[strat==levels(as.factor(strat))[2]])$p.value
        if (p>=0.0001 & pname==TRUE) {
          p.col <- c(format(round(p, 4), nsmall=4), 'Wilcoxon rank sum', rep(NA, 4))
        }
        else {
          if (p<0.0001 & pname==TRUE) {
            p.col <- c('<0.0001', 'Wilcoxon rank sum', rep(NA, 4))
          }
          else {
            if (p>=0.0001 & pname==FALSE) {
              p.col <- c(format(round(p, 4), nsmall=4), rep(NA, 5))
            }
            else {
              if (p<0.0001 & pname==FALSE) {
                p.col <- c('<0.0001', rep(NA, 5))
              }
            }
          }
        }      
        out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                      cbind(cont, tot)), p.col)), as.character)
        out <- replace(out, is.na(out), '')
        out <- cbind(as.vector(c(paste(header, '     '), '   Mean (SD)', '   Median (IQR)', 
                                 '   Q1, Q3', '   Min, Max', '   Missing')), out)
        rownames(out) <- NULL
        colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
      }
      else {
        if (ptype=='wilcox.pair') {
          p <- wilcox.test(var[strat==levels(as.factor(strat))[1]], var[strat==levels(as.factor(strat))[2]],
                           paired=TRUE)$p.value
          if (p>=0.0001 & pname==TRUE) {
            p.col <- c(format(round(p, 4), nsmall=4), 'Paired Wilcoxon', rep(NA, 4))
          }
          else {
            if (p<0.0001 & pname==TRUE) {
              p.col <- c('<0.0001', 'Paired Wilcoxon', rep(NA, 4))
            }
            else {
              if (p>=0.0001 & pname==FALSE) {
                p.col <- c(format(round(p, 4), nsmall=4), rep(NA, 5))
              }
              else {
                if (p<0.0001 & pname==FALSE) {
                  p.col <- c('<0.0001', rep(NA, 5))
                }
              }
            }
          }      
          out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                        cbind(cont, tot)), p.col)), as.character)
          out <- replace(out, is.na(out), '')
          out <- cbind(as.vector(c(paste(header, '     '), '   Mean (SD)', '   Median (IQR)', 
                                   '   Q1, Q3', '   Min, Max', '   Missing')), out)
          rownames(out) <- NULL
          colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
        }
        else {
          if (ptype=='kruskal') {
            p <- kruskal.test(var, strat)$p.value
            if (p>=0.0001 & pname==TRUE) {
              p.col <- c(format(round(p, 4), nsmall=4), 'Kruskal-Wallis', rep(NA, 4))
            }
            else {
              if (p<0.0001 & pname==TRUE) {
                p.col <- c('<0.0001', 'Kruskal-Wallis', rep(NA, 4))
              }
              else {
                if (p>=0.0001 & pname==FALSE) {
                  p.col <- c(format(round(p, 4), nsmall=4), rep(NA, 5))
                }
                else {
                  if (p<0.0001 & pname==FALSE) {
                    p.col <- c('<0.0001', rep(NA, 5))
                  }
                }
              }
            }      
            out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                          cbind(cont, tot)), p.col)), as.character)
            out <- replace(out, is.na(out), '')
            out <- cbind(as.vector(c(paste(header, '     '), '   Mean (SD)', '   Median (IQR)', 
                                     '   Q1, Q3', '   Min, Max', '   Missing')), out)
            rownames(out) <- NULL
            colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
          }
          else {
            if (ptype=='anova') {
              p <- summary(aov(var~strat))[[1]][["Pr(>F)"]][[1]]
              if (p>=0.0001 & pname==TRUE) {
                p.col <- c(format(round(p, 4), nsmall=4), 'ANOVA', rep(NA, 4))
              }
              else {
                if (p<0.0001 & pname==TRUE) {
                  p.col <- c('<0.0001', 'ANOVA', rep(NA, 4))
                }
                else {
                  if (p>=0.0001 & pname==FALSE) {
                    p.col <- c(format(round(p, 4), nsmall=4), rep(NA, 5))
                  }
                  else {
                    if (p<0.0001 & pname==FALSE) {
                      p.col <- c('<0.0001', rep(NA, 5))
                    }
                  }
                }
              }      
              out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                            cbind(cont, tot)), p.col)), as.character)
              out <- replace(out, is.na(out), '')
              out <- cbind(as.vector(c(paste(header, '     '), '   Mean (SD)', '   Median (IQR)', 
                                       '   Q1, Q3', '   Min, Max', '   Missing')), out)
              rownames(out) <- NULL
              colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
            }
          }
        }
      }
    }
  }
  return(data.frame(out))
}

################
# LaTeX output #
################
out.latex <- function(tab, colnames=NULL) {
  named <- as.vector(tab[,1])
  tags <- grepl('^ ', named)
  named <- c(ifelse(
    tags==F,
    paste("\\vspace*{0.1cm}", 
          paste("\\", "\\", sep=''), named) ,
    paste("\\hskip .5cm", named, sep=' ')))
  
  output <- cbind(named, tab[,2:dim(tab)[2]])
  colnames(output) <- colnames
  
  print(xtable(output, align='llrrrr'), 
        type="latex", 
        sanitize.text.function = function(x){x}, 
        include.rownames=F)
}

###############
# HTML output #
###############
out.html <- function(tab, colnames) {
  named <- as.vector(tab[,1])
  tags <- grepl('^ ', named)
  named <- c(ifelse(tags==F, paste('<b>', named, '</b>', sep=''), 
                    paste("&nbsp;", "&nbsp;", "&nbsp;", named, sep=' ')))
  output <- cbind(named, tab[,2:dim(tab)[2]])
  
  return (
    htmlTable(as.matrix(output), 
              rnames=F, 
              header=colnames,
              align=c('l', rep('r', ncol(output)-1)))
  )
}

#####################
# Plain text output #
#####################
out.plain <- function(tab, colnames=NULL) {
  output <- cbind(format(as.vector(tab[,1]), justify='left'), 
                  example[,2:dim(tab)[2]])
  colnames(output)=colnames
  return(print(output, row.names=F))
}
