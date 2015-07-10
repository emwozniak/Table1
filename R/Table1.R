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
  
  #Construct categorical summary with no strata
  ##Column percents only
  if (is.null(strat)) {
    n <- length(which(is.na(var)==FALSE))
    tot <- as.matrix(table(var))
    tot[] <- paste0(tot, 
                paste0(' (', 
                    format(round((tot/colSums(tot))*100, dec), 
                    nsmall=dec), '%)'))
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
  
  #Construct categorical summary with strata
  ##Row and column percents
  else if (!is.null(strat)) {
    cat <- as.matrix(table(var, as.factor(strat)))
    tot <- as.matrix(apply(cat, 1, sum))
    n <- as.matrix(apply(cat, 2, sum))
    n[] <- paste0(n,
                  paste0(' (',
                      format(round((n/sum(n))*100, dec),
                      nsmall=dec), '%)')
                  )
    n <- cbind(t(n), apply(tot, 2, sum))
    
    #Stratified summaries
    cat[] <- paste0(cat, 
                    paste0(' (', 
                          format(round((cat/rowSums(cat))*100, dec), 
                          nsmall=dec), '%)',
                    paste0(' (', 
                          format(round(t(t(cat)/colSums(cat))*100, dec), 
                          nsmall=dec), '%)')
                    ))
    
    #Overall summary for total column
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
    
    #cbind stratified and overall summaries
    #rbind counts and missings to create complete summary
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
  
  #Chi-square test
  ##Options to print test on table
  ##Re-format small p-values
  if (ptype=='chisq') {
    p <- chisq.test(var, strat)$p.value
    if (p>=0.0001 & pname==TRUE) {
      p.col <- c(signif(p, 3), 'Chi-square', rep(NA, length(levels(as.factor(var))) + 2))
    }
    else if (p<0.0001 & pname==TRUE) {
      p.col <- c('<0.0001', 'Chi-square', rep(NA, length(levels(as.factor(var))) + 2))
    }
    else if (p>=0.0001 & pname==FALSE) {
      p.col <- c(signif(p, 3), rep(NA, length(levels(as.factor(var))) + 3)) 
    }
    else if (p<0.0001 & pname==FALSE) {
      p.col <- c('<0.0001', rep(NA, length(levels(as.factor(var))) + 3))
    }
    
    #Create data frame with chi-square p-values          
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
  
  #Fisher exact test
  ##Options to print test on table
  ##Re-format small p-values
  else {
    if (ptype=='fisher') {
      p <- fisher.test(var, strat)$p.value
      if (p>=0.0001 & pname==TRUE) {
        p.col <- c(signif(p, 3), 'Fisher exact', rep(NA, length(levels(as.factor(var))) + 2))
      }
      else if (p<0.0001 & pname==TRUE) {
        p.col <- c('<0.0001', 'Fisher exact', rep(NA, length(levels(as.factor(var))) + 2))
      }
      else if (p>=0.0001 & pname==FALSE) {
        p.col <- c(signif(p, 3), rep(NA, length(levels(as.factor(var))) + 3))
      }
      else if (p<0.0001 & pname==FALSE) {
        p.col <- c('<0.0001', rep(NA, length(levels(as.factor(var))) + 3))
      }
      
      
      #Create data frame with Fisher test p-values       
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
    
    #McNemar's test
    ##Options to print test on table
    ##Re-format small p-values
    else {
      if (ptype=='mcnemar') {
        p <- mcnemar.test(var, strat)$p.value
        if (p>=0.0001 & pname==TRUE) {
          p.col <- c(signif(p, 3), 'McNemar', rep(NA, length(levels(as.factor(strat))) + 2))
        }
        else if (p<0.0001 & pname==TRUE) {
          p.col <- c('<0.0001', 'McNemar', rep(NA, length(levels(as.factor(strat))) + 2))
        }
        else if (p>=0.0001 & pname==FALSE) {
          p.col <- c(signif(p, 3), rep(NA, length(levels(as.factor(strat))) + 3))
        }
        else if (p<0.0001 & pname==FALSE) {
          p.col <- c('<0.0001', rep(NA, length(levels(as.factor(strat))) + 3))
        }
        
        
        #Create data frame with McNemar's test p-values        
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
  
  #Continuous summary with no strata
  if (is.null(strat)) {
    n.tot <- length(which(is.na(var)==FALSE))
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
    out <- sapply(data.frame(rbind(NA, n.tot,
                      mean.sd.tot, med.iqr.tot, q1.q3.tot, min.max.tot, miss.tot)),
                  as.character)
    out <- replace(out, is.na(out), '')
    out <- cbind(as.vector(c(paste(header, '     '),  '   Count', '   Mean (SD)', '   Median (IQR)', 
                             '   Q1, Q3', '   Min, Max', '   Missing')), out)
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 'Overall')
  }
  
  #Continuous summary with strata
  else {
    if (!is.null(strat)) {
      
      #Stratified summaries
      mean.sd <- format(
                    round(aggregate(var, list(strat), mean, na.rm=T)[,-1], dec), 
                 nsmall=dec)
      mean.sd[] <- paste0(mean.sd, 
                          paste0(' (', 
                            (format(
                                round(aggregate(var, list(strat), sd, na.rm=T)[,-1], dec), 
                              small=dec)),
                            ')'))
      med.iqr <- format(
                    round(aggregate(var, list(strat), median, na.rm=T)[,-1], dec), 
                 nsmall=dec)
      med.iqr[] <- paste0(med.iqr, 
                          paste0(' (', 
                            (format(
                                round(aggregate(var, list(strat), IQR, na.rm=T)[,-1], dec), 
                             nsmall=dec)), 
                          ')'))
      q1.q3 <- format(
                  round(aggregate(var, list(strat), quantile, 0.25, na.rm=T)[,-1], dec), 
               nsmall=dec)
      q1.q3[] <- paste0(q1.q3, 
                        paste0(', ', 
                            (format(
                                round(aggregate(var, list(strat), quantile, 0.75, na.rm=T)[,-1], dec), 
                             nsmall=dec))
                        ))
      min.max <- format(
                    round(aggregate(var, list(strat), min, na.rm=T)[,-1], dec), 
                 nsmall=dec)
      min.max[] <- paste0(min.max, 
                          paste0(', ', 
                              (format(
                                  round(aggregate(var, list(strat), max, na.rm=T)[,-1], dec), 
                               nsmall=dec))
                          ))
      miss <- aggregate(var, list(strat), function(x) {sum(is.na(x))})[,-1]
      n <- as.vector(aggregate(var, list(strat), length)[,-1]) - as.vector(miss)
      cont <- rbind(n, mean.sd, med.iqr, q1.q3, min.max, miss)
      
      #Overall summaries for total column
      n.tot <- sum(n)
      mean.sd.tot <- paste(
        format(
          round(mean(var[!is.na(strat)], na.rm=T), dec), 
          nsmall=dec), 
        paste('(', 
              format(
                round(sd(var[!is.na(strat)], na.rm=T), dec), 
                nsmall=dec), ')', 
              sep=''))
      med.iqr.tot <- paste(
        format(
          round(median(var[!is.na(strat)], na.rm=T), dec), 
          nsmall=dec), 
        paste('(', 
              format(
                round(IQR(var[!is.na(strat)], na.rm=T), dec), 
                nsmall=dec), ')', 
              sep=''))
      q1.q3.tot <- paste(
        format(
          round(quantile(var[!is.na(strat)], 0.25, na.rm=T), dec), 
          nsmall=dec), ', ',
        format(
          round(quantile(var[!is.na(strat)], 0.75, na.rm=T), dec),
          nsmall=dec), 
        sep='') 
      min.max.tot <- paste(
        format(
          round(min(var[!is.na(strat)], na.rm=T), dec), 
          nsmall=dec), ', ',
        format(
          round(max(var[!is.na(strat)], na.rm=T), dec),
          nsmall=dec), 
        sep='')
      miss.tot=sum(miss)
      tot <- rbind(n.tot, mean.sd.tot, med.iqr.tot, q1.q3.tot, min.max.tot, miss.tot)
      
      #cbind stratified and total columns
      #rbind counts and missings to create complete summary
      out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                                     cbind(cont, tot))),
                    as.character)
      out <- replace(out, is.na(out), '')
      out <- cbind(as.vector(c(paste(header, '     '), '   Count', '   Mean (SD)', '   Median (IQR)', 
                               '   Q1, Q3', '   Min, Max', '   Missing')), out)
      rownames(out) <- NULL
      colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall')
    }
  }
  
  #t-test
  ##Options to print test on table
  ##Re-format small p-values
  if (ptype=='ttest') {
    p <- t.test(var[strat==levels(as.factor(strat))[1]], var[strat==levels(as.factor(strat))[2]])$p.value
    if (p>=0.0001 & pname==TRUE) {
      p.col <- c(signif(p, 3), 't-test', rep(NA, 5))
    }
    else if (p<0.0001 & pname==TRUE) {
      p.col <- c('<0.0001', 't-test', rep(NA, 5))
    }
    else if (p>=0.0001 & pname==FALSE) {
      p.col <- c(signif(p, 3), rep(NA, 6))
    }
    else if (p<0.0001 & pname==FALSE) {
      p.col <- c('<0.0001', rep(NA, 6))
    }
    
    
    #Create data frame with t-test p-values      
    out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                                         cbind(cont, tot)), p.col)), as.character)
    out <- replace(out, is.na(out), '')
    out <- cbind(as.vector(c(paste(header, '     '), '   Count',  '   Mean (SD)', '   Median (IQR)', 
                             '   Q1, Q3', '   Min, Max', '   Missing')), out)
    rownames(out) <- NULL
    colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
  }
  
  #Paired t-test
  ##Options to print test on table
  ##Re-format small p-values
  else {
    if (ptype=='ttest.pair') {
      p <- t.test(var[strat==levels(as.factor(strat))[1]], var[strat==levels(as.factor(strat))[2]],
                  paired=TRUE)$p.value
      if (p>=0.0001 & pname==TRUE) {
        p.col <- c(signif(p, 3), 'Paired t-test', rep(NA, 5))
      }
      else if (p<0.0001 & pname==TRUE) {
        p.col <- c('<0.0001', 'Paired t-test', rep(NA, 5))
      }
      else if (p>=0.0001 & pname==FALSE) {
        p.col <- c(signif(p, 3), rep(NA, 6))
      }
      else if (p<0.0001 & pname==FALSE) {
        p.col <- c('<0.0001', rep(NA, 6))
      }
           
      #Create data frame with paired t-test p-values      
      out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                                           cbind(cont, tot)), p.col)), as.character)
      out <- replace(out, is.na(out), '')
      out <- cbind(as.vector(c(paste(header, '     '), '   Count', '   Mean (SD)', '   Median (IQR)', 
                               '   Q1, Q3', '   Min, Max', '   Missing')), out)
      rownames(out) <- NULL
      colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
      
    }
    
    #Wilcoxon rank sum test
    ##Options to print test on table
    ##Re-format small p-values
    else {
      if (ptype=='wilcox') {
        p <- wilcox.test(var[strat==levels(as.factor(strat))[1]], var[strat==levels(as.factor(strat))[2]])$p.value
        if (p>=0.0001 & pname==TRUE) {
          p.col <- c(signif(p, 3), 'Wilcoxon rank sum', rep(NA, 5))
        }
        else if (p<0.0001 & pname==TRUE) {
          p.col <- c('<0.0001', 'Wilcoxon rank sum', rep(NA, 5))
        }
        else if (p>=0.0001 & pname==FALSE) {
          p.col <- c(signif(p, 3), rep(NA, 6))
        }
        else if (p<0.0001 & pname==FALSE) {
          p.col <- c('<0.0001', rep(NA, 6))
        }
             
        #Create data frame with Wilcoxon rank sum p-values      
        out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                                             cbind(cont, tot)), p.col)), as.character)
        out <- replace(out, is.na(out), '')
        out <- cbind(as.vector(c(paste(header, '     '), '   Count', '   Mean (SD)', '   Median (IQR)', 
                                 '   Q1, Q3', '   Min, Max', '   Missing')), out)
        rownames(out) <- NULL
        colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
      }
      
      #Signed rank test
      ##Options to print test on table
      ##Re-format small p-values
      else {
        if (ptype=='wilcox.pair') {
          p <- wilcox.test(var[strat==levels(as.factor(strat))[1]], var[strat==levels(as.factor(strat))[2]],
                           paired=TRUE)$p.value
          if (p>=0.0001 & pname==TRUE) {
            p.col <- c(signif(p, 3), 'Signed-rank', rep(NA, 5))
          }
          else if (p<0.0001 & pname==TRUE) {
            p.col <- c('<0.0001', 'Signed-rank', rep(NA, 5))
          }
          else if (p>=0.0001 & pname==FALSE) {
            p.col <- c(signif(p, 3), rep(NA, 6))
          }
          else if (p<0.0001 & pname==FALSE) {
            p.col <- c('<0.0001', rep(NA, 6))
          }
                   
          #Create data frame with signed-rank test p-values       
          out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                                               cbind(cont, tot)), p.col)), as.character)
          out <- replace(out, is.na(out), '')
          out <- cbind(as.vector(c(paste(header, '     '), '   Count', '   Mean (SD)', '   Median (IQR)', 
                                   '   Q1, Q3', '   Min, Max', '   Missing')), out)
          rownames(out) <- NULL
          colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
        }
        
        #Kruskal-Wallis test
        ##Options to print test on table
        ##Re-format small p-values
        else {
          if (ptype=='kruskal') {
            p <- kruskal.test(var, strat)$p.value
            if (p>=0.0001 & pname==TRUE) {
              p.col <- c(signif(p, 3), 'Kruskal-Wallis', rep(NA, 5))
            }
            else if (p<0.0001 & pname==TRUE) {
              p.col <- c('<0.0001', 'Kruskal-Wallis', rep(NA, 5))
            }
            else if (p>=0.0001 & pname==FALSE) {
              p.col <- c(signif(p, 3), rep(NA, 6))
            }
            else if (p<0.0001 & pname==FALSE) {
              p.col <- c('<0.0001', rep(NA, 6))
            }
                       
            #Create a data frame with Kruskal-Wallis test p-values      
            out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                                                 cbind(cont, tot)), p.col)), as.character)
            out <- replace(out, is.na(out), '')
            out <- cbind(as.vector(c(paste(header, '     '), '   Count', '   Mean (SD)', '   Median (IQR)', 
                                     '   Q1, Q3', '   Min, Max', '   Missing')), out)
            rownames(out) <- NULL
            colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
          }
          
          #ANOVA
          ##Options to print test on table
          ##Re-format small p-values
          else {
            if (ptype=='anova') {
              p <- summary(aov(var~strat))[[1]][["Pr(>F)"]][[1]]
              if (p>=0.0001 & pname==TRUE) {
                p.col <- c(signif(p, 3), 'ANOVA', rep(NA, 5))
              }
              else if (p<0.0001 & pname==TRUE) {
                p.col <- c('<0.0001', 'ANOVA', rep(NA, 5))
              }
              else if (p>=0.0001 & pname==FALSE) {
                p.col <- c(signif(p, 3), rep(NA, 6))
              }
              else if (p<0.0001 & pname==FALSE) {
                p.col <- c('<0.0001', rep(NA, 6))
              }
                           
              #Create a data frame with ANOVA p-values      
              out <- sapply(data.frame(cbind(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                                                   cbind(cont, tot)), p.col)), as.character)
              out <- replace(out, is.na(out), '')
              out <- cbind(as.vector(c(paste(header, '     '), '   Count', '   Mean (SD)', '   Median (IQR)', 
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
  tags2 <- (grepl('Count', named, fixed=TRUE) | grepl('%', named, fixed=TRUE) | 
              grepl('Missing', named, fixed=T)==TRUE | grepl('Mean', named, fixed=TRUE) |
              grepl('Median', named, fixed=TRUE) | grepl('Q1', named, fixed=TRUE) |
              grepl('Min', named, fixed=TRUE))
  named <- ifelse(tags2==F,
                  paste("\\textbf{", named, "}", sep=''),
                  named)
  named <- c(ifelse(
    tags==F,
    paste("\\vspace*{0.1cm}", 
          paste("\\", "\\", sep=''), named) ,
    paste("\\hskip .5cm", named, sep=' ')))
  
  #output <- cbind(named, tab[,2:dim(tab)[2]])
  output <- apply(cbind(named, tab[,2:dim(tab)[2]]), 2, function(x) gsub('%', '\\\\%', x))
  colnames(output) <- colnames
  
  print(xtable(output, align=paste(c('l', 'l', rep('r', dim(output)[2]-1)), collapse='')), 
        type="latex", 
        sanitize.text.function = function(x){x}, 
        include.rownames=F)
}

###############
# HTML output #
###############
#For categorical variables, formatting may be a problem if categories 
#have similar names to available summary statistics
out.html <- function(tab, colnames) {
  named <- as.vector(tab[,1])
  tags <- grepl('^ ', named)
  tags2 <- (grepl('Count', named, fixed=TRUE) | grepl('%', named, fixed=TRUE) | 
              grepl('Missing', named, fixed=T)==TRUE | grepl('Mean', named, fixed=TRUE) |
              grepl('Median', named, fixed=TRUE) | grepl('Q1', named, fixed=TRUE) |
              grepl('Min', named, fixed=TRUE))
  named <- ifelse((tags==FALSE | tags2==FALSE), paste('<b>', named, '</b>', sep=''), named)
  named <- ifelse(tags==TRUE, paste("&nbsp;", "&nbsp;", "&nbsp;", named, sep=' '), named)
  
  output <- cbind(named, as.vector(tab[,2:dim(tab)[2]]))
  
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
                  tab[,2:dim(tab)[2]])
  colnames(output)=colnames
  return(print(output, row.names=F))
}


#############################
# Vectorized table function #
#############################
make.table <- function(dat,
                       #Categorical variable options
                       cat.varlist=NULL,
                       cat.header=names(sapply(cat.varlist, FUN=get, simplify=F, USE.NAMES=T)),
                       cat.rownames=lapply(sapply(cat.varlist, FUN=get, simplify=F, USE.NAMES=T), FUN=function(x) 
                         as.vector(levels(as.factor(x)))),
                       cat.ptype='None',
                       
                       #Continuous variable options
                       cont.varlist=NULL, 
                       cont.header=names(sapply(cont.varlist, FUN=get, simplify=F, USE.NAMES=T)),
                       cont.ptype='None',
                       
                       #Overall table options
                       strat=NULL,
                       dec=2,
                       pname=FALSE,
                       colnames=NULL,
                       output='plain'
)

{
  #Warnings
  if (missing(dat)) {
    warning('A data frame must be provided in dat=.')
  }
  if ((output=='plain')==FALSE) {
    warning('Package dependencies: library(htmlTable) for HTML output and library(xtable) for LaTeX output')
  }
  if (any(c(cat.ptype, cont.ptype) != 'None')==TRUE & is.null(strat)==TRUE) {
    warning('A stratifying variable must be identified for p-values to be computed. See strat=.')
  }
  if (any(c(cat.varlist, cont.varlist) %in% ls('package:base'))==TRUE) {
    warning("Variables cannot take the names of any base R functions -- try 
            which(dput(colnames(dat)) %in% ls('package:base'))")
  }
  if (any(is.na(strat))==TRUE) {
    warning('Some observations are omitted from summary due to missing levels of strata variables.')
  }
  
  if (is.null(strat)) {
    cat.strat=rep(list(strat), length(cat.varlist))
    cont.strat=rep(list(strat), length(cont.varlist))
    strat.miss=0
  }
  else {
    if (!is.null(strat)) {
      cat.strat=rep(list(interaction(sapply(strat, FUN=get, simplify=FALSE, USE.NAMES=TRUE))), length(cat.varlist))
      cont.strat=rep(list(interaction(sapply(strat, FUN=get, simplify=FALSE, USE.NAMES=TRUE))), length(cont.varlist))
      strat.miss=lapply(sapply(strat, FUN=get, simplify=FALSE, USE.NAMES=TRUE), function(x) sum(is.na(x)))
      strat.rem=sum(is.na(interaction(sapply(strat, FUN=get, simplify=FALSE, USE.NAMES=TRUE))))
    }
  }
  
  if (!is.null(strat) & strat.rem>0) {
    print(paste("Total observations removed from table:", strat.rem, sep=' '))
    print("Summary of total missing stratification variable(s):")
    print(strat.miss)
    footer.miss <- paste(strat.rem, "observations removed due to missing values in stratifying variable(s)", sep=' ')
  }
  else {footer.miss <- paste('')}
  
  #If only categorical variables are provided
  if (is.null(cont.varlist)) {
    cats <- mapply(cat.var, 
                   var=sapply(cat.varlist, FUN=get, simplify=F, USE.NAMES=T), 
                   strat=cat.strat, 
                   dec=dec, 
                   rownames=cat.rownames,
                   header=cat.header,
                   ptype=cat.ptype,
                   pname=pname,
                   SIMPLIFY=FALSE)
    #Reorder output by variable order in dataset
    tab <- do.call(rbind, 
                   (c(cats))[order(match(names(c(cats)), 
                                         names(dat)))])
  }
  
  else {
    #If only continuous variables are provided
    if (is.null(cat.varlist)) {
      conts <- mapply(cont.var, 
                      var=sapply(cont.varlist, FUN=get, simplify=F, USE.NAMES=T),
                      strat=cont.strat,
                      dec=dec,
                      header=cont.header,
                      ptype=cont.ptype,
                      pname=pname,
                      SIMPLIFY=FALSE)
      #Reorder output by variable order in dataset
      tab <- do.call(rbind, 
                     (c(conts))[order(match(names(c(conts)), 
                                            names(dat)))])
    }
    else {
      #If both categorical and continuous variables are provided
      cats <- mapply(cat.var, 
                     var=sapply(cat.varlist, FUN=get, simplify=F, USE.NAMES=T), 
                     strat=cat.strat, 
                     dec=dec, 
                     rownames=cat.rownames,
                     header=cat.header,
                     ptype=cat.ptype,
                     pname=pname,
                     SIMPLIFY=FALSE)
      conts <- mapply(cont.var, 
                      var=sapply(cont.varlist, FUN=get, simplify=F, USE.NAMES=T),
                      strat=cont.strat,
                      dec=dec,
                      header=cont.header,
                      ptype=cont.ptype,
                      pname=pname,
                      SIMPLIFY=FALSE)
      #Reorder output by variable order in dataset
      tab <- do.call(rbind, 
                     (c(cats, conts))[order(match(names(c(cats, conts)), 
                                                  names(dat)))])
    }
  } 
  
  #Define column names
  if (!is.null(colnames)) {
    colnames <- colnames
  }
  else {
    if (is.null(colnames)) {
      colnames <- colnames(tab)
    }
  }
  
  #Define output 
  if (output=='plain') {
    out.plain <- function(tab) {
      output <- cbind(format(as.vector(tab[,1]), justify='left'), 
                      data.frame(tab[,2:dim(tab)[2]]))
      colnames(output) <- colnames
      return(print(output, row.names=F))
    }
    out.plain(tab)
  }
  else {
    if (output=='html') {
      out.html <- function(tab) {
        named <- as.vector(tab[,1])
        tags <- grepl('^ ', named)
        tags2 <- (grepl('Count', named, fixed=TRUE) | grepl('%', named, fixed=TRUE) | 
                    grepl('Missing', named, fixed=T)==TRUE | grepl('Mean', named, fixed=TRUE) |
                    grepl('Median', named, fixed=TRUE) | grepl('Q1', named, fixed=TRUE) |
                    grepl('Min', named, fixed=TRUE))
        named <- ifelse((tags==FALSE | tags2==FALSE), paste('<b>', named, '</b>', sep=''), named)
        named <- ifelse(tags==TRUE, paste("&nbsp;", "&nbsp;", "&nbsp;", named, sep=' '), named)
        
        output <- cbind(named, as.vector(tab[,2:dim(tab)[2]]))
        colnames(output) <- colnames
        
        return (
          htmlTable(as.matrix(output), 
                    rnames=F, 
                    header=colnames,
                    align=c('l', rep('r', ncol(output)-1)),
                    tfoot=footer.miss)
        )
      }
      out.html(tab)
    }
    else {
      if (output=='latex') {
        out.latex <- function(tab) {
          named <- as.vector(tab[,1])
          tags <- grepl('^ ', named)
          tags2 <- (grepl('Count', named, fixed=TRUE) | grepl('%', named, fixed=TRUE) | 
                      grepl('Missing', named, fixed=T)==TRUE | grepl('Mean', named, fixed=TRUE) |
                      grepl('Median', named, fixed=TRUE) | grepl('Q1', named, fixed=TRUE) |
                      grepl('Min', named, fixed=TRUE))
          named <- ifelse(tags2==F,
                          paste("\\textbf{", named, "}", sep=''),
                          named)
          named <- c(ifelse(
            tags==F,
            paste("\\vspace*{0.1cm}", 
                  paste("\\", "\\", sep=''), named) ,
            paste("\\hskip .5cm", named, sep=' ')))
          
          #output <- cbind(named, tab[,2:dim(tab)[2]])
          output <- apply(cbind(named, data.frame(tab[,2:dim(tab)[2]])), 2, function(x) gsub('%', '\\\\%', x))
          colnames(output) <- colnames
          
          print(xtable(output, align=paste(c('l', 'l', rep('r', dim(output)[2]-1)), collapse=''),
                       caption=footer.miss), 
                type="latex", 
                sanitize.text.function = function(x){x}, 
                include.rownames=F)
        }
        out.latex(tab)
      }
    }
  }
}

