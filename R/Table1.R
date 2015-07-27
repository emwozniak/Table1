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
  
  if (ptype!='None' & pname==FALSE) {
    p <- c(stat.col(var, strat, ptype, pname=FALSE), rep(NA, length(levels(as.factor(var))) + 3))
    out <- cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
    
  }
  
  else if (ptype!='None' & pname==TRUE) {
    p <- c(stat.col(var, strat, ptype, pname=TRUE), rep(NA, length(levels(as.factor(var))) + 2))
    out <-  cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
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
    out <- cbind(as.vector(c(paste(header, '     '),  '   Count', '   Mean (SD)', '   Median (IQR)', 
                             '   Q1, Q3', '   Min, Max', '   Missing')), 
                 replace(out, is.na(out), ''))
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
      #cbind vector of row headers and replace NAs with '' to show up as a blank cell when printed
      out <- cbind(as.vector(c(paste(header, '     '), '   Count', '   Mean (SD)', '   Median (IQR)', 
                               '   Q1, Q3', '   Min, Max', '   Missing')), 
                   replace(out, is.na(out), ''))
      rownames(out) <- NULL
      colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall')
    }
  }
  
  #Print p-values without the name of the test used
  if (ptype!='None' & pname==FALSE) {
    p <- c(stat.col(var, strat, ptype, pname=FALSE), rep(NA, 6))
    out <- cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')    
  }
  
  #Print p-values with the name of the test used
  else if (ptype!='None' & pname==TRUE) {
    p <- c(stat.col(var, strat, ptype, pname=TRUE), rep(NA, 5))
    out <-  cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', as.vector(levels(as.factor(strat))), 'Overall', 'p-value')
  }
  
  #Remove summary statistics specified as unwanted
  if (!is.null(rm.stat)) {
    out <- out[-((which(c('count', 'meansd', 'mediqr', 'q1q3', 'minmax', 'miss') %in% rm.stat))+1),]
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
out.html <- function (tab, colnames, stripe=TRUE, stripe.col='#F7F7F7') 
{
  if (stripe==FALSE) {
    named <- as.vector(tab[, 1])
    tags <- grepl("^ ", named)
    tags2 <- (grepl("Count", named, fixed = TRUE) | 
                grepl("%", named, fixed = TRUE) | 
                grepl("Missing", named, fixed = T) == TRUE | 
                grepl("Mean", named, fixed = TRUE) | 
                grepl("Median", named, fixed = TRUE) | 
                grepl("Q1", named, fixed = TRUE) | 
                grepl("Min", named, fixed = TRUE))
    named <- ifelse((tags == FALSE | tags2 == FALSE), 
                    paste("<b>", named, "</b>", sep = ""), 
                    named)
    named <- ifelse(tags == TRUE, 
                    paste("&nbsp;", "&nbsp;", "&nbsp;", named, sep = " "), 
                    named)
    output <- cbind(named, as.vector(tab[, 2:dim(tab)[2]]))
    return(htmlTable(as.matrix(output), 
                     rnames = F, 
                     header = colnames, 
                     align = c("l", rep("r", ncol(output) - 1)), 
                     css.cell = "padding-left: .2em; padding-right: 2em;"))
  }
  else if (stripe==TRUE) {
    named <- as.vector(tab[, 1])
    tags <- grepl("^ ", named) 
    tags2 <- (grepl("Count", named, fixed = TRUE) | 
                grepl("%", named, fixed = TRUE) | 
                grepl("Missing", named, fixed = T) == TRUE | 
                grepl("Mean", named, fixed = TRUE) | 
                grepl("Median", named, fixed = TRUE) | 
                grepl("Q1", named, fixed = TRUE) | 
                grepl("Min", named, fixed = TRUE))
    named <- ifelse((tags == FALSE | tags2 == FALSE), 
                    paste("<b>", named, "</b>", sep = ""), 
                    named)
    named <- ifelse(tags == TRUE, 
                    paste("&nbsp;", "&nbsp;", "&nbsp;", named, sep = " "), 
                    named)
    output <- cbind(named, as.vector(tab[, 2:dim(tab)[2]]))
    #Get row lengths for each variable group in the table
    v <- (rle(tags)$length)[c(FALSE, TRUE)]+1
    #Specify colors for striping
    color <- c('#FFFFFF', stripe.col)
    
    return(htmlTable(as.matrix(output), 
                     rnames=FALSE, 
                     header=colnames, 
                     col.rgroup=unlist(mapply(rep, x=color, times=v), use.names=FALSE),
                     align = c("l", rep("r", ncol(output) - 1)), 
                     css.cell = "padding-left: .2em; padding-right: 2em;"))    
  }
}

#####################
# Plain text output #
#####################
out.plain <- function(tab, colnames=NULL) {
  output <- cbind(format(as.vector(tab[,1]), justify='left'), 
                  data.frame(tab[,2:dim(tab)[2]]))
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
    print('Package dependencies: library(htmlTable) for HTML output and library(xtable) for LaTeX output')
  }
  #if (any(c(cat.ptype, cont.ptype) != 'None')==TRUE & is.null(strat)==TRUE) {
    #warning('A stratifying variable must be identified for p-values to be computed. See strat=.')
  #}
  if (any(c(cat.varlist, cont.varlist) %in% ls('package:base'))==TRUE) {
    warning("Variables cannot take the names of any base R functions -- try 
            which(dput(colnames(dat)) %in% ls('package:base'))")
  }
  
  
  if (is.null(strat)) {
    cat.strat=rep(list(strat), length(cat.varlist))
    cont.strat=rep(list(strat), length(cont.varlist))
    strat.miss=0
    strat.rem=0
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
  else if (is.null(colnames)) {
    colnames <- colnames(tab)
  }
  
  #Define output 
  if (output=='plain') {
    out.plain(tab, colnames=colnames)
  }
  else if (output=='html') {
    out.html(tab, colnames=colnames)
  }
  else if (output=='latex') {
    out.latex(tab, colnames=colnames)
  } 
  }

################################
# Minimal input table function #
################################

quick.table <- function(dat,
                        strat=NULL,
                        dec=2,
                        colnames=NULL,
                        output='plain',
                        classlim=6
)
  
{
  #Warnings
  if (missing(dat)) {
    warning('A data frame must be provided in dat=.')
  }
  if ((output=='plain')==FALSE) {
    print('Package dependencies: library(htmlTable) for HTML output and library(xtable) for LaTeX output')
  }
  
  #Remove any variables with the same name as a base R function
  if ((any(c(names(dat)) %in% ls('package:base')))==TRUE) {
    warning('Variables cannot share the names of any base R functions.')
    warning(print(paste('Removed variable(s):', 
                        names(dat)[which(c(names(dat)) %in% ls('package:base'))],
                        sep=' ')))
    dat <- dat[,-which(c(names(dat)) %in% ls('package:base'))]
  }
  
  ## Classify continuous variables ##
  #Get all numeric variables with equal or more than classification limit (default 6) levels when factored
  cont <- dat[, sapply(dat, is.numeric)]
  cont <- dat[, sapply(dat, function(x) length(levels(as.factor(x)))>=classlim)]
  
  if (nrow(cont)>0 & ncol(cont)>0) {
    print('The following variables are summarized as continuous:')
  }
  cont.varlist <- dput(colnames(cont))
  cont.header=names(sapply(cont.varlist, FUN=get, simplify=F, USE.NAMES=T))
  
  ## Classify categorical variables ##
  #All variables that are not continuous; non-numeric or numeric with <classlim factorable levels
  cat <- dat[, -which(c(names(dat)) %in% c(names(cont)))]
  
  #Remove variables from table if specified as stratifying variables
  if (length(strat)>0) {
    cat <- cat[,-which(c(names(cat)) %in% strat)]
  }  
  
  if (nrow(cat)>0 & ncol(cat)>0) {
    print('The following variables are summarized as categorical:')
  }
  cat.varlist <- dput(colnames(cat))
  cat.header=names(sapply(cat.varlist, FUN=get, simplify=F, USE.NAMES=T))
  cat.rownames=lapply(sapply(cat.varlist, FUN=get, simplify=F, USE.NAMES=T), FUN=function(x) 
    as.vector(levels(as.factor(x))))
  
  #Define global options for the table
  pname=FALSE
  cont.ptype='None'
  cat.ptype='None'
  
  #Define options for no stratifying variable
  if (is.null(strat)) {
    cat.strat=rep(list(strat), length(cat.varlist))
    cont.strat=rep(list(strat), length(cont.varlist))
    strat.miss=0
    strat.rem=0
  }
  
  #Define options for stratifying variable 
  else {
    if (!is.null(strat)) {
      cat.strat=rep(list(interaction(sapply(strat, FUN=get, simplify=FALSE, USE.NAMES=TRUE))), length(cat.varlist))
      cont.strat=rep(list(interaction(sapply(strat, FUN=get, simplify=FALSE, USE.NAMES=TRUE))), length(cont.varlist))
      strat.miss=lapply(sapply(strat, FUN=get, simplify=FALSE, USE.NAMES=TRUE), function(x) sum(is.na(x)))
      strat.rem=sum(is.na(interaction(sapply(strat, FUN=get, simplify=FALSE, USE.NAMES=TRUE))))
    }
  }
  
  #Define options for stratifying variable
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
    out.plain(tab, colnames=colnames)
  }
  else if (output=='html') {
      out.html(tab, colnames=colnames)
    }
    else if (output=='latex') {
        out.latex(tab, colnames=colnames)
      }
}

#######################
# Statistical testing #
#######################
#When adding tests, define both p and the name of the test

stat.col <- function(var, strat, ptype, pname=FALSE) {
  
  ##One sample tests
  #One-sample t-test, mean=0
  if (ptype=='t.oneway') {
    p <- t.test(var, mu=0)$p.value
  }
  #One-sample median test, median=0
  else if (ptype=='wilcox.oneway') {
    p <- wilcox.test(var, mu=0)$p.value
  }
  #Binomial test, proportion=0.5
  else if (ptype=='prop.oneway') {
    p <- prop.test(sum(var, na.rm=T), sum(!is.na(var)), p=0.5)$p.value
  }
  
  ##Independent groups
  #Chi-square test
  else if (ptype=='chisq') {
    p <- chisq.test(var, strat)$p.value
  }
  #Fisher's exact test
  else if (ptype=='fisher') {
    p <- fisher.test(var, strat)$p.value
  }
  #t-test
  else if (ptype=='ttest') {
    p <- t.test(var[strat==levels(as.factor(strat))[1]], 
                var[strat==levels(as.factor(strat))[2]])$p.value
  }
  #One-way ANOVA
  else if (ptype=='anova') {
    p <- summary(aov(var~strat))[[1]][["Pr(>F)"]][[1]]
  }
  #Kruskal-Wallis
  else if (ptype=='kruskal') {
    p <- kruskal.test(var, strat)$p.value
  }
  #Wilcoxon rank-sum test
  else if (ptype=='wilcox') {
    p <- wilcox.test(var[strat==levels(as.factor(strat))[1]], 
                     var[strat==levels(as.factor(strat))[2]])$p.value
  }
  
  ##Dependent groups
  #Paired t-test
  else if (ptype=='ttest.pair') {
    p <- t.test(var[strat==levels(as.factor(strat))[1]], 
                var[strat==levels(as.factor(strat))[2]],
                paired=TRUE)$p.value
  }
  #Wilcoxon signed-rank test
  else if (ptype=='wilcox.pair') {
    p <- wilcox.test(var[strat==levels(as.factor(strat))[1]], 
                     var[strat==levels(as.factor(strat))[2]],
                     paired=TRUE)$p.value
  }
  #McNemar's test
  else if (ptype=='mcnemar') {
    p <- mcnemar.test(var, strat)$p.value
  }
  
  #Format p-values for consistency
  if (p>=0.001) {
    p <- format(round(p, 3), nsmall=3)
  }
  else {p <- '<0.001'}
  
  #Print test or statistic if requested
  if (pname==TRUE) {
    if (ptype=='t.oneway') {
      p.col <- c(p, '1-sample t-test')
    }
    else if (ptype=='wilcox.oneway') {
      p.col <- c(p, '1-sample median')
    }
    else if (ptype=='prop.oneway') {
      p.col <- c(p, 'Binomial test')
    }
    else if (ptype=='chisq') {
      p.col <- c(p, 'Chi-square')
    }
    else if (ptype=='fisher') {
      p.col <- c(p, 'Fisher exact')
    }
    else if (ptype=='ttest') {
      p.col <- c(p, 't-test')
    }
    else if (ptype=='anova') {
      p.col <- c(p, '1-way ANOVA')
    }
    else if (ptype=='kruskal') {
      p.col <- c(p, 'Kruskal-Wallis')
    }
    else if (ptype=='wilcox') {
      p.col <- c(p, 'Wilcoxon rank-sum')
    }
    else if (ptype=='ttest.pair') {
      p.col <- c(p, 'Paired t-test')
    }
    else if (ptype=='wilcox.pair') {
      p.col <- c(p, 'Wilcoxon signed-rank')
    }
    else if (ptype=='mcnemar') {
      p.col <- c(p, 'McNemar')
    }
  }
  else {p.col <- p}
  return(p.col)
}  