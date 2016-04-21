\name{make.table}
\alias{make.table}

\title{Make Summary Table}

\description{This function produces summary statistics for categeorical and continuous variables and combines the results into a data frame for plain text, HTML, or LaTeX output, serving as a wrapper to apply vectorized inputs to \code{\link{cat.var}} and \code{\link{cont.var}}. Please note that the dataset input must be attached (see \code{\link{attach}}) and cleaned, e.g. blank cells must be replaced with NAs. 
}
\usage{
make.table(dat, cat.varlist, cat.header, cat.rownames, cat.ptype,
cont.varlist, cont.header, cont.ptype, strat, cat.rmstat, cont.rmstat,
dec, pname, colnames, output, stripe, stripe.col, header.style,
factor.style, stat.style, caption, footer, tspanner, n.tspanner,
cgroup, n.cgroup, col.columns, vspace)
}

\arguments{
  \item{dat}{
  Attached data frame input of all variables used for the summary table. Required. See \code{\link{attach}}.
  }
  \item{cat.varlist}{
  Vector of categorical variable names. Optional; defaults to NULL.
  }
  \item{cat.header}{
  Ordered vector of short descriptions for each categorical variable. Optional; defaults to variable names.
  }
  \item{cat.rownames}{
  Ordered list of vectors providing row names for each level of each vector found in cat.varlist. Optional; defaults to the coded value of each level.
  }
  \item{cat.ptype}{
  Ordered vector of tests requested to add p-values to table for categorical variables. Optional; defaults to no p-values. See \code{\link{stat.col}} for a complete list of options. 
  }
  \item{cont.varlist}{
  Vector of continuous variable names. Optional; defaults to NULL.
  }
  \item{cont.header}{
  Ordered vector of short descriptions for each continuous variable. Optional; defaults to variable name.
  }
  \item{cont.ptype}{
    Ordered vector of tests requested to add p-values to table for continuous variables. Optional; defaults to no p-values. See \code{\link{stat.col}} for a complete list of options. 
  }
  \item{strat}{
  Vector of one or more stratifying variables. Optional; defaults to no stratification.
  }
  \item{cat.rmstat}{
  Ordered list of vectors of statistics that should be removed from final summaries of categorical variables.
  }
  \item{cont.rmstat}{
    Ordered list of vectors of statistics that should be removed from final summaries of continuous variables.
  }
  \item{dec}{
  Integer value indicating the number of decimal places to be printed for each summary statistic. Optional; defaults to 2.
  }
  \item{pname}{
  Logical indicator for whether to print statistical test used beneath each p-value. Optional; defaults to FALSE.
  }
  \item{colnames}{
  Ordered vector of column names for table. Optional; defaults to levels of stratifying variable(s).
  }
  \item{output}{
  Choice of formatted table output: 'html', 'latex', or 'plain'. Optional; defaults to plain text printed in console.
  }
  \item{stripe}{
  HTML output only: logical indicator for whether to zebra stripe every other variable in the table. Optional; defaults to TRUE.
  }
  \item{stripe.col}{
  HTML output only: hex color code to be used for zebra striping the table. Optional; defaults to a light gray, #F7F7F7.
  }
  \item{header.style}{Variable name style: "plain", "bold", "italic", or "bolditalic". Optional; defaults to "bold".
  }
  \item{factor.style}{Variable factor levels style: "plain", "bold", "italic", or "bolditalic". Optional; defaults to "bold".
  }
  \item{stat.style}{Summary statistic style: "plain", "bold", "italic", or "bolditalic". Optional; defaults to "plain".
  }
  \item{caption}{Table caption. Optional; no default.
  }
  \item{footer}{Table footer. Optional; no default.
  }
  \item{tspanner}{Rowwise table spanner, passed to htmlTable. Optional; see ?htmlTable for default information.
  }
  \item{n.tspanner}{Vector of rows in each table spanner, passed to htmlTable. Optional; see ?htmlTable for default information.
  }
  \item{cgroup}{Column group headers, passed to htmlTable. Optional; see ?htmlTable for default information.
  }
  \item{n.cgroup}{Vector of columns under each group header, passed to htmlTable. Optional; see ?htmlTable for default information.
  }
  \item{col.columns}{Vector of column colors, passed to htmlTable. Optional; defaults to "none".
  }
  \item{vspace}{
  TRUE for additional vertical whitespace between variables (default)
  }
}

\value{
Returns a table of summary statistics for specified categorical and continuous variables in the requested output format. The plain text default returns a data frame to the console.
}

\author{
Erica Wozniak
}

\note{
A few miscellaneous notes: \cr
\cr
The data frame must be attached (see \code{\link{attach}}) prior to use of the function.
\cr
\cr
Table output to HTML and LaTeX require the package dependencies \code{\link{htmlTable}} and \code{\link{xtable}}
\cr
\cr
The user must ensure that no variable names share the name of a base R function; otherwise, the program will throw an error. 
\cr
\cr
By default, the final summary table is rearranged such that the variables in the table are in the same order as in the raw data set (dat=). 
\cr
\cr
If missing values occur in stratifying variables (strat=), then the observations with missings will be removed from the table, and a comment will be printed indicating the total number removed. If multiple stratifying variables are provided, look to the console for a summary of total missing observations for each variable.
\cr
\cr
A table may be constructed by applying \code{\link{rbind}} to individual calls to \code{\link{cat.var}} and \code{\link{cont.var}}, and generating HTML or LaTeX output using \code{\link{out.html}} or \code{\link{out.latex}}; \code{\link{make.table}} serves as a wrapper for these functions.
}

\seealso{
  \code{\link{Table1}}
  \code{\link{quick.table}}
  \code{\link{cat.var}}
  \code{\link{cont.var}}
  \code{\link{stat.col}}
  \code{\link{out.plain}}
  \code{\link{out.latex}}
  \code{\link{out.html}}
}

\examples{
library(survival)
attach(pbc)

##Example with non-HTML options
make.table(dat=pbc,
    cat.varlist=c('stage', 'sex'),
    cat.header=c('Stage', 'Sex'),
    cat.rownames=list(c('I', 'II', 'III', 'IV'), c('Male', 'Female')),
    cat.ptype=c('fisher', 'fisher'),   
    cont.varlist=c('bili', 'copper'),
    cont.header=c('Bilirubin', 'Urine copper'),
    cont.ptype=c('wilcox', 'wilcox'), 
    strat=c('trt'),
    #Remove row percents from all categorical variables (list is recycled)
    cat.rmstat=list('row'),
    #Remove missing, min/max from bili and no summary stats from copper
    cont.rmstat=list(c('miss', 'minmax'), 'None'),
    colnames=c('', 'D-penicillamine', 'Placebo', 'Overall', 'p-value')
    )
    
##Example with HTML options
library(htmlTable)
make.table(dat=pbc,
    cat.varlist=c('stage', 'sex'),
    cat.header=c('Stage', 'Sex'),
    cat.rownames=list(c('I', 'II', 'III', 'IV'), c('Male', 'Female')),
    cat.ptype=c('fisher', 'fisher'),   
    cont.varlist=c('bili', 'copper'),
    cont.header=c('Bilirubin', 'Urine copper'),
    cont.ptype=c('wilcox', 'wilcox'), 
    strat=c('trt'),
    cat.rmstat=list('row'),
    colnames=c('', 'D-penicillamine', 'Placebo', 'Overall', 'p-value'),
    
    output='html',   
    #Check out www.color-hex.com
    stripe.col='#f4dfd0'
    )
}
