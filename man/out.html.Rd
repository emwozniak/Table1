\name{out.html}
\alias{out.html}

\title{HTML Output}

\description{This function utilizes \code{\link{htmlTable}} to output the results of the \code{\link{make.table}} and \code{\link{quick.table}} functions to the R Viewer as a formatted HTML table, and may be used within a knitr document.
}
\usage{
out.html(tab, colnames, stripe, stripe.col, header.style,
factor.style, stat.style, caption, footer, tspanner, n.tspanner,
cgroup, n.cgroup, col.columns)
}

\arguments{
  \item{tab}{A \code{\link{make.table}} or \code{\link{quick.table}} data frame. Required.
  }
  \item{colnames}{A vector of column names for the table output. Optional.
  }
  \item{stripe}{Logical indicator for whether to include shaded zebra striping of every other variable in the output. Optional; defaults to TRUE.
  }
  \item{stripe.col}{Hex code for the color of the zebra striping. Optional; defaults to a light gray, #F7F7F7.
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
}
  
\details{
  A call to this function is an optional argument to both \code{\link{make.table}} and \code{\link{quick.table}} via output='html'.
  }
\value{A formatted HTML table in the Viewer or code for inclusion in a knitr document.}
%\references{
%
\author{Erica Wozniak}
%\note{
%}

\seealso{
  \code{\link{Table1}}
  \code{\link{make.table}}
  \code{\link{quick.table}}
  \code{\link{cat.var}}
  \code{\link{cont.var}}
  \code{\link{stat.col}}
  \code{\link{out.plain}}
  \code{\link{out.latex}}
}
\examples{
library(survival)
attach(pbc)

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