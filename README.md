# Table1
## Introduction
The `make.table` function in this package aids in the generation of flexible and reproducible descriptive summary tables. The results may be outputted as PDF LaTeX documents via Sweave or HTML files via knitr, or printed to the R console. An `attach`ed data frame must be provided, and categorical and continuous variables need to be specified separately with `cat.varlist` and `cont.varlist`, along with other optional vectorized inputs. Optional inputs for headers, row names, summary statistics, p-values, and table striping may be used to stylize the output. The table can be stratified by one or more variables.

As a less time-consuming (but less flexible) alternative, an `attach`ed data frame may be passed to `quick.table`, which dynamically classifies variables as either categorical or continuous and generates a complete set of summary statistics for all available variables.

## Flexible table options
#### Minimal required inputs
The following code generates a minimal HTML table.

```r
library(survival)
library(htmlTable)
attach(pbc)
make.table(dat = pbc,
   cat.varlist = c('stage', 'sex'),
  cont.varlist = c('bili', 'copper'),
        output = 'html')
```

#### Inferential statistics
A list of available statistical tests can be found in the help file `?stat.col`. Comparisons are performed across the specified straifying variable(s). For example,

```r
make.table(dat = pbc,
   cat.varlist = c('stage', 'sex'),
  cont.varlist = c('bili', 'copper'),
         strat = c('trt'),
     cat.ptype = c('fisher', 'fisher'),
    cont.ptype = c('wilcox', 'wilcox'),
        output = 'html')
```

#### Aesthetic options
The table can be customized to be more informative using text headers, column names, row names for categorical variables, color hex for zebra striping, and descriptions of the statistical tests used. A list of all options can be viewed in the help file `?make.table`.

```r
make.table(dat = pbc,
   cat.varlist = c('stage', 'sex'),
    cat.header = c('Disease stage', 'Gender'),
  cat.rownames = list(c('I', 'II', 'III', 'IV'), c('Male', 'Female')),
  cont.varlist = c('bili', 'copper'),
   cont.header = c('Bilirubin', 'Copper'),
         strat = c('trt'),
     cat.ptype = c('fisher', 'fisher'),
    cont.ptype = c('wilcox', 'wilcox'),
         pname = TRUE,
      colnames = c(' ', 'D-penicillamine', 'Placebo', 'Overall', 'p-value'),
        output = 'html',
    stripe.col = 'f4dfd0')
```

## Quick table options
#### Minimal required input
Only an `attach`ed data frame needs to be passed to the function. The results will be printed to the R console if no output option is specified.

```r
quick.table(dat = pbc,
         output = 'html')
```

#### Additional options
By default, `quick.table` classifies variables as categorical if they are factorable into six levels or fewer, and continuous otherwise. This default can be overridden with the `classlim` option. Further options include stratifying variable(s), number of decimal places, column names, and zebra striping for HTML output. All labeling information is pulled directly from variable names and factor levels.

```r
quick.table(dat = pbc,
          strat = 'trt',
       colnames = c(' ', 'D-penicillamine', 'Placebo', 'Overall'),
       classlim = 4,
            dec = 3,
     stripe.col = 'f4dfd0',
         output = 'html'
```
