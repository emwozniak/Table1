# Table1
## Introduction
The `make.table` function in this package aids in the generation of flexible and reproducible descriptive summary tables. The results may be outputted as PDF LaTeX documents via Sweave or HTML files via knitr, or printed to the R console. An `attach`ed data frame must be provided, and categorical and continuous variables need to be specified separately with `cat.varlist` and `cont.varlist`, along with other optional vectorized inputs. Optional inputs for headers, row names, summary statistics, p-values, and table striping may be used to stylize the output. The table can be stratified by one or more variables.

As a less time-consuming (but less flexible) alternative, an `attach`ed data frame may be passed to `quick.table`, which dynamically classifies variables as either categorical or continuous and generates a complete set of summary statistics for all available variables.

## Flexible table options
#### Minimal required inputs
The following code generates a minimal table outputted to the console.

```r
library(survival)
library(htmlTable)
attach(pbc)
make.table(dat = pbc,
   cat.varlist = c('stage', 'sex'),
  cont.varlist = c('bili', 'copper'))
```

```r
       Variable       Overall
 sex                          
   Count                   418
   (%)                        
   m               44 (10.53%)
   f              374 (89.47%)
   Missing                   0
 bili                         
    Count                  418
    Mean (SD)      3.22 (4.41)
    Median (IQR)   1.40 (2.60)
    Q1, Q3           0.80, 3.4
    Min, Max          0.30, 28
    Missing                  0
 copper                       
    Count                  310
    Mean (SD)    97.65 (85.61)
    Median (IQR) 73.00 (81.75)
    Q1, Q3          41.25, 123
    Min, Max         4.00, 588
    Missing                108
 stage                        
   Count                   412
   (%)                        
   1               21 ( 5.10%)
   2               92 (22.33%)
   3              155 (37.62%)
   4              144 (34.95%)
   Missing                   6
```

#### Inferential statistics
A list of available statistical tests can be found in the help file `?stat.col`. Comparisons are performed across the specified straifying variable(s). For example,

```r
make.table(dat = pbc,
   cat.varlist = c('stage', 'sex'),
  cont.varlist = c('bili', 'copper'),
         strat = c('trt'),
     cat.ptype = c('fisher', 'fisher'),
    cont.ptype = c('wilcox', 'wilcox'))
```

```r
         Variable                    X1                    X2                Overall p.value
 sex                                                                                   0.377
   Count (%)               158 (50.64%)          154 (49.36%)                    312        
   (Row %)(Col %)                                                                           
   m               21 (58.33%) (13.29%)  15 (41.67%) ( 9.74%)  36 (100.00%) (11.54%)        
   f              137 (49.64%) (86.71%) 139 (50.36%) (90.26%) 276 (100.00%) (88.46%)        
   Missing                            0                     0                      0        
 bili                                                                                  0.842
    Count                           158                   154                    312        
    Mean (SD)               2.87 (3.63)           3.65 (5.28)            3.26 (4.53)        
    Median (IQR)            1.40 (2.40)           1.30 (2.88)            1.35 (2.62)        
    Q1, Q3                   0.80, 3.20            0.72, 3.60             0.80, 3.42        
    Min, Max                0.30, 20.00           0.30, 28.00            0.30, 28.00        
    Missing                           0                     0                      0        
 copper                                                                                0.718
    Count                           157                   153                    310        
    Mean (SD)             97.64 (90.59)         97.65 (80.49)          97.65 (85.61)        
    Median (IQR)          73.00 (81.00)         73.00 (96.00)          73.00 (81.75)        
    Q1, Q3                40.00, 121.00         43.00, 139.00          41.25, 123.00        
    Min, Max               9.00, 588.00          4.00, 558.00           4.00, 588.00        
    Missing                           1                     1                      2        
 stage                                                                                 0.205
   Count (%)               158 (50.64%)          154 (49.36%)                    312        
   (Row %)(Col %)                                                                           
   1               12 (75.00%) ( 7.59%)   4 (25.00%) ( 2.60%)  16 (100.00%) ( 5.13%)        
   2               35 (52.24%) (22.15%)  32 (47.76%) (20.78%)  67 (100.00%) (21.47%)        
   3               56 (46.67%) (35.44%)  64 (53.33%) (41.56%) 120 (100.00%) (38.46%)        
   4               55 (50.46%) (34.81%)  54 (49.54%) (35.06%) 109 (100.00%) (34.94%)        
   Missing                            0                     0                      0        
```

#### Label options
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
      colnames = c(' ', 'D-penicillamine', 'Placebo', 'Overall', 'p-value'))
```

```r
                           D-penicillamine               Placebo                Overall           p-value
 Gender                                                                                             0.377
   Count (%)                  158 (50.64%)          154 (49.36%)                    312      Fisher exact
   (Row %)(Col %)                                                                                        
   Male               21 (58.33%) (13.29%)  15 (41.67%) ( 9.74%)  36 (100.00%) (11.54%)                  
   Female            137 (49.64%) (86.71%) 139 (50.36%) (90.26%) 276 (100.00%) (88.46%)                  
   Missing                               0                     0                      0                  
 Bilirubin                                                                                          0.842
    Count                              158                   154                    312 Wilcoxon rank-sum
    Mean (SD)                  2.87 (3.63)           3.65 (5.28)            3.26 (4.53)                  
    Median (IQR)               1.40 (2.40)           1.30 (2.88)            1.35 (2.62)                  
    Q1, Q3                      0.80, 3.20            0.72, 3.60             0.80, 3.42                  
    Min, Max                   0.30, 20.00           0.30, 28.00            0.30, 28.00                  
    Missing                              0                     0                      0                  
 Copper                                                                                             0.718
    Count                              157                   153                    310 Wilcoxon rank-sum
    Mean (SD)                97.64 (90.59)         97.65 (80.49)          97.65 (85.61)                  
    Median (IQR)             73.00 (81.00)         73.00 (96.00)          73.00 (81.75)                  
    Q1, Q3                   40.00, 121.00         43.00, 139.00          41.25, 123.00                  
    Min, Max                  9.00, 588.00          4.00, 558.00           4.00, 588.00                  
    Missing                              1                     1                      2                  
 Disease stage                                                                                      0.205
   Count (%)                  158 (50.64%)          154 (49.36%)                    312      Fisher exact
   (Row %)(Col %)                                                                                        
   I                  12 (75.00%) ( 7.59%)   4 (25.00%) ( 2.60%)  16 (100.00%) ( 5.13%)                  
   II                 35 (52.24%) (22.15%)  32 (47.76%) (20.78%)  67 (100.00%) (21.47%)                  
   III                56 (46.67%) (35.44%)  64 (53.33%) (41.56%) 120 (100.00%) (38.46%)                  
   IV                 55 (50.46%) (34.81%)  54 (49.54%) (35.06%) 109 (100.00%) (34.94%)                  
   Missing                               0                     0                      0              
```

#### Export options
LaTeX and HTML output are supported in addition to printing to the console. HTML output includes a color hex option for zebra striping alternating variables.

## Quick table options
#### Minimal required input
Only an `attach`ed data frame needs to be passed to the function. The results will be printed to the R console if no output option is specified. Lists of variable classifications and warnings for any removed variables will be printed.

```r
quick.table(dat = pbc)
```

```r
[1] "Removed variable(s): chol"
[1] "The following variables are summarized as continuous:"
c("id", "time", "age", "bili", "albumin", "copper", "alk.phos", 
"ast", "trig", "platelet", "protime")
[1] "The following variables are summarized as categorical:"
c("status", "trt", "sex", "ascites", "hepato", "spiders", "edema", 
"stage")
        Variable           Overall
 id                               
    Count                      418
    Mean (SD)      209.50 (120.81)
    Median (IQR)   209.50 (208.50)
    Q1, Q3          105.25, 313.75
    Min, Max             1.00, 418
    Missing                      0
 time                             
    Count                      418
    Mean (SD)    1917.78 (1104.67)
    Median (IQR) 1730.00 (1520.75)
    Q1, Q3         1092.75, 2613.5
    Min, Max           41.00, 4795
    Missing                      0
 status                           
   Count                       418
   (%)                            
   0                  232 (55.50%)
   1                   25 ( 5.98%)
   2                  161 (38.52%)
   Missing                       0
 trt                              
   Count                       312
   (%)                            
   1                  158 (50.64%)
   2                  154 (49.36%)
   Missing                     106
 age                              
    Count                      418
    Mean (SD)        50.74 (10.45)
    Median (IQR)     51.00 (15.41)
    Q1, Q3            42.83, 58.24
    Min, Max          26.28, 78.44
    Missing                      0
 sex                              
   Count                       418
   (%)                            
   m                   44 (10.53%)
   f                  374 (89.47%)
   Missing                       0
 ascites                          
   Count                       312
   (%)                            
   0                  288 (92.31%)
   1                   24 ( 7.69%)
   Missing                     106
 hepato                           
   Count                       312
   (%)                            
   0                  152 (48.72%)
   1                  160 (51.28%)
   Missing                     106
 spiders                          
   Count                       312
   (%)                            
   0                  222 (71.15%)
   1                   90 (28.85%)
   Missing                     106
 edema                            
   Count                       418
   (%)                            
   0                  354 (84.69%)
   0.5                 44 (10.53%)
   1                   20 ( 4.78%)
   Missing                       0
 bili                             
    Count                      418
    Mean (SD)          3.22 (4.41)
    Median (IQR)       1.40 (2.60)
    Q1, Q3               0.80, 3.4
    Min, Max              0.30, 28
    Missing                      0
 albumin                          
    Count                      418
    Mean (SD)          3.50 (0.42)
    Median (IQR)       3.53 (0.53)
    Q1, Q3              3.24, 3.77
    Min, Max            1.96, 4.64
    Missing                      0
 copper                           
    Count                      310
    Mean (SD)        97.65 (85.61)
    Median (IQR)     73.00 (81.75)
    Q1, Q3              41.25, 123
    Min, Max             4.00, 588
    Missing                    108
 alk.phos                         
    Count                      312
    Mean (SD)    1982.66 (2140.39)
    Median (IQR) 1259.00 (1108.50)
    Q1, Q3            871.50, 1980
    Min, Max       289.00, 13862.4
    Missing                    106
 ast                              
    Count                      312
    Mean (SD)       122.56 (56.70)
    Median (IQR)    114.70 (71.30)
    Q1, Q3            80.60, 151.9
    Min, Max         26.35, 457.25
    Missing                    106
 trig                             
    Count                      282
    Mean (SD)       124.70 (65.15)
    Median (IQR)    108.00 (66.75)
    Q1, Q3              84.25, 151
    Min, Max            33.00, 598
    Missing                    136
 platelet                         
    Count                      407
    Mean (SD)       257.02 (98.33)
    Median (IQR)   251.00 (129.50)
    Q1, Q3             188.50, 318
    Min, Max            62.00, 721
    Missing                     11
 protime                          
    Count                      416
    Mean (SD)         10.73 (1.02)
    Median (IQR)      10.60 (1.10)
    Q1, Q3             10.00, 11.1
    Min, Max              9.00, 18
    Missing                      2
 stage                            
   Count                       412
   (%)                            
   1                   21 ( 5.10%)
   2                   92 (22.33%)
   3                  155 (37.62%)
   4                  144 (34.95%)
   Missing                       6
Warning messages:
1: In quick.table(dat = pbc) :
  Variables cannot share the names of any base R functions.
2: In quick.table(dat = pbc) : Removed variable(s): chol
```

#### Additional options
By default, `quick.table` classifies variables as categorical if they are factorable into six levels or fewer, and continuous otherwise. This default can be overridden with the `classlim` option. Further options include stratifying variable(s), number of decimal places, column names, and zebra striping for HTML output. All labeling information is pulled directly from variable names and factor levels.

```r
quick.table(dat = pbc,
          strat = 'trt',
       colnames = c(' ', 'D-penicillamine', 'Placebo', 'Overall'),
       classlim = 4,
            dec = 3)
```

```r
[1] "Removed variable(s): chol"
[1] "The following variables are summarized as continuous:"
c("id", "time", "age", "bili", "albumin", "copper", "alk.phos", 
"ast", "trig", "platelet", "protime", "stage")
[1] "The following variables are summarized as categorical:"
c("status", "sex", "ascites", "hepato", "spiders", "edema")
[1] "Total observations removed from table: 106"
[1] "Summary of total missing stratification variable(s):"
$trt
[1] 106

                        D-penicillamine               Placebo                Overall
 id                                                                                 
    Count                           158                   154                    312
    Mean (SD)            159.83 (87.35)        153.08 (93.22)         156.50 (90.21)
    Median (IQR)        156.00 (150.50)       156.50 (162.75)        156.50 (155.50)
    Q1, Q3                86.25, 236.75         68.00, 230.75          78.75, 234.25
    Min, Max               1.00, 311.00          5.00, 312.00           1.00, 312.00
    Missing                           0                     0                      0
 time                                                                               
    Count                           158                   154                    312
    Mean (SD)         2015.62 (1094.12)     1996.86 (1155.93)      2006.36 (1123.28)
    Median (IQR)      1895.00 (1401.50)     1811.00 (1618.25)      1839.50 (1506.25)
    Q1, Q3             1231.00, 2632.50      1153.00, 2771.25       1191.00, 2697.25
    Min, Max             41.00, 4556.00        51.00, 4523.00         41.00, 4556.00
    Missing                           0                     0                      0
 status                                                                             
   Count (%)               158 (50.64%)          154 (49.36%)                    312
   (Row %)(Col %)                                                                   
   0               83 (49.40%) (52.53%)  85 (50.60%) (55.19%) 168 (100.00%) (53.85%)
   1               10 (52.63%) ( 6.33%)   9 (47.37%) ( 5.84%)  19 (100.00%) ( 6.09%)
   2               65 (52.00%) (41.14%)  60 (48.00%) (38.96%) 125 (100.00%) (40.06%)
   Missing                            0                     0                      0
 age                                                                                
    Count                           158                   154                    312
    Mean (SD)             51.42 (11.01)         48.58 ( 9.96)          50.02 (10.58)
    Median (IQR)          51.93 (15.92)         48.11 (14.37)          49.79 (14.48)
    Q1, Q3                 42.98, 58.90          41.43, 55.80           42.24, 56.71
    Min, Max               26.28, 78.44          30.57, 74.52           26.28, 78.44
    Missing                           0                     0                      0
 sex                                                                                
   Count (%)               158 (50.64%)          154 (49.36%)                    312
   (Row %)(Col %)                                                                   
   m               21 (58.33%) (13.29%)  15 (41.67%) ( 9.74%)  36 (100.00%) (11.54%)
   f              137 (49.64%) (86.71%) 139 (50.36%) (90.26%) 276 (100.00%) (88.46%)
   Missing                            0                     0                      0
 ascites                                                                            
   Count (%)               158 (50.64%)          154 (49.36%)                    312
   (Row %)(Col %)                                                                   
   0              144 (50.00%) (91.14%) 144 (50.00%) (93.51%) 288 (100.00%) (92.31%)
   1               14 (58.33%) ( 8.86%)  10 (41.67%) ( 6.49%)  24 (100.00%) ( 7.69%)
   Missing                            0                     0                      0
 hepato                                                                             
   Count (%)               158 (50.64%)          154 (49.36%)                    312
   (Row %)(Col %)                                                                   
   0               85 (55.92%) (53.80%)  67 (44.08%) (43.51%) 152 (100.00%) (48.72%)
   1               73 (45.62%) (46.20%)  87 (54.37%) (56.49%) 160 (100.00%) (51.28%)
   Missing                            0                     0                      0
 spiders                                                                            
   Count (%)               158 (50.64%)          154 (49.36%)                    312
   (Row %)(Col %)                                                                   
   0              113 (50.90%) (71.52%) 109 (49.10%) (70.78%) 222 (100.00%) (71.15%)
   1               45 (50.00%) (28.48%)  45 (50.00%) (29.22%)  90 (100.00%) (28.85%)
   Missing                            0                     0                      0
 edema                                                                              
   Count (%)               158 (50.64%)          154 (49.36%)                    312
   (Row %)(Col %)                                                                   
   0              132 (50.19%) (83.54%) 131 (49.81%) (85.06%) 263 (100.00%) (84.29%)
   0.5             16 (55.17%) (10.13%)  13 (44.83%) ( 8.44%)  29 (100.00%) ( 9.29%)
   1               10 (50.00%) ( 6.33%)  10 (50.00%) ( 6.49%)  20 (100.00%) ( 6.41%)
   Missing                            0                     0                      0
 bili                                                                               
    Count                           158                   154                    312
    Mean (SD)               2.87 (3.63)           3.65 (5.28)            3.26 (4.53)
    Median (IQR)            1.40 (2.40)           1.30 (2.88)            1.35 (2.62)
    Q1, Q3                   0.80, 3.20            0.72, 3.60             0.80, 3.42
    Min, Max                0.30, 20.00           0.30, 28.00            0.30, 28.00
    Missing                           0                     0                      0
 albumin                                                                            
    Count                           158                   154                    312
    Mean (SD)               3.52 (0.44)           3.52 (0.40)            3.52 (0.42)
    Median (IQR)            3.56 (0.62)           3.54 (0.44)            3.55 (0.49)
    Q1, Q3                   3.21, 3.83            3.34, 3.78             3.31, 3.80
    Min, Max                 2.10, 4.64            1.96, 4.38             1.96, 4.64
    Missing                           0                     0                      0
 copper                                                                             
    Count                           157                   153                    310
    Mean (SD)             97.64 (90.59)         97.65 (80.49)          97.65 (85.61)
    Median (IQR)          73.00 (81.00)         73.00 (96.00)          73.00 (81.75)
    Q1, Q3                40.00, 121.00         43.00, 139.00          41.25, 123.00
    Min, Max               9.00, 588.00          4.00, 558.00           4.00, 588.00
    Missing                           1                     1                      2
 alk.phos                                                                           
    Count                           158                   154                    312
    Mean (SD)         2021.30 (2183.44)     1943.01 (2101.69)      1982.66 (2140.39)
    Median (IQR)      1214.50 (1187.25)     1283.00 (1027.25)      1259.00 (1108.50)
    Q1, Q3              840.75, 2028.00       922.50, 1949.75        871.50, 1980.00
    Min, Max           369.00, 11552.00      289.00, 13862.40       289.00, 13862.40
    Missing                           0                     0                      0
 ast                                                                                
    Count                           158                   154                    312
    Mean (SD)            120.21 (54.52)        124.97 (58.93)         122.56 (56.70)
    Median (IQR)         111.60 (74.79)        117.40 (68.12)         114.70 (71.30)
    Q1, Q3                76.73, 151.51         83.78, 151.90          80.60, 151.90
    Min, Max              26.35, 338.00         28.38, 457.25          26.35, 457.25
    Missing                           0                     0                      0
 trig                                                                               
    Count                           139                   143                    282
    Mean (SD)            124.14 (71.54)        125.25 (58.52)         124.70 (65.15)
    Median (IQR)         106.00 (61.50)        113.00 (70.50)         108.00 (66.75)
    Q1, Q3                84.50, 146.00         84.50, 155.00          84.25, 151.00
    Min, Max              33.00, 598.00         44.00, 432.00          33.00, 598.00
    Missing                          19                    11                     30
 platelet                                                                           
    Count                           156                   152                    308
    Mean (SD)           258.75 (100.32)       265.20 ( 90.73)         261.94 (95.61)
    Median (IQR)        255.00 (132.50)       259.50 (115.75)        257.00 (122.75)
    Q1, Q3               189.50, 322.00        206.75, 322.50         199.75, 322.50
    Min, Max              62.00, 563.00         71.00, 487.00          62.00, 563.00
    Missing                           2                     2                      4
 protime                                                                            
    Count                           158                   154                    312
    Mean (SD)              10.65 (0.85)          10.80 (1.14)           10.73 (1.00)
    Median (IQR)           10.60 (0.97)          10.60 (1.40)           10.60 (1.10)
    Q1, Q3                 10.03, 11.00          10.00, 11.40           10.00, 11.10
    Min, Max                9.00, 14.10           9.20, 17.10            9.00, 17.10
    Missing                           0                     0                      0
 stage                                                                              
    Count                           158                   154                    312
    Mean (SD)               2.97 (0.94)           3.09 (0.81)            3.03 (0.88)
    Median (IQR)            3.00 (2.00)           3.00 (1.00)            3.00 (2.00)
    Q1, Q3                   2.00, 4.00            3.00, 4.00             2.00, 4.00
    Min, Max                 1.00, 4.00            1.00, 4.00             1.00, 4.00
    Missing                           0                     0                      0
Warning messages:
1: In quick.table(dat = pbc, strat = "trt", colnames = c(" ", "D-penicillamine",  :
  Variables cannot share the names of any base R functions.
2: In quick.table(dat = pbc, strat = "trt", colnames = c(" ", "D-penicillamine",  :
  Removed variable(s): chol
```
