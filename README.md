# The Generalizability of Personality Effects in Politics
---

### Description and data sources

Replication material for 'The Generalizability of Personality Effects in Politics' published in the European Journal of Personality. This repository contains all files required to produce the figures, tables and numerical information provided in the manuscript and supplementary material.

### Author/contact

- Joseph A. Vitriol, Harvard University, joe_vitriol@fas.harvard.edu
- Erik Gahner Larsen, University of Kent, E.G.Larsen@kent.ac.uk
- Steven G. Ludeke, University of Southern Denmark

### Repository content

- `01-representative-create_data.R` = R script used to create the datasets used for the analysis (requires original data)
- `02-representative-analysis.R` = R script used for all analyses in the article and supplementary material
- `personalitypolitics.csv` = Data used in manuscript (generated via `01-representative-create_data.R`)
- `sessionInfo.txt` = Output from `sessionInfo()` in R

### Session info

The analyses were made with [RStudio](http://www.rstudio.com/) (Version 1.1.463) with the following R session:

```
## R version 3.5.2 (2018-12-20)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS  10.15.1

## Matrix products: default
## BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods   base     

## other attached packages:
##  [1] magrittr_1.5     xtable_1.8-4     psych_1.8.12     scales_1.1.0     gridExtra_2.3    stargazer_5.2.2
##  [7] psy_1.1          forcats_0.4.0    stringr_1.4.0    dplyr_0.8.3      purrr_0.3.3      readr_1.3.1     
## [13] tidyr_1.0.0.9000 tibble_2.1.3     ggplot2_3.2.1    tidyverse_1.3.0

## loaded via a namespace (and not attached):
##  [1] tidyselect_0.2.5 haven_2.2.0      lattice_0.20-38  colorspace_1.4-1 vctrs_0.2.0.9007 generics_0.0.2  
##  [7] yaml_2.2.0       rlang_0.4.2.9000 pillar_1.4.2     foreign_0.8-72   glue_1.3.1       withr_2.1.2     
## [13] DBI_1.0.0        dbplyr_1.4.2     modelr_0.1.5     readxl_1.3.1     lifecycle_0.1.0  munsell_0.5.0   
## [19] gtable_0.3.0     cellranger_1.1.0 rvest_0.3.5      labeling_0.3     parallel_3.5.2   broom_0.5.2     
## [25] Rcpp_1.0.3       backports_1.1.5  jsonlite_1.6     farver_2.0.1     fs_1.3.1         mnormt_1.5-5    
## [31] hms_0.5.2        stringi_1.4.3    cli_1.1.0        tools_3.5.2      lazyeval_0.2.2   crayon_1.3.4    
## [37] pkgconfig_2.0.3  xml2_1.2.2       reprex_0.3.0     lubridate_1.7.4  assertthat_0.2.1 httr_1.4.1      
## [43] rstudioapi_0.10  R6_2.4.1         nlme_3.1-142     compiler_3.5.2  
```
