---
title: "Vignette Title"
author: "Vignette Author"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Data Mining Functions}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

# Organization of Data for Data Mining Functions
--------

In order to use the Data Mining functions in FuzzyR package, it is necessary for the input table to be in a wide format. The wide format for organization of data is illustrated in Table 1.


|RecID | DataColumn1 | DataColumn2 | DataColumn3 | DataColumn4 | ... | DataColumnN |
|------:|---------:|----------:|---------:|---------:|----:|---------:|
| 10001 | 0.109329 | 1.070871  | 8.972172 | 2.729732 | ... | 9.286801 |
| 10002 | 7.100293 | 23.338388 | 2.288310 | 6.793121 | ... | 2.682328 |
| 10003 | 5.661230 | 4.331081  | 2.078080 | 7.532731 | ... | 3.790279 |

The column RecID is used as an identifier and it is assumed that the contents in this column have unique values. In other words,
RecID is a unique identifier or primary key for a record. DataColumn1, DataColumn2...DataColumnN contain values for various attributes
associated with the identifier. In other words, these attributes represent features associated with unique identifiers. An
example could be age, weight, height, body mass index, blood pressure, blood glucose, etc. for various patients in a medical
study. In practice, some of these attributes could be NULL for a given identifier. Typically, one or more of these columns would be the *dependent* variables or *response* and the other columns would be the *independent* variables or *predictors* or *covariates*.
