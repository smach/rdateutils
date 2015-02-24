---
title: "rdateutils"
author: "Sharon Machlis"
output:
  md_document:
    variant: markdown_github
---

# rdateutils

This package is a collection of simple R date utilities to generate specific dates relative to the current date, in both date object and character string formats.

Available functions include yesterday.date(), yesterday.str(), first.of.last.month.date(), first.of.last.month.str(), end.of.last.month.date(), end.of.last.month.str() and so on. Run help(package="rdateutils") for a full list.

To install, you need to have the devtools package on your system (if you don't have it, install that with install.packages("devtools") ) and then run


```r
devtools::install.github("smach/rdateutils.R")
```
