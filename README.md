
# sneasy

*Kate Turetsky, 2021*

The sneasy package contains a variety of helper functions used to make
social network analysis easier. These functions were created for Group
Dynamics Lab members and affiliates. Anyone can use the functions, with
proper citation. However, many of the functions (e.g., `buildnet`) are
quite specific to the way we set up data and the type of networks we
study in our lab.

## Installation

You can install the sneasy package from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("kturetsky/sneasy")
```

## Contents

The sneasy package contains four categories of functions: (1) functions
for forming networks from collected data (`buildnet` and `attachatt`),
(2) functions for calculating node-level network metrics, such as
centrality (`centdf`), reciprocity (`tierecip`), and diversity
(`simpsd`, `homophily`, `groupcount`), (3) shortcut functions to use in
node permutation tests (`nodeperm` and `pperm`), and (4) a couple other
additional functions that are often handy during SNA (`gmc` and
`rescale01`). The package also contains sample data (`bionet` and
`attfile`).

See the vignette for this package here:
<https://kturetsky.github.io/sneasy/articles/Using-sneasy-package.html>
The vignette demonstrates how to use the sneasy package in social
network analysis, beginning with forming networks in R from collected
network data and ending with node permutation tests of significance.
