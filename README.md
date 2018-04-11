# invafish-sim
Collection of function to simulate the translocations and their impact of invasive freswhater fish. The intentions is that the functions should support a workflow for a MSE type of model. The package is intended for use in the INVAFISH / ODYSSEUS projects, and serve both as interenal working platform and as documentation. 

Typicall use will be to collate functions defined in this package building a simulation workflow. 

## Installation

```r
install.packages("devtools")
devtools::install_github("NINAnor/invafish-sim")
library(invasim)

```
See vignette on [basic usage](https://github.com/NINAnor/INVAFISH-sim/blob/master/vignettes/invafish-sim%20basic.Rmd) for, well, basic usage... 

## Orgainization
Organized as a R pacakge: see http://r-pkgs.had.co.nz/ for description on how to work with this, especially the [github section](http://r-pkgs.had.co.nz/git.html) for introduction on how to work with this through github. 

All functions defined inside the R/ folder with files 1.dataIO, 2.wrangling etc.. are collections of main functions doing data import, data wrangling, etc etc.  Auxiliary functions should be stored in sepparate files named after the main function file (e.g. f_wrangling). Organize functions into logical namespaces. e.g. all get_... functions deal with data import, all wrangle_... deals with data wrangling etc..

See /vignettes for use-cases, descriptions and manuals. Please update the /vignettes/infafish-sim basic.Rmd with the most basic steps and functions as they are added to the repository. 
