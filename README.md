# invafish-sim
Collection of function to simulate the translocations and their impact of invasive freswhater fish. Intended for internal use in the project, and for documenting results.

## Installation

```r
install.packages("devtools")
devtools::install_github("NINAnor/invafish-sim")

```
## Orgainization
Organized as a R pacakge: see http://r-pkgs.had.co.nz/ for description on how to work with this, especially the (github section)[http://r-pkgs.had.co.nz/git.html] for introduction on how to work with this through github. 

All functions defined inside the R/ folder with files 1.dataIO, 2.wrangling etc.. are collections of main functions doing data import, data wrangling, etc etc.  Auxiliary functions should be stored in sepparate files named after the main function file (e.g. f_wrangling). 

See /vignettes for use-cases, descriptions and manuals. Please update the /vignettes/infafish-sim basic.Rmd with the most basic steps and functions as they are added to the repository. 
