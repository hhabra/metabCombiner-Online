# metabCombiner Online

This is a Shiny App implementation of metabCombiner and contains most of the functionality in the metabCombiner R package.


# Installation and Usage 

First download R version version 4.0 or later and run the following commands to install package dependencies:

```r
#one-time installation
install.packages(c("shiny", "shinyjs", "shinyFiles", "shinythemes", "devtools", "BiocManager"))
BiocManager::install("BiocStyle")
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
devtools::install_github("hhabra/metabCombiner", build_vignettes = TRUE)
```

Then run the following command to launch the app

```r
shiny::runGitHub('metabCombiner-Online', 'hhabra')

```

 
