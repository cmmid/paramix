# paramix

Tools for calculating aggregate parameters and for disaggregating outcomes.

## Overview

For more complex infectious disease [compartmental models](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology), you may need to represent populations groups which discretize or group some continuous feature. For example, your model might use broad age groups, but you might have higher resolution data on critical processes by age. And once you've run your model, you might want outcomes associated with those processes at finer resolution for some follow on analyses.

The `paramix` package provides convenient functions to create both the correct aggregate parameters and to disaggregate outcomes.

## Installation

```r
remotes::install_github("cmmid/paramix")
```

## Demo Analysis

In addition to the vignette introductory analysis, we provide a more extensive analysis pipeline, divided into stages and linked together via GNUMake. You will also need to have the R package `renv` installed.

You can create a copy of this pipeline from an R console:

```r
path_to_destination <- file.path("~", "Downloads", "paramixdemo") # or wherever
if (!dir.exists(path_to_destination)) { # if needed, create the directory
  dir.create(path_to_destination, recursive = TRUE)
}
# get the analysis pipeline
system.file("analysis", package = "paramix") |>
  list.files(full.names = TRUE, recursive = FALSE, include.dirs = TRUE) |>
  file.copy(
    from = _,
    to = path_to_destination,
    recursive = TRUE
  )
# should be a bunch of TRUEs indicating the files copied successfully
```

then go to where you created the copy and run the `make` command:

```bash
~/Downloads/paramixdemo$ make
```

This should result in a fair bit of environment setup (to ensure setup for reproducibility)

## Usage

Briefly, there are three steps:

 - create a translation object (`alembic`) based on the parameter function (e.g. the infection fatality ratio as a function of age), the density distribution of the pertinent feature (e.g. the population age pyramid), the model partitions of the feature (e.g. age groups), and the post-processing partitions of the feature (e.g. age in years).
 - create model parameters (`blend`) from the alembic
 - apply the alembic to model results (`distill`) to impute higher resolution outcomes
