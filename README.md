# paramix

Tools for calculating aggregate parameters and for disaggregating outcomes.

## Overview

For more complex infectious disease [compartmental models](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology), you may need to represent populations groups which discretize or group some continuous feature. For example, your model might use broad age groups, but you might have higher resolution data on critical processes by age. And once you've run your model, you might want outcomes associated with those processes at finer resolution for some follow on analyses.

The `paramix` package provides convenient functions to create both the correct aggregate parameters and to disaggregate outcomes.

## Installation

```r
remotes::install_github("cmmid/paramix")
```

## Usage

### Aggregation

To create parameters, you'll need a parameter function, a density weighting (either a function or series of values), and partition points.

### Disaggregation
