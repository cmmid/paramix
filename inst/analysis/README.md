
# QUICK START

You will need `gnumake` (or other `make` with compatible syntax support), `R`, and the `renv` `R` package installed.

To run the analysis, go to a command prompt and enter:

```bash
path/to/this/directory$ make -j4
```

The `-j4` option enables running the parts of the analysis in parallel. This analysis has a two-by-two scenario setup, so 4 is the maximum useful amount of parallelization. If your system has fewer cores (or highly constrained memory), feel free to use a smaller number or remove the option entirely for serial run only.

You will be prompted by `renv` to agree to setting up the `renv` environment. The `socialmixr` dependency tree is sprawling, so will this may take a moment.

# DETAILS

This analysis comprises stages: building up inputs, transforming them to outputs, and then visualizing those outputs. The explicit commands for that process are specified in the `GNUmakefile`. The scripts behind those commands are in the `scripts` folder.

The stage results are stored roughly as follows:

 - inputs downloaded into the `input` directory
 - outputs generated, then written to the `output` directory
 - figures generated, then written to the `figure` directory
