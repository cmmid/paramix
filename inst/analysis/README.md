
# QUICK START

You will need `gnumake` (or other `make` with compatible syntax support), `R`,
and the `renv` `R` package installed.

To run the analysis, go to a command prompt and enter:

```bash
path/to/this/directory$ make
```

You will be prompted by `renv` to agree to setting up the `renv` library.
The `socialmixr` dependency tree is sprawling, so will this may take a moment.

# DETAILS

This analysis comprises stages: building up inputs, transforming them to
outputs, and then visualizing those outputs. The explicit commands for that
process are specified in the `GNUmakefile`. The scripts behind those commands
are in the `scripts` folder.

The stage results are stored roughly as follows:

 - inputs downloaded into the `input` directory
 - outputs generated, then written to the `output` directory
 - figures generated, then written to the `figure` directory
