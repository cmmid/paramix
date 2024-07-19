
stopifnot("Must have `renv` installed." = require(renv))

res <- renv::restore(prompt = FALSE)

print(res)
