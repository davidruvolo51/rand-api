<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# random-api

A collection of API clients that I have built for various projects, but not really needed in as an R package.

## Available Api Clients

The following sections provide an overview of the available scripts located in `R/`.

### Grid.ac

Methods for downloading and processing [grid.ac](https://grid.ac/) releases.

```r
# download the latest release
g <- grid$new()
g$listReleases()
g$downloadRelease()
g$print()

saveRDS(g$data, "grid_data.RDS")
```

*See file `R/grid_ac.R`*

**Note**: This isn't affiliated with the GRID.ac project in anyway.

### Pubmed

A mini API client for retrieving publication metadata from Pubmed Api.

```r
# find publications with the term "R Core Team" (reference, author, etc.) that were published in 2021
p <- pubmed$new()
p$pubmed("(\"R Core Team\") AND ((\"2021\"[Date - Publication]))")
p$print()
```

*See file `R/pubmed.R`*
