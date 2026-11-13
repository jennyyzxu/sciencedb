# Retrieve Rich Raw Metadata for a ScienceDB Dataset

Queries ScienceDB website by DOI (Digital Object Identifier) to retrieve
the complete raw backend metadata. This includes dataset metrics, raw
author details, file structures, licenses, and internal identifiers.

## Usage

``` r
sdb_dive(doi, raw = FALSE)
```

## Arguments

- doi:

  A character string specifying the DOI.

- raw:

  Default is false, which returned a cleaned dataframe.

## Value

A nested list containing the full API metadata response.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch complete raw metadata for a dataset
raw_info <- sdb_dive(doi = "10.57760/sciencedb.15594", raw = TRUE)

# Access raw backend elements
id <- raw_info[[1]]$id
access <- raw_info[[1]]$shareStatus
} # }
```
