# Retrieve Rich Raw Metadata for a ScienceDB Dataset

Queries ScienceDB website by DOI (Digital Object Identifier) to retrieve
the complete raw backend metadata. This includes dataset metrics, raw
author details, file structures, licenses, and internal identifiers.

## Usage

``` r
sdb_dive(doi)
```

## Arguments

- doi:

  A character string specifying the DOI (e.g.,
  "10.57760/sciencedb.15594").

## Value

A nested list containing the full API metadata response directly from
ScienceDB's backend service.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch complete rwa metadata for a dataset
raw_info <- sdb_dive("10.57760/sciencedb.15594")

# Access raw backend elements
id <- raw_info[[1]]$id
access <- raw_info[[1]]$shareStatus} # }
```
