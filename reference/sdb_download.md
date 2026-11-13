# Download All Dataset Files to Local Directory by DOI

Download All Dataset Files to Local Directory by DOI

## Usage

``` r
sdb_download(doi)
```

## Arguments

- doi:

  A character string specifying the DOI.

## Value

The path to the temporary directory with downloaded files. Users should
copy any files they want before the ending R session.

## Examples

``` r
if (FALSE) { # \dontrun{
sdb_download(doi = "10.57760/sciencedb.15594")
} # }
```
