# Download All Dataset Files to Local Directory by DOI

Download All Dataset Files to Local Directory by DOI

## Usage

``` r
sdb_download(doi, user_path)
```

## Arguments

- doi:

  A character string specifying the DOI (e.g.,
  "10.57760/sciencedb.15594").

- user_path:

  A character string specifying the local directory path where
  downloaded files will be saved.

## Value

Invisible. Files are saved directly to the specified user path.

## Examples

``` r
if (FALSE) { # \dontrun{
sdb_download(doi = "10.57760/sciencedb.15594",
             user_path = "~/Desktop")
} # }
```
