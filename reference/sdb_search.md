# Search ScienceDB Datasets by Keyword

Executes a programmatic search against the ScienceDB platform using
internal query endpoints. It returns a cleaned and structured data frame
of dataset metadata.

## Usage

``` r
sdb_search(keyword = NULL)
```

## Arguments

- keyword:

  A character string specifying the search query or topic (e.g.,
  'personality', 'economics'). Default is 'NULL', which returns top
  default results.

## Value

A data frame containing the following columns:

- Title:

  English dataset title (falls back to Chinese title if English is
  missing).

- Author:

  Semicolon-separated list of author names.

- Keyword:

  Semicolon-separated list of English keywords.

- Publish:

  Publish date string.

- Doi:

  Digital Object Identifier associated with the dataset.

- Taxonomy:

  Semicolon-separated taxonomy categories.

- Language:

  Dataset language code/string.

- Introduction:

  Dataset abstract or description.

- Access:

  Access conditions (PUBLIC/RESTRICTED/EMBARGO).

- Views:

  Total page view count.

- Downloads:

  Total file download count.

- Cstr:

  China Science and Technology Resource identifier.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- sdb_search("personality")
head(results)
} # }
```
