# pmcbioc

<!-- badges: start -->
<!-- badges: end -->

The goal of pmcbioc is to facilitate summary and analysis of
[PubMedCentral][] result sets. The PubMedCentral results are XML files
containing article metadata, the body of the article, and end matter
(citations, etc.). [pmcbioc][] parses the XML to extract article
metadata (title, year, author, keyword, reference PMID) to a database
for quick access. pmcbioc also indexes the PubMedCentral XML file and
provides facilities for rapidly querying the file.

[PubMedCentral]: https://pubmed.ncbi.nlm.nih.gov/
[pmcbioc]: https://mtmorgan.github.io/pmcbioc/

## Installation

Install the development version of [pmcbioc][] from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mtmorgan/pmcbioc")
```

## Use

See the [introduction][] article for basic use.

[introduction]: articles/introduction.html
