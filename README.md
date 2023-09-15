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

## Use and example

See the [introduction][] article for basic use, including parsing and
indexing the PubMedCentral XML to a database for fast access to key
data.

Some simple observations from Bioconductor citations downloaded on
2023-09-08 include:

- [Publications][pub] grow expoonentially, perhaps at a decreased rate
  after 2010.
- PLoS ONE, Scientific Reports and Nature Communications are the three
  most [prominent venues][venue] for publications about
  *Bioconductor*.
- [Authors][author] mentioning *Bioconductor* are a little difficult
  to distinguish (shared surname and given names), but the most
  mentioning unambiguous author is 'Quackenbush'. 'Huber' is the most
  mentioning *Bioconductor* leader.
- Common [keywords][keyword] emphasize gene experession, RNA-Seq,
  methylation, transcriptomics, epigenetics, etc.
- Publications on edgeR, DESeq2, limma, and the original
  *Bioconductor* book are [cited by other publications][refpmid]
  mentioning *Bioconductor*.

[introduction]: https://mtmorgan.github.io/pmcbioc/articles/introduction.html
[pub]: https://mtmorgan.github.io/pmcbioc/articles/introduction.html#pub
[venue]: https://mtmorgan.github.io/pmcbioc/articles/introduction.html#venue
[author]: https://mtmorgan.github.io/pmcbioc/articles/introduction.html#author
[keyword]: https://mtmorgan.github.io/pmcbioc/articles/introduction.html#keyword
[refpmid]: https://mtmorgan.github.io/pmcbioc/articles/introduction.html#refpmid
