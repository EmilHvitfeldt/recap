
<!-- README.md is generated from README.Rmd. Please edit that file -->
recap
=====

<!-- badges: start -->
<!-- badges: end -->
The goal of recap is to provide various automatic summarization of text documents.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EmilHvitfeldt/recap")
```

Example
-------

``` r
example <- "A section of DNA that contains instructions to make a protein is called a gene. Each gene has the sequence for at least one polypeptide. Proteins form structures, and also form enzymes. The enzymes do most of the work in cells. Proteins are made out of smaller polypeptides, which are formed of amino acids. To make a protein to do a particular job, the correct amino acids have to be joined up in the correct order.

Proteins are made by tiny machines in the cell called ribosomes. Ribosomes are in the main body of the cell, but DNA is only in the nucleus of the cell. The codon is part of the DNA, but DNA never leaves the nucleus. Because DNA cannot leave the nucleus, the cell makes a copy of the DNA sequence in RNA. This is smaller and can get through the holes – pores – in the membrane of the nucleus and out into the cell.

Genes encoded in DNA are transcribed into messenger RNA (mRNA) by proteins such as RNA polymerase. Mature mRNA is then used as a template for protein synthesis by the ribosome. Ribosomes read codons, 'words' made of three base pairs that tell the ribosome which amino acid to add. The ribosome scans along an mRNA, reading the code while it makes protein. Another RNA called tRNA helps match the right amino acid to each codon."
```

``` r
library(recap)
recap_luhn(example)
#> # A tibble: 16 x 2
#>    sentence                                                           score
#>    <chr>                                                              <int>
#>  1 Ribosomes are in the main body of the cell, but DNA is only in th…     4
#>  2 Proteins are made by tiny machines in the cell called ribosomes.       3
#>  3 Because DNA cannot leave the nucleus, the cell makes a copy of th…     3
#>  4 A section of DNA that contains instructions to make a protein is …     2
#>  5 The codon is part of the DNA, but DNA never leaves the nucleus.        2
#>  6 Genes encoded in DNA are transcribed into messenger RNA (mRNA) by…     2
#>  7 Mature mRNA is then used as a template for protein synthesis by t…     2
#>  8 Ribosomes read codons, 'words' made of three base pairs that tell…     2
#>  9 The ribosome scans along an mRNA, reading the code while it makes…     2
#> 10 Proteins form structures, and also form enzymes.                       1
#> 11 The enzymes do most of the work in cells.                              1
#> 12 Proteins are made out of smaller polypeptides, which are formed o…     1
#> 13 To make a protein to do a particular job, the correct amino acids…     1
#> 14 This is smaller and can get through the holes – pores – in the me…     1
#> 15 Each gene has the sequence for at least one polypeptide.               0
#> 16 Another RNA called tRNA helps match the right amino acid to each …     0
```
