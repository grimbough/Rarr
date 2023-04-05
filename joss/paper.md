---
title: 'Rarr'
tags:
  - R
  - Bioconductor
  - zarr
  - out-of-memory
authors:
  - name: Mike L. Smith
    orcid: 0000-0002-7800-3848
    corresponding: true
    affiliation: 1
affiliations:
 - name: European Molecular Biology Laboratory (EMBL), Genome Biology Unit, Heidelberg, Germany
   index: 1
date: 20 March 2023
bibliography: paper.bib
---

# Summary

Bioconductor is an open-source software project and community that provides
tools for the analysis and comprehension of biological data. One of the key
missions of Bioconductor is to provide software infrastructure for common tasks
and datatypes, allowing researchers to focus on developing interoperable tools
for higher-level data analysis without reinventing the wheel.  As the volume of
data produced by experiments in molecular biology continues to grow,
outstripping advances in compute power, it has become imperative to provide
technical solutions that allow researchers to easily interact with datasets that
exceed the memory capacity of standard consumer hardware.  One such approach is
provided by the HDF5 (Hierarchical Data Format 5) file format, which allows
efficient access to subsets of compressed data on disk. Here we present the
current state-of-the-art for working with HDF5 files in Bioconductor.

# Statement of need

The Zarr file format has been developed for the storage of large multi-dimensional
arrays.  It's design is specifically focused to facilitate easy access to datasets
store in the cloud, and this has led to adoption of the Zarr file format across 
a wide range of scientific disciplines, including genomics, astrophysics and earth sciences [@geozarr].

# 


In order to reduce file sizes and data transfer costs, the Zarr specification allows for
a variety of compression tools that process chunks on their way to and from disk.
Rarr uses the built-in compression tools found in R to support using the zlib, gzip,
bzip2 and LZMA compression protocols.  Rarr is also bundled with a copy of the BLOSC
meta-compressor, which further provides support for compression using LZ4, snappy and ZSTD.



# Availability

The stable release version of Rarr can be found and installed from Bioconductor, while developmental
versions can be found on GitHub. We welcome code contributions, bug reports, fixes and
feature requests by opening issues and pull requests on the GitHub repository

<!--
# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"
-->

<!--
# Figures

# Figures can be included like this:
# ![Caption for example figure.\label{fig:example}](figure.png)
# and referenced from text using \autoref{fig:example}.
# 
# Figure sizes can be customized by adding an optional second parameter:
# ![Caption for example figure.](figure.png){ width=20% }
-->

# Acknowledgements


I would like to thank Jean-Karim Hériché and Christian Tischer for fruitful
discussions and access to example datasets during the package development.

# References