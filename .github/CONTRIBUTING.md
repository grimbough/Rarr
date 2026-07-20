# Contributing to Rarr

There are many ways you can contribute to Rarr. 
All contributions are very much welcome and only some of them require deep technical knowledge.
If you have something you would like to contribute, but you are not sure how, please don't hesitate to reach out by opening an [issue](https://github.com/Huber-group-EMBL/Rarr/issues) or sending an email.

## 🗣️ Spreading the word

The easiest (and possibly one of the most useful) way you can contribute to Rarr is by spreading the word. 
Please [cite it](https://huber-group-embl.github.io/Rarr/authors.html) in your publications and tell your friends and colleagues about it!

If you like it, please also consider adding a [star on GitHub](https://docs.github.com/en/get-started/exploring-projects-on-github/saving-repositories-with-stars).

## ✍️ Fixing typos

Small typos or grammatical errors in documentation may be edited directly using the GitHub web interface, so long as the changes are made in the _source_ file.
In other words, please edit a `.R` file in the `R/` folder, and not the `.Rd` files in the `man/` folder.
This is because this package uses [roxygen2](https://roxygen2.r-lib.org/) to automatically rebuild the `.Rd` files.

## 😥 Reporting bugs

If you think you found a bug in Rarr, even if you're unsure, please let us know.
The best way is to open an issue on GitHub: https://github.com/Huber-group-EMBL/Rarr/issues.

Please try to create a [reprex](https://reprex.tidyverse.org/) with the minimal amount of code required to reproduce the bug you encountered.

Please also include your session info (e.g. via the R command `sessioninfo::session_info()`).

Finally, if your issue relates to reading a specific file, remember to include said file.

## 🆕 Adding or requesting new extensions

If you find a file that Rarr cannot read yet, please open an issue or send an email with an example file.
If the data itself cannot be shared, please at least including the metadata (`zarr.json` or `.zattrs`)

If there is a file that Rarr cannot write yet, please send a link to the relevant specification or extension document.

## 🗳️ Voting for new features

Whenever possible, upcoming plans for Rarr as announced as GitHub issues.
If you are particularly interested in a specific feature and you would like to hel prioritise it, please use [the GitHub reactions feature](https://github.blog/2016-03-10-add-reactions-to-pull-requests-issues-and-comments/).

## 📖 Code of Conduct

Please note that Rarr has adopted [Bioconductor Code of Conduct](https://bioconductor.github.io/bioc_coc_multilingual/). 
By contributing to this project you agree to abide by its terms.
