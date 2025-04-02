
<!-- README.md is generated from README.Rmd. Please edit that file -->

# respin <img src="figures/readme/compendium-sticker.png" align="right" style="float:right; height:120px;"/>

<!-- badges: start -->
<!-- badges: end -->
<p align="left">
• <a href="#overview">Overview</a><br> • <a href="#data-sources">Data
sources</a><br> • <a href="#workflow">Workflow</a><br> •
<a href="#content">Content</a><br> •
<a href="#installation">Installation</a><br> •
<a href="#usage">Usage</a><br> •
<a href="#contributing">Contributing</a>
</p>

## Overview

The [RESPIN](https://respin-project.eu/) project (REinforcing
Science-Policy INterfaces for integrated biodiversity and climate
knowledge and policies) is looking to improve the uptake of existing
knowledge on biodiversity and climate in decision-making and to
strengthen collaboration between IPBES and IPCC. To do so, RESPIN aims
to identify gaps in the knowledge provision and develop ideas for how to
improve the engagement of experts.

This analysis focus on ongoing engagement in IPBES and IPCC processes
and existing capacity building activities for knowledge holders to
engage with IPBES and IPCC in Europe and Central Asia.

## Data sources

This project uses the IPBES regions and sub-regions layer available on
[Zenodo](https://zenodo.org/records/5719431).

## Content

This repository is structured as follow:

- [`DESCRIPTION`](https://github.com/frbcesab/respin/tree/main/DESCRIPTION):
  contains project metadata (authors, date, dependencies, etc.)

- [`make.R`](https://github.com/frbcesab/respin/tree/main/make.R): main
  R script to run the entire project

- [`R/`](https://github.com/frbcesab/respin/tree/main/R): contains R
  functions developed especially for this project

- [`analyses/`](https://github.com/frbcesab/respin/tree/main/analyses):
  contains R scripts to run analyses

- [`figures/`](https://github.com/frbcesab/respin/tree/main/figures):
  contains maps as PNG files

## Installation

To install this compendium:

- [Fork](https://docs.github.com/en/get-started/quickstart/contributing-to-projects)
  this repository using the GitHub interface.
- [Clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
  your fork using `git clone fork-url` (replace `fork-url` by the URL of
  your fork). Alternatively, open [RStudio
  IDE](https://posit.co/products/open-source/rstudio/) and create a New
  Project from Version Control.

## Usage

Launch the
[`make.R`](https://github.com/frbcesab/respin/tree/main/make.R) file
with:

``` r
source("make.R")
```

**Notes**

- All required packages listed in the `DESCRIPTION` file will be
  installed (if necessary)
- All required packages and R functions will be loaded
- Some analyses listed in the `make.R` might take time

## Contributing

All types of contributions are encouraged and valued. For more
information, check out our [Contributor
Guidelines](https://github.com/frbcesab/respin/blob/main/CONTRIBUTING.md).

Please note that this project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
