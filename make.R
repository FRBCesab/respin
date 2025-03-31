#' respin: A Research Compendium
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Nicolas Casajus \email{rdev.nc@gmail.com}
#'
#' @date 2025/03/31

## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all()


## Import data to map ----

values <- load_values()

values <- values[
  !(values$Country_name %in%
    c("Colombia", "Congo (the Democratic Republic of the)")),
]


## Import legend information ----

legends <- load_legends()


## Downlopad IPBES basemap ----

rutils::get_world_basemap(path = here::here("data"))
countries <- load_countries()


## Prepare regions ----

source(here::here("analyses", "prepare_data.R"))
source(here::here("analyses", "create_maps.R"))
