load_countries <- function() {
  sf::st_read(
    dsn = here::here("data", "IPBES_Regions_Subregions2.shp"),
    quiet = TRUE
  )
}


load_values <- function() {
  readxl::read_xlsx(
    path = here::here("data", "maps_respin_wp1.xlsx"),
    sheet = 1
  ) |>
    as.data.frame()
}


load_legends <- function() {
  readxl::read_xlsx(
    path = here::here("data", "maps_respin_wp1.xlsx"),
    sheet = 2
  ) |>
    as.data.frame()
}
