## Projection system ----

albers <- paste0(
  "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 ",
  "+y_0=0 +ellps=intl +units=m +no_defs"
)


## Create Europe (area w/ data) ----

europe <- countries[countries$"Area" %in% values$"Country_name", ]


## Create non data region (for display) ----

others <- countries[
  countries$"Area" %in%
    c(
      "Morocco",
      "Algeria",
      "Libya",
      "Tunisia",
      "Egypt",
      "Saudi Arabia",
      "Greenland",
      "Iran (Islamic Republic of)",
      "Iraq",
      "Palestine, State of",
      "Yemen",
      "Oman",
      "Bahrain",
      "Kuwait",
      "Qatar",
      "Syrian Arab Republic (the)",
      "United Arab Emirates (the)",
      "Lebanon",
      "Jordan",
      "Afghanistan",
      "Pakistan",
      "Mongolia",
      "China"
    ),
]


## Append two regions ----

world <- rbind(europe, others)
world <- sf::st_union(world)


## Crop regions ----

bbox <- sf::st_bbox(
  c("xmin" = -35, "ymin" = -25, "xmax" = 125, "ymax" = 90)
) |>
  sf::st_as_sfc() |>
  sf::st_as_sf(crs = 4326)

europe <- sf::st_intersection(europe, bbox)


## Project in Albers ----

europe <- sf::st_transform(europe, albers)
others <- sf::st_transform(others, albers)
world <- sf::st_transform(world, albers)


## Crop w/ map extent ----

bbox <- sf::st_bbox(
  c("xmin" = -3550000, "ymin" = 209000, "xmax" = 5250000, "ymax" = 5000000)
) |>
  sf::st_as_sfc() |>
  sf::st_as_sf(crs = sf::st_crs(europe))

europe <- sf::st_intersection(europe, bbox)
others <- sf::st_intersection(others, bbox)
world <- sf::st_intersection(world, bbox)


## Add data ----

europe <- merge(
  europe,
  values,
  by.x = "Area",
  by.y = "Country_name",
  all = FALSE
)
