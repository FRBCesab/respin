#' @title Add graticules (parallels and meridians) to a map
#'
#' @description
#' This function adds graticules (parallels and meridians) to a map and returns
#' axes labels and coordinates.
#'
#' @param projection [string] the CRS of the map in the PROJ4 standard.
#' @param parallels [numeric] vector of latitudes to add parallels.
#' @param meridians [numeric] vector of longitudes to add meridians.
#' @param col [string] color of graticules.
#' @param lwd [numeric] width of graticules.
#' @param lty [integer] type of graticules.
#' @param add [boolean] If TRUE, graticules are added to the map.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.com}
#'
#' @export
#'
#' @return
#' This function returns a 2-elements list.
#'
#' A first data frame for the meridians with 4 columns:
#'   - x: the x-coordinates of meridians
#'   - y: the y-coordinates of meridians
#'   - label: the labels of meridians (in degrees)
#'   - direction: the direction, i.e. W(est) or E(ast)
#'
#' A second data frame for the parallels with 4 columns:
#'   - x: the x-coordinates of parallels
#'   - y: the y-coordinates of parallels
#'   - label: the labels of parallels (in degrees)
#'   - direction: the direction, i.e. S(outh) or N(orth)

add_graticules <- function(
  parallels = seq(-90, 90, by = 10),
  meridians = seq(-180, 180, by = 20),
  projection = NULL,
  col = par()$fg,
  lwd = par()$lwd,
  lty = par()$lty,
  las = 0,
  digits = 1,
  line = 0,
  add = TRUE,
  labels = TRUE,
  ...
) {
  # check_current_device()

  if (!is.null(parallels)) {
    if (!is.numeric(parallels) | sum(is.na(parallels))) {
      stop("Argument 'parallels' must be a numeric without NA.")
    }
  }

  if (!is.null(meridians)) {
    if (!is.numeric(meridians) | sum(is.na(meridians))) {
      stop("Argument 'meridians' must be a numeric without NA.")
    }
  }

  opar <- par()$xpd
  on.exit(par(xpd = opar))

  lon_bottom <- NULL
  lat_left <- NULL
  lon_top <- NULL
  lat_right <- NULL

  if (!is.null(parallels)) {
    ## Create Worldwide Parallels ----

    lats <- parallels
    at_lon <- seq(-180, 180, by = 0.01)
    geometry <- list()

    ## Create one Single Spatial Line per Parallel ----

    for (i in 1:length(lats)) {
      geometry[[i]] <- data.frame(
        x = at_lon,
        y = rep(lats[i], length(at_lon))
      ) |>
        data.matrix() |>
        sf::st_linestring() |>
        sf::st_sfc() |>
        sf::st_as_sf(crs = sf::st_crs(4326))
    }

    geometry <- do.call(rbind, geometry)

    parallels <- data.frame("latitude" = lats)
    sf::st_geometry(parallels) <- sf::st_geometry(geometry)

    ## Project Parallels (if required) ----

    if (!is.null(projection)) {
      parallels <- sf::st_transform(parallels, projection)
    }

    ## Crop with Map Extent ----

    area <- c(
      "xmin" = par()$usr[1],
      "ymin" = par()$usr[3],
      "xmax" = par()$usr[2],
      "ymax" = par()$usr[4]
    ) |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_as_sf(crs = sf::st_crs(parallels)$"epsg")

    parallels <- sf::st_crop(parallels, area)

    # ## Get y-axis (left) Coordinates ----

    y_axe <- data.frame(
      x = rep(par()$usr[1], 2),
      y = c(par()$usr[3], par()$usr[4])
    ) |>
      data.matrix() |>
      sf::st_linestring() |>
      sf::st_sfc() |>
      sf::st_as_sf(crs = projection)
    sf::st_geometry(y_axe) <- "geometry"

    lat_left <- data.frame()

    for (i in 1:nrow(parallels)) {
      y_ats <- sf::st_intersection(parallels[i, ], y_axe)

      if (nrow(y_ats) > 0) {
        at <- sf::st_drop_geometry(parallels[i, "latitude", drop = TRUE])

        label <- ifelse(
          test = at == 0,
          yes = sprintf("%.0f\u00B0", at),
          no = ifelse(
            test = at > 0,
            yes = sprintf(paste0("%.", digits, "f\u00B0N"), at),
            no = sprintf(paste0("%.", digits, "f\u00B0S"), -1 * at)
          )
        )

        lat_left <- rbind(
          lat_left,
          data.frame(
            x = sf::st_coordinates(y_ats)[, 1],
            y = sf::st_coordinates(y_ats)[, 2],
            label = label
          )
        )
      }
    }

    ## Get y-axis (right) Coordinates ----

    y_axe <- data.frame(
      x = rep(par()$usr[2], 2),
      y = c(par()$usr[3], par()$usr[4])
    ) |>
      data.matrix() |>
      sf::st_linestring() |>
      sf::st_sfc() |>
      sf::st_as_sf(crs = projection)
    sf::st_geometry(y_axe) <- "geometry"

    lat_right <- data.frame()

    for (i in 1:nrow(parallels)) {
      y_ats <- sf::st_intersection(parallels[i, ], y_axe)

      if (nrow(y_ats) > 0) {
        at <- sf::st_drop_geometry(parallels[i, "latitude", drop = TRUE])

        label <- ifelse(
          test = at == 0,
          yes = sprintf("%.0f\u00B0", at),
          no = ifelse(
            test = at > 0,
            yes = sprintf(paste0("%.", digits, "f\u00B0N"), at),
            no = sprintf(paste0("%.", digits, "f\u00B0S"), -1 * at)
          )
        )

        lat_right <- rbind(
          lat_right,
          data.frame(
            x = sf::st_coordinates(y_ats)[, 1],
            y = sf::st_coordinates(y_ats)[, 2],
            label = label
          )
        )
      }
    }

    ## Plot Parallels ----

    if (add) {
      plot(
        x = sf::st_geometry(parallels),
        add = TRUE,
        col = col,
        lwd = lwd,
        lty = lty
      )
    }

    ## Plot Axes ----

    if (labels) {
      par(xpd = TRUE)

      srt <- ifelse(las %in% c(0, 3), 90, 0)

      if (srt == 0) adj <- c(1 + line, NA) else adj <- c(NA, -1 * line)

      text(
        x = lat_left$x,
        y = lat_left$y,
        labels = lat_left$label,
        col = col,
        srt = srt,
        adj = adj,
        ...
      )

      if (srt == 0) adj <- c(-1 * line, NA) else adj <- c(NA, 1 + line)

      text(
        x = lat_right$x,
        y = lat_right$y,
        labels = lat_right$label,
        col = col,
        srt = srt,
        adj = adj,
        ...
      )
    }
  }

  if (!is.null(meridians)) {
    ## Create Worldwide Meridians ----

    lons <- meridians
    at_lat <- seq(-90, 90, by = 0.005)
    geometry <- list()

    ## Create one Single Spatial Line per Parallel ----

    for (i in 1:length(lons)) {
      geometry[[i]] <- data.frame(
        x = rep(lons[i], length(at_lat)),
        y = at_lat
      ) |>
        data.matrix() |>
        sf::st_linestring() |>
        sf::st_sfc() |>
        sf::st_as_sf(crs = sf::st_crs(4326))
    }

    geometry <- do.call(rbind, geometry)

    meridians <- data.frame("longitude" = lons)
    sf::st_geometry(meridians) <- sf::st_geometry(geometry)

    ## Project Parallels (if required) ----

    if (!is.null(projection)) {
      meridians <- sf::st_transform(meridians, projection)
    }

    ## Crop with Map Extent ----

    area <- c(
      "xmin" = par()$usr[1],
      "ymin" = par()$usr[3],
      "xmax" = par()$usr[2],
      "ymax" = par()$usr[4]
    ) |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_as_sf(crs = sf::st_crs(meridians)$"epsg")

    meridians <- sf::st_crop(meridians, area)

    ## Get x-axis (bottom) Coordinates ----

    x_axe <- data.frame(
      x = c(par()$usr[1], par()$usr[2]),
      y = rep(par()$usr[3], 2)
    ) |>
      data.matrix() |>
      sf::st_linestring() |>
      sf::st_sfc() |>
      sf::st_as_sf(crs = projection)
    sf::st_geometry(x_axe) <- "geometry"

    lon_bottom <- data.frame()

    for (i in 1:nrow(meridians)) {
      x_ats <- sf::st_intersection(meridians[i, ], x_axe)

      if (nrow(x_ats) > 0) {
        at <- sf::st_drop_geometry(meridians[i, "longitude", drop = TRUE])

        label <- ifelse(
          test = at == 0,
          yes = sprintf("%.0f\u00B0", at),
          no = ifelse(
            test = at > 0,
            yes = sprintf(paste0("%.", digits, "f\u00B0E"), at),
            no = sprintf(paste0("%.", digits, "f\u00B0W"), -1 * at)
          )
        )

        lon_bottom <- rbind(
          lon_bottom,
          data.frame(
            x = sf::st_coordinates(x_ats)[, 1],
            y = sf::st_coordinates(x_ats)[, 2],
            label = label
          )
        )
      }
    }

    ## Get x-axis (top) Coordinates ----

    x_axe <- data.frame(
      x = c(par()$usr[1], par()$usr[2]),
      y = rep(par()$usr[4], 2)
    ) |>
      data.matrix() |>
      sf::st_linestring() |>
      sf::st_sfc() |>
      sf::st_as_sf(crs = projection)
    sf::st_geometry(x_axe) <- "geometry"

    lon_top <- data.frame()

    for (i in 1:nrow(meridians)) {
      x_ats <- sf::st_intersection(meridians[i, ], x_axe)

      if (nrow(x_ats) > 0) {
        at <- sf::st_drop_geometry(meridians[i, "longitude", drop = TRUE])

        label <- ifelse(
          test = at == 0,
          yes = sprintf("%.0f\u00B0", at),
          no = ifelse(
            test = at > 0,
            yes = sprintf(paste0("%.", digits, "f\u00B0E"), at),
            no = sprintf(paste0("%.", digits, "f\u00B0W"), -1 * at)
          )
        )

        lon_top <- rbind(
          lon_top,
          data.frame(
            x = sf::st_coordinates(x_ats)[, 1],
            y = sf::st_coordinates(x_ats)[, 2],
            label = label
          )
        )
      }
    }

    ## Plot Meridians ----

    if (add) {
      plot(
        x = sf::st_geometry(meridians),
        add = TRUE,
        col = col,
        lwd = lwd,
        lty = lty
      )
    }

    ## Plot Axes ----

    if (labels) {
      par(xpd = TRUE)

      srt <- ifelse(las %in% c(2, 3), 90, 0)

      if (srt == 0) adj <- c(0.5, 1 + line) else adj <- c(NA, -1 * line)

      text(
        x = lon_bottom$x,
        y = lon_bottom$y,
        labels = lon_bottom$label,
        col = col,
        srt = srt,
        adj = adj,
        ...
      )

      if (srt == 0) adj <- c(0.5, -1 * line) else adj <- c(NA, 1 + line)

      text(
        x = lon_top$x,
        y = lon_top$y,
        labels = lon_top$label,
        col = col,
        srt = srt,
        adj = adj,
        ...
      )
    }
  }

  invisible(
    list(
      "bottom" = lon_bottom,
      "left" = lat_left,
      "top" = lon_top,
      "right" = lat_right
    )
  )
}
