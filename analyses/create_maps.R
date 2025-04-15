n_maps <- sort(unique(legends$"Map"))

for (i in n_maps) {
  leg <- legends[legends$Map == i, ]

  png(
    filename = here::here("figures", paste0("map-", i, ".png")),
    width = 210,
    height = 120,
    units = "mm",
    pointsize = 12,
    res = 300
  )

  par(mar = rep(0.75, 4), xaxs = "i", yaxs = "i", family = "serif")

  plot(
    x = sf::st_geometry(europe),
    lwd = 0.25,
    col = "#bebebe",
    border = "white"
  )

  rect(
    xleft = par()$usr[1],
    ybottom = par()$usr[3],
    xright = par()$usr[2],
    ytop = par()$usr[4],
    border = NA,
    col = "#c6ecff"
  )

  for (j in 1:nrow(leg)) {
    pos <- which(europe[, i + 5, drop = TRUE] == leg[j, "Value", drop = TRUE])
    if (length(pos) > 0) {
      plot(
        x = sf::st_geometry(europe[pos, ]),
        lwd = 0.25,
        col = leg[j, "Color"],
        border = "white",
        add = TRUE
      )
    }
  }

  plot(
    x = sf::st_geometry(others),
    lwd = 0.25,
    add = TRUE,
    border = "#bebebe",
    col = "white"
  )

  plot(
    x = sf::st_geometry(world),
    lwd = 0.25,
    col = NA,
    border = "#93dbff",
    add = TRUE
  )

  grats <- add_graticules(
    projection = sf::st_crs(europe),
    lwd = 0.25,
    col = "#bebebe",
    cex = 0.4,
    line = 0.5
  )

  box(col = "white", lwd = 2.0)
  box(col = "#bebebe", lwd = 0.5)

  text(
    x = par()$usr[1],
    y = par()$usr[4] - 1000000,
    labels = leg[1, "Title"],
    font = 2,
    cex = 0.65,
    pos = 4
  )

  for (j in 1:nrow(leg)) {
    ytop <- par()$usr[4] - 1200000 - ((j - 1) * 100000)

    rect(
      par()$usr[1] + 150000,
      ytop - 75000,
      par()$usr[1] + 300000,
      ytop,
      col = leg[j, "Color"],
      border = "#bebebe",
      lwd = 0.25
    )

    text(
      x = par()$usr[1] + 280000,
      y = ytop - 50000,
      labels = leg[j, "Legend"],
      pos = 4,
      cex = .5
    )
  }

  dev.off()
}
