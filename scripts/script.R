library(tidyverse)
library(sf)
library(tmap)
library(grid)
library(gridExtra)
library(ggpubr)
library(geobr)
library(lwgeom)
library(extrafont)

moc <- read_municipality(3143302, year = 2019)
rgint <- read_intermediate_region(3102, year = 2019)
mg <- read_state(31, year = 2019)
uf <- read_state("all", year = 2019)
mun_mg <- read_municipality(31)


map_moc <- ggplot() +
  geom_sf(
    data = mun_mg
  ) +
  geom_sf(
    data = moc,
    fill = "darkgrey",
    colour = "black"
  ) +
  scale_x_continuous(limits = c(st_bbox(moc)$xmin - (st_bbox(moc)$ymax - st_bbox(moc)$ymin)/2, st_bbox(moc)$xmax + (st_bbox(moc)$ymax - st_bbox(moc)$ymin)/2)) +
  scale_y_continuous(limits = c(st_bbox(moc)$ymin, st_bbox(moc)$ymax)) +
  theme_bw()

map_rgint <- ggplot() +
  geom_sf(
    data = uf,
    fill = "white"
  ) +
  geom_sf(
    data = rgint,
    fill = "lightgrey",
    colour = "black"
  ) +
  geom_sf(
    data = moc,
    fill = "darkgrey"
  ) +
  scale_x_continuous(limits = c(st_bbox(rgint)$xmin, st_bbox(rgint)$xmax)) +
  scale_y_continuous(limits = c(st_bbox(rgint)$ymin, st_bbox(rgint)$ymax))

map_uf <- ggplot() +
  geom_sf(
    data = uf,
    fill = "white",
    colour = "black"
  ) +
  geom_sf(
    data = mg,
    fill = "white",
    colour = "black"
  ) +
  geom_sf(
    data = rgint,
    fill = "darkgrey",
    colour = "black"
  ) +
  scale_x_continuous(limits = c(st_bbox(mg)$xmin, st_bbox(mg)$xmax)) +
  scale_y_continuous(limits = c(st_bbox(mg)$ymin, st_bbox(mg)$ymax)) + theme(
    panel.background = element_rect(fill = alpha("lightblue", 255)),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = alpha("black", 0)),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

carto <- arrangeGrob(
  map_moc,
  map_uf,
  text_grob(
    "
    Sistema de coordenadas geográficas\n
    DATUM: SIRGAS 2000 EPSG: 4674\n
    Fonte: IBGE, 2019\n
    Org.: ALVES, R. F.
    ",
    lineheight = .5
  ),
  layout_matrix = rbind(
    c(1, 1, 2),
    c(1, 1, 2),
    c(1, 1, 3)
  )
)

ggsave("./plots/rik.pdf", carto, width = 297, height = 210, units = "mm")
