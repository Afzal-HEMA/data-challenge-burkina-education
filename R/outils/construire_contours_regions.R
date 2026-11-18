# ============================================================
# Construit data/referentiels/regions_bfa.geojson (13 régions, découpage 2016-2024)
# Source : Natural Earth 10m admin-1 (domaine public), 45 provinces fusionnées par région.
# Les contours Natural Earth suivent la frontière internationale du fond OpenStreetMap
# bien mieux que la version précédente.
# Ajoute pour chaque région un point d'étiquette bien à l'intérieur (point le plus
# éloigné des limites, cherché sur une grille), pour que le nom tienne dans la région.
# Usage (une seule fois, hors pipeline) :
#   Rscript R/outils/construire_contours_regions.R chemin/ne_10m_admin_1_states_provinces.geojson
# ============================================================
suppressMessages({ library(sf); library(dplyr) })
args <- commandArgs(TRUE)
src <- if (length(args)) args[1] else "ne_10m_admin_1_states_provinces.geojson"
sf_use_s2(FALSE)

prov <- st_read(src, quiet = TRUE) |> filter(adm0_a3 == "BFA") |> st_make_valid()
stopifnot(nrow(prov) == 45)
reg <- prov |>
  mutate(zone = recode(region, "Plateau-Central" = "Plateau Central")) |>
  group_by(zone) |> summarise(.groups = "drop") |> st_make_valid()
stopifnot(nrow(reg) == 13)

# point d'étiquette : point de la région le plus éloigné de sa limite (grille de ~2 km)
etiquette <- function(g) {
  g <- st_transform(st_sfc(g, crs = 4326), 32630)
  bb <- st_bbox(g); pas <- max(diff(bb[c(1, 3)]), diff(bb[c(2, 4)])) / 60
  pts <- st_make_grid(g, cellsize = pas, what = "centers")
  pts <- pts[lengths(st_intersects(pts, g)) > 0]
  d <- as.numeric(st_distance(pts, st_cast(g, "MULTILINESTRING")))
  st_coordinates(st_transform(pts[which.max(d)], 4326))
}
xy <- t(sapply(st_geometry(reg), etiquette))
reg$lng_etiquette <- round(xy[, 1], 4); reg$lat_etiquette <- round(xy[, 2], 4)

reg <- st_transform(reg, 4326)
sortie <- "data/referentiels/regions_bfa.geojson"
unlink(sortie)
st_write(reg, sortie, quiet = TRUE, layer_options = c("COORDINATE_PRECISION=5", "RFC7946=YES"))
cat("Écrit :", sortie, "-", round(file.size(sortie) / 1024), "Ko\n")
print(st_drop_geometry(reg))
