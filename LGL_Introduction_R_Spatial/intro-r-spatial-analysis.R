library(tidyverse)
library(sf)
library(mapview)

# =============================
# set working folder
# setwd("~/YOUR/PATH")
# =============================

# gis data sets
gpkg_file = "gis_datasets.gpkg"
csv_file = "aerodromes.csv"

# list gpkg layers
st_layers(gpkg_file)

# read GeoPackage layers
island = st_read(dsn = gpkg_file, layer = "vancouver_island")
places = st_read(dsn = gpkg_file, layer = "places")
watercourses = st_read(dsn = gpkg_file, layer = "watercourses")
samples = st_read(dsn = gpkg_file, layer = "samples")

# verify class of these layers
class(places)

# glimpse
glimpse(places)

# data frame
places |>
  select(place_name_en)

# plot using base R
plot(island$geom)
plot(places$geom, add = TRUE)

# plot using ggplot
ggplot() +
  geom_sf(data = island) +
  geom_sf(data = places) +
  geom_sf_label(data = places[places$place_name_en == "Victoria", ], aes(label = place_name_en)) +
  geom_sf_text(data=island, aes(label = "Van Isle"))

# plot using mapview
mapview(list(island, places))

# better version
mapview(
  select(places, place_name_en, population_estimate),
  layer.name = "Populated Places",
  col.regions = "sienna1",
  label = "place_name_en",
  popup = TRUE
) +
  mapview(
    island,
    layer.name = "Vancouver Island",
    col.regions = "palegreen",
    alpha.regions = 0.3,
    label = NA,
    popup = FALSE
  )

# assign CRS
st_crs(samples) = 3005
samples

# read coordinates from a CSV file
csv = read_csv(csv_file)
glimpse(csv)

# create Lon/Lat (NAD83) points layer
aerodromes = st_as_sf(
  csv,
  coords = c('longitude', 'latitude'),
  crs = st_crs(4269),
  remove = FALSE
)

# reproject from NAD83 to BC Albers and extract coordinates
aerodromes_3005 = aerodromes %>% 
  st_transform(crs = 3005) %>% 
  mutate(bcalbers_x = st_coordinates(.)[, 1]) %>% 
  mutate(bcalbers_y = st_coordinates(.)[, 2])

# check results
aerodromes_3005 |>
  as.data.frame() |>
  select(longitude, latitude, bcalbers_x, bcalbers_y) |>
  head(5)

# read directly from ArcGIS Online
library(arcgislayers)

# Victoria neighborhoods
url_nhoods = "https://maps.victoria.ca/server/rest/services/OpenData/OpenData_Land/MapServer/4/query?outFields=*&where=1%3D1"
vic_nhoods = arc_open(url_nhoods) |> arc_select()

# Victoria business licences
url_business = "https://maps.victoria.ca/server/rest/services/OpenData/OpenData_PermitsAndLicences/MapServer/1/query?outFields=*&where=1%3D1"
vic_business = arc_open(url_business) |> arc_select()

mapview(
  list(vic_business, vic_nhoods),
  col.regions = list("green", "orange"),
  label = list(NA, "Neighbourhood"),
  legend = list(FALSE, FALSE),
  popup = list(FALSE, FALSE)
)

# find the nearest aerodrome indices
nearest = st_nearest_feature(places, aerodromes_3005)
nearest

# join the closest aerodrome name and calculate distance
places = places |>
  mutate(
    closest_aero = st_join(places, aerodromes_3005, st_nearest_feature)[["aerodrome_name"]],
    closest_dist = st_distance(places, aerodromes_3005[nearest, ], by_element = TRUE)
  )

# check results
places |>
  as.data.frame() |>
  select(place_name_en, closest_aero, closest_dist) |>
  head(5)

# select places within a 1-km buffer around rivers
flooded_places = places[lengths(st_intersects(places, st_buffer(watercourses, 1000)))>0,]

# check results
flooded_places |>
  as.data.frame() |>
  select(place_name_en) |>
  head(5)

# visualization of flooded places
mapview(places, col.region = "green", cex = 3) +
  mapview(flooded_places, col.region = "black", alpha.regions = 1, label = "place_name_en") +
  mapview(watercourses, color = "blue") +
  mapview(st_buffer(watercourses, 1000), col.region = "pink")

# create the city boundary
vic_boundary = vic_nhoods |> summarise()
mapview(vic_boundary)

# subset neighbourhoods: North Park
north_park = vic_nhoods |>
  filter(Neighbourhood == "North Park")

# select all business licences inside North Park
north_park_licences = vic_business[lengths(st_intersects(vic_business, north_park)) > 0, ]
mapview(
  north_park_licences,
  layer.name = paste("licences:", nrow(north_park_licences)),
  col.regions = "red") + north_park

# count number of business licences in each neighborhood
vic_nhoods$n_licences = lengths(st_intersects(vic_nhoods, vic_business))
mapview(
  filter(vic_nhoods, n_licences > 0),
  zcol = "n_licences",
  layer.name = "licences")

# using H3
library(h3jsr)

# get indices of h3 hexagons located inside the neighborhoods
h3_idxs = polygon_to_cells(
  geometry = st_transform(vic_nhoods, 4326),
  res = 10
)

# create hexagons
h3_nhoods = cell_to_polygon(
  input = unlist(h3_idxs),
  simple = FALSE
)

mapview(
  list(vic_nhoods, h3_nhoods),
  color = list("purple", "black"),
  alpha.regions = list(0.5, 0),
  legend = list(FALSE, FALSE)
)

# count number of licences per hexagon
h3_nhoods$n_licences = lengths(st_intersects(h3_nhoods, st_transform(vic_business, 4326)))

mapview(
  filter(h3_nhoods, n_licences > 0),
  zcol = "n_licences",
  layer.name = "licences")

# access cloud-optimized geospatial data
library(arrow)

# connect to Overture Maps buildings
buildings = open_dataset('s3://overturemaps-us-west-2/release/2024-09-18.0/theme=buildings?region=us-west-2')
nrow(buildings) # 2+ billion buildings

# download all buildings in Victoria
# extract bbox
vic_bbox = st_bbox(st_transform(vic_nhoods, 4326))
mapview(st_as_sf(st_as_sfc(vic_bbox))) + vic_nhoods

# read into memory all buildings in Victoria
vic_buildings = buildings |>
  filter(bbox$xmin > vic_bbox[1],
         bbox$ymin > vic_bbox[2],
         bbox$xmax < vic_bbox[3],
         bbox$ymax < vic_bbox[4]) |>
  select(id, geometry, height) |>
  collect() |>
  st_as_sf(crs = 4326) |>
  mutate(height = ifelse(is.na(height), 8, height))

# mapview
mapview(vic_buildings, layer.name = nrow(vic_buildings))

# 3D visualization with rdeck
# remotes::install_github("qfes/rdeck@*release")
library(rdeck)

rdeck(map_style = mapbox_dark(),
      initial_view_state = view_state(
        center = c(-123.3479, 48.45627),
        zoom = 11,
        bearing = 15,
        pitch = 85
      )) |>
  add_polygon_layer(
    data = vic_buildings,
    name = "Victoria, BC",
    get_polygon = geometry,
    get_elevation = height,
    get_fill_color = scale_color_linear(
      col = height,
      palette = viridisLite::inferno(100, direction = -1)
    ),
    extruded = TRUE,
    opacity = 0.2
  )

# visualization using mapgl
library(mapgl)

maplibre() |>
  add_navigation_control() |>
  add_fullscreen_control() |>
  add_scale_control() |>
  fit_bounds(vic_buildings) |>
  add_fill_extrusion_layer(
    id = "Buildings",
    source = vic_buildings,
    fill_extrusion_color = "orange",
    fill_extrusion_height = list(
      "interpolate",
      list("linear"),
      list("zoom"),
      10,
      0,
      16,
      list("get", "height")
    )
  ) |>
  add_layers_control(collapsible = TRUE)

# heatmap
maplibre(bounds = h3_nhoods) |>
  add_heatmap_layer(
    id = "Heatmap",
    source = vic_business,
    heatmap_opacity = 0.5,
    heatmap_radius = 20
  ) |>
  add_circle_layer(
    id = "Business Locations",
    source = vic_business,
    circle_color = "blue",
    min_zoom = 15,
    popup = "TRADE_NAME"
  )
