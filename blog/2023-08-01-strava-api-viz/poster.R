# generate posters based on strava hikes with contour lines

pacman::p_load(rStrava, dplyr, sf, purrr, tmap, raster)
source(here::here("blog/2023-08-01-strava-api-viz/secrets.R"))

SAVE_PLOTS = TRUE

# create the authentication token
stoken <- httr::config(
  token = strava_oauth(
    app_name,
    app_client_id,
    app_secret,
    app_scope = "activity:read_all",
    cache = TRUE)) 

COLLECT_ACIVITIES = FALSE

if(COLLECT_ACIVITIES) {
  activities = stoken |> 
    get_activity_list() |> 
    compile_activities() |>
    filter(start_date_local >= as.Date("2023/07/15"),
           start_date_local <= as.Date("2023/07/23")) |> 
    mutate(start_date = reorder(start_date, min))
  
  write.csv(activities, "blog/2023-08-01-strava-api-viz/strava_activities.csv")
} else {
  activities = read.csv("blog/2023-08-01-strava-api-viz/strava_activities.csv") |> 
    filter(name != "Wapta Falls") |>
    mutate(order = lubridate::day(start_date), 
           name = reorder(name, order)) 
}

gp2sf <- function(gp) {
  gp |> 
    googlePolylines::decode() |> 
    map_dfr(
      function(df) {
        df |> 
          st_as_sf(coords = c("lon", "lat")) |> 
          st_combine() |> 
          st_cast("LINESTRING") |> 
          st_sf() 
      }) |> 
    pull(1)
}

# make posters

# get max bbox to offset each plot the same
bbox_list = lapply(split(activities, activities$name), FUN = function(x) {
  
  
  line_data = x |> 
    mutate(geom = gp2sf(map.summary_polyline)) |> 
    st_sf(crs = "EPSG:4326")
  
  # get topographic data & set boundary for plot
  st_bbox(line_data)}) |> 
  bind_rows() |> 
  summarise(dx = max(xmax - xmin), 
            dy = max(ymax - ymin))

delta_x = bbox_list$dx / 2
delta_y = bbox_list$dy / 1.5 # make more vertical

# landscape
posters_l = lapply(split(activities, activities$name), FUN = function(x) {
  
  if(x$name %in% c("Wilcox Viewpoint", "Edith Cavell Peak")) {
    city = "Jasper, AB"
  } else {
    city = "Yoho, BC"
  }
  
  title_cards = sprintf("%s\n%s\nDistance: %s km\nElevation: %s m", 
                        city, 
                        format(as.Date(x$start_date), "%B %e, %Y"),
                        round(x$distance, 2), 
                        round(x$elev_high - x$elev_low, 2))
  
  # generate hike path
  line_data = x |> 
    mutate(geom = gp2sf(map.summary_polyline)) |> 
    st_sf(crs = "EPSG:4326")
  
  # get topographic data & set boundary for plot
  bbox = st_bbox(line_data)
  lat = (bbox$ymax + bbox$ymin)/2
  lng = (bbox$xmax + bbox$xmin)/2
  padding = 0.001
  
  elevation_data <- getData("SRTM", lon = lng, lat = lat)
  zoomed_data = raster::crop(
    elevation_data, extent(
      lng - delta_x - padding, 
      lng + delta_x + padding,
      lat - delta_y - padding - 0.001, # for text in bottom
      lat + delta_y + padding)
  )
  contour_lines = rasterToContour(zoomed_data)
  
  # make plot 
  out = tm_shape(contour_lines) + 
    tm_iso(along.lines = TRUE) + 
    tm_shape(line_data) + 
    tm_lines(col = "grey20", lwd = 3) + 
    tm_style("classic") + 
    tm_layout(frame.double.line = TRUE) + 
    tm_credits(text = sprintf("%s - %s", x$name, city), 
               position = c("center", "BOTTOM"), bg.color = "white", align="center")
  
  
  if(SAVE_PLOTS) {
    # set parameters to be consistent with bbox
    width = 1200
    height = width * delta_x / delta_y
    tmap_save(asp = 0, tm = out, width = width / 1.5, height = height / 1.5, units = "px")
    }
  
  out
  
})

width = 1200
height = width * delta_x / delta_y
tmap_save(tmap_arrange(posters_l, nrow = 1), width = width*2, height = height/2, units = "px")

