library(magrittr)
##
# Plot function
##
PlotOnGmap <- function(dataset, name_col, lat_col, lon_col, gmap_key, gmap_style){
  tmp <- googleway::google_map(
    key = gmap_key,
    data = dataset,
    location = c(45.964993, -66.646332),  # Fallback option: Fredericton,
    styles = gmap_style,
    zoom = 12
  ) %>% 
    googleway::add_markers(
      lat = "Y", 
      lon = "X",
      info_window = "Type",
      update_map_view = FALSE
    )
  
  return(tmp)
}


##
# Parameters
##
data_dir <- "./Dataset/"
gmap_api_key <- "AIzaSyBqXn0BmOkn5mCtyhpHh2ROPHX8YfVS-bg"
gmap_custom_style <- 
'[
{"featureType":"all","elementType":"all","stylers":[{"invert_lightness":false},{"saturation":-100},{"lightness":30},{"gamma":0.5},{"hue":"#435158"}]},
{"featureType":"road.arterial","elementType":"all","stylers":[{"color":"#ff7f7f"}]},
{"featureType":"transit.station","elementType":"labels.text","stylers":[{"visibility":"off"}]}
]'

##
# Workflow
##

# Load data
dataset <- read.csv(paste0(data_dir, "Traffic_Accidents.csv"), header = TRUE, stringsAsFactors = FALSE)
colnames(dataset)[1] <- "X"
dataset_clean <- dataset %>% 
  dplyr::filter(!is.na(X))
ppp <- PlotOnGmap(dataset_clean[,], "Type", "Y", "X", gmap_api_key, gmap_custom_style)
