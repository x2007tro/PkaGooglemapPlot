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
    zoom = 15
  ) %>% 
    # googleway::add_markers(
    #   lat = "Y", 
    #   lon = "X",
    #   colour = "color_choice",
    #   #marker_icon = "icon_url",
    #   info_window = "Type",
    #   update_map_view = FALSE
    # ) %>% 
    googleway::add_heatmap(
      lat = "Y", 
      lon = "X",
      option_radius = 0.005,
      #option_gradient = c('orange', 'blue', 'mediumpurple4', 'snow4', 'thistle1'),
      option_opacity = 0.8
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
    {
"featureType": "all",
"elementType": "all",
"stylers": [
{
  "visibility": "on"
}
]
},
{
  "featureType": "all",
  "elementType": "labels",
  "stylers": [
  {
  "visibility": "off"
  },
  {
  "saturation": "-100"
  }
  ]
},
  {
  "featureType": "all",
  "elementType": "labels.text.fill",
  "stylers": [
  {
  "saturation": 36
  },
  {
  "color": "#000000"
  },
  {
  "lightness": 40
  },
  {
  "visibility": "off"
  }
  ]
  },
  {
  "featureType": "all",
  "elementType": "labels.text.stroke",
  "stylers": [
  {
  "visibility": "off"
  },
  {
  "color": "#000000"
  },
  {
  "lightness": 16
  }
  ]
  },
  {
  "featureType": "all",
  "elementType": "labels.icon",
  "stylers": [
  {
  "visibility": "off"
  }
  ]
  },
  {
  "featureType": "administrative",
  "elementType": "geometry.fill",
  "stylers": [
  {
  "color": "#000000"
  },
  {
  "lightness": 20
  }
  ]
  },
  {
  "featureType": "administrative",
  "elementType": "geometry.stroke",
  "stylers": [
  {
  "color": "#000000"
  },
  {
  "lightness": 17
  },
  {
  "weight": 1.2
  }
  ]
  },
  {
  "featureType": "landscape",
  "elementType": "geometry",
  "stylers": [
  {
  "color": "#000000"
  },
  {
  "lightness": 20
  }
  ]
  },
  {
  "featureType": "landscape",
  "elementType": "geometry.fill",
  "stylers": [
  {
  "color": "#4d6059"
  }
  ]
  },
  {
  "featureType": "landscape",
  "elementType": "geometry.stroke",
  "stylers": [
  {
  "color": "#4d6059"
  }
  ]
  },
  {
  "featureType": "landscape.natural",
  "elementType": "geometry.fill",
  "stylers": [
  {
  "color": "#4d6059"
  }
  ]
  },
  {
  "featureType": "poi",
  "elementType": "geometry",
  "stylers": [
  {
  "lightness": 21
  }
  ]
  },
  {
  "featureType": "poi",
  "elementType": "geometry.fill",
  "stylers": [
  {
  "color": "#4d6059"
  }
  ]
  },
  {
  "featureType": "poi",
  "elementType": "geometry.stroke",
  "stylers": [
  {
  "color": "#4d6059"
  }
  ]
  },
  {
  "featureType": "road",
  "elementType": "geometry",
  "stylers": [
  {
  "visibility": "on"
  },
  {
  "color": "#7f8d89"
  }
  ]
  },
  {
  "featureType": "road",
  "elementType": "geometry.fill",
  "stylers": [
  {
  "color": "#7f8d89"
  }
  ]
  },
  {
  "featureType": "road.highway",
  "elementType": "geometry.fill",
  "stylers": [
  {
  "color": "#7f8d89"
  },
  {
  "lightness": 17
  }
  ]
  },
  {
  "featureType": "road.highway",
  "elementType": "geometry.stroke",
  "stylers": [
  {
  "color": "#7f8d89"
  },
  {
  "lightness": 29
  },
  {
  "weight": 0.2
  }
  ]
  },
  {
  "featureType": "road.arterial",
  "elementType": "geometry",
  "stylers": [
  {
  "color": "#000000"
  },
  {
  "lightness": 18
  }
  ]
  },
  {
  "featureType": "road.arterial",
  "elementType": "geometry.fill",
  "stylers": [
  {
  "color": "#7f8d89"
  }
  ]
  },
  {
  "featureType": "road.arterial",
  "elementType": "geometry.stroke",
  "stylers": [
  {
  "color": "#7f8d89"
  }
  ]
  },
  {
  "featureType": "road.local",
  "elementType": "geometry",
  "stylers": [
  {
  "color": "#000000"
  },
  {
  "lightness": 16
  }
  ]
  },
  {
  "featureType": "road.local",
  "elementType": "geometry.fill",
  "stylers": [
  {
  "color": "#7f8d89"
  }
  ]
  },
  {
  "featureType": "road.local",
  "elementType": "geometry.stroke",
  "stylers": [
  {
  "color": "#7f8d89"
  }
  ]
  },
  {
  "featureType": "transit",
  "elementType": "geometry",
  "stylers": [
  {
  "color": "#000000"
  },
  {
  "lightness": 19
  }
  ]
  },
  {
  "featureType": "water",
  "elementType": "all",
  "stylers": [
  {
  "color": "#2b3638"
  },
  {
  "visibility": "on"
  }
  ]
  },
  {
  "featureType": "water",
  "elementType": "geometry",
  "stylers": [
  {
  "color": "#2b3638"
  },
  {
  "lightness": 17
  }
  ]
  },
  {
  "featureType": "water",
  "elementType": "geometry.fill",
  "stylers": [
  {
  "color": "#24282b"
  }
  ]
  },
  {
  "featureType": "water",
  "elementType": "geometry.stroke",
  "stylers": [
  {
  "color": "#24282b"
  }
  ]
  },
  {
  "featureType": "water",
  "elementType": "labels",
  "stylers": [
  {
  "visibility": "off"
  }
  ]
  },
  {
  "featureType": "water",
  "elementType": "labels.text",
  "stylers": [
  {
  "visibility": "off"
  }
  ]
  },
  {
  "featureType": "water",
  "elementType": "labels.text.fill",
  "stylers": [
  {
  "visibility": "off"
  }
  ]
  },
  {
  "featureType": "water",
  "elementType": "labels.text.stroke",
  "stylers": [
  {
  "visibility": "off"
  }
  ]
  },
  {
  "featureType": "water",
  "elementType": "labels.icon",
  "stylers": [
  {
  "visibility": "off"
  }
  ]
  }
  ]'

##
# Workflow
##

# Load data
dataset <- read.csv(paste0(data_dir, "Traffic_Accidents.csv"), header = TRUE, stringsAsFactors = FALSE)
colnames(dataset)[1] <- "X"
dataset_clean <- dataset %>% 
  dplyr::filter(!is.na(X))

# Plot
# basic_plot <- PlotOnGmap(dataset = dataset_clean, gmap_key = gmap_api_key, gmap_style = gmap_custom_style)

##
# Analysis

# count by # of injuried
# comment: consider only plot accidents with >0 injuries
by_injuried <- dataset_clean %>% 
  dplyr::group_by(NoInjured) %>% 
  dplyr::summarise(Count = n())

# count by # of killed
# comment: only 12 out of 9k involves death, useless
by_killed <- dataset_clean %>% 
  dplyr::group_by(NoKilled) %>% 
  dplyr::summarise(Count = n())

# count by year
# comment: no apparent trend, useless
by_acc_year <- dataset_clean %>% 
  dplyr::group_by(Year_) %>% 
  dplyr::summarise(Count = n())

# count by month
# comment: Jan and Feb has the highest accident due to weather probably
by_acc_month <- dataset_clean %>% 
  dplyr::filter(Month_ <= 12 & Month_ >= 1) %>% 
  dplyr::group_by(Month_) %>% 
  dplyr::summarise(Count = n())

# count by month and # injuried
# comment: Jan and Feb has the highest accident due to weather probably, same pattern
by_acc_month_and_injuried <- dataset_clean %>% 
  dplyr::filter(Month_ <= 12 & Month_ >= 1) %>% 
  dplyr::group_by(Month_, NoInjured) %>% 
  dplyr::summarise(Count = n())

# count by severity
# comment: no apparent trend, useless
by_severity <- dataset_clean %>% 
  dplyr::group_by(Severity) %>% 
  dplyr::summarise(Count = n())

# count by day_of_week
# comment: apparently accident happens more and more from Monday through Friday
by_day_of_week <- dataset_clean %>% 
  dplyr::group_by(DayOfWeek) %>% 
  dplyr::summarise(Count = n())

##
# Second wave of plot
color_map <- data.frame(
  no_injury = 2:5,
  color_choice = c("green", "blue", "lavender", "red"),
  stringsAsFactors = FALSE
)
dataset_injuried <- dataset_clean %>% 
  dplyr::filter(NoInjured > 1) %>% 
  dplyr::inner_join(color_map, by = c("NoInjured" = "no_injury")) %>% 
  dplyr::mutate(icon_url = "./Icon/Market/m1.png")

dataset_final <- dataset_injuried
final_plot <- PlotOnGmap(dataset = dataset_final, gmap_key = gmap_api_key, gmap_style = gmap_custom_style)
