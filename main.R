library(magrittr)
##
# Plot function
##
PlotOnGmap <- function(dataset, dataset2, name_col, lat_col, lon_col, gmap_key, gmap_style){
  tmp <- googleway::google_map(
    key = gmap_key,
    data = dataset,
    location = c(45.964993, -66.646332),  # Fallback option: Fredericton,
    styles = gmap_style,
    zoom = 12
  ) %>% 
    googleway::add_heatmap(
      lat = "Y", 
      lon = "X",
      option_radius = 0.003,
      option_gradient = viridis::plasma(256, alpha = 0.8, begin = 0.5, end = 1, direction = 1),
      option_opacity = 0.8
    ) %>% 
    googleway::add_markers(
      data = dataset2,
      lat = "Y",
      lon = "X",
      colour = "color_choice",
      #marker_icon = "icon_url",
      info_window = "Type",
      update_map_view = FALSE
    )
    
  
  return(tmp)
}


##
# Parameters
##
data_dir <- "./Dataset/"
map_style_dir <- "./MapStyle/"
gmap_api_key <- "AIzaSyBqXn0BmOkn5mCtyhpHh2ROPHX8YfVS-bg"
map_style_name <- c("assassin_creed","matriarchy","black_and_white","blue","blue_essence",
                    "gowalla","hcre","hopper","multibrand_network","simplex","teal_map")  # 3,4,6,10,
gmap_custom_style <- readr::read_file(paste0(map_style_dir, map_style_name[10], ".js"))
color_map <- data.frame(
  no_killed = 1:4,
  color_choice = c("blue", "lavender", "green", "red"),
  stringsAsFactors = FALSE
)

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

# count by hour
# comment: off-work time definitely wins here
by_hour <- dataset_clean %>% 
  dplyr::group_by(Hour_) %>% 
  dplyr::summarise(Count = n())

# count by day of week and hour
# comment: off-work time definitely wins here
by_day_of_week_and_hour <- dataset_clean %>% 
  dplyr::group_by(DayOfWeek, Hour_) %>% 
  dplyr::summarise(Count = n())

# count by street
# comment: not much info, regent and prospect is the winner
by_street <- dataset_clean %>% 
  dplyr::group_by(Street) %>% 
  dplyr::summarise(Count = n())

# count by type
# comment: suprising that a lot of parking lot accident
by_type <- dataset_clean %>% 
  dplyr::group_by(Type) %>% 
  dplyr::summarise(Count = n())

##
# Second wave of plot
dataset_injuried <- dataset_clean %>% 
  dplyr::filter(NoInjured > 1 | NoKilled > 0) %>% 
  dplyr::mutate(icon_url = "./Icon/Marker/m1.png")

# create marker data
dataset_marker <- dataset_injuried %>% 
  dplyr::filter(NoKilled > 0) %>%
  dplyr::inner_join(color_map, by = c("NoKilled" = "no_killed"))

dataset_final <- dataset_injuried

final_plot <- PlotOnGmap(
  dataset = dataset_final, 
  dataset2 = dataset_marker, 
  gmap_key = gmap_api_key, 
  gmap_style = gmap_custom_style
)

##
# Westmorland bridge plot
dataset_bridge <- dataset_clean %>% 
  dplyr::filter(Street == "Westmorland Street Bridge" | Near == "Westmorland Street Bridge")

# create marker data
dataset_marker <- dataset_bridge %>% 
  dplyr::filter(NoInjured > 0) %>%
  dplyr::inner_join(color_map, by = c("NoInjured" = "no_killed"))

dataset_final <- dataset_bridge

bridge_plot <- PlotOnGmap(
  dataset = dataset_final, 
  dataset2 = dataset_marker, 
  gmap_key = gmap_api_key, 
  gmap_style = gmap_custom_style
)
