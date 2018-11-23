library(magrittr)
library(ggplot2)

##
# Theme function
bgcolor <- "#DCDCDC"
ftcolor <- "#003152"
def_wid <- 1500
def_hgt <- 700
title_size <- 5
subtitle_size <- 3
axis_size <- 2

theme_casey_wleg <- function(base_size = 11, base_family = ""){
  theme(rect = element_rect(fill = bgcolor),
        panel.background = element_rect(fill = bgcolor),
        legend.key = element_rect(fill = bgcolor),
        text = element_text(color = ftcolor),
        axis.line = element_line(color = ftcolor),
        axis.text = element_text(color = ftcolor, size = rel(axis_size), face = "bold"),
        axis.ticks = element_line(color = ftcolor),
        axis.text.x = element_text(color = ftcolor, angle = 45),
        #axis.title = element_text(color = ftcolor, size = rel(axis_size)),
        axis.title = element_blank(),
        plot.title = element_text(color = ftcolor, size = rel(title_size), face = "bold"),
        plot.subtitle = element_text(color = ftcolor, size = rel(subtitle_size), face = "bold.italic"),
        legend.text = element_text(color = ftcolor, size = rel(axis_size)),
        legend.title = element_text(color = ftcolor, size = rel(axis_size)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}

theme_casey_woleg <- function(base_size = 11, base_family = ""){
  theme(rect = element_rect(fill = bgcolor),
        panel.background = element_rect(fill = bgcolor),
        legend.key = element_rect(fill = bgcolor),
        text = element_text(color = ftcolor),
        #axis.line = element_line(color = ftcolor),
        axis.text = element_text(color = ftcolor, size = rel(axis_size), face = "bold"),
        axis.ticks = element_line(color = ftcolor),
        axis.text.x = element_text(color = ftcolor, angle = 45),
        #axis.title = element_text(color = ftcolor, size = rel(axis_size)),
        axis.title = element_blank(),
        plot.title = element_text(color = ftcolor, size = rel(title_size), face = "bold"),
        plot.subtitle = element_text(color = ftcolor, size = rel(subtitle_size), face = "bold.italic"),
        legend.position = "none",
        panel.grid.major = element_line(color = ftcolor),
        panel.grid.minor = element_blank())
}

##
# Read data
data <- readxl::read_excel("infographic - open data provinces.xlsx", 
                           sheet = "data2")

# 1 bar chart
totN <- data %>% 
  dplyr::filter(Type == 'open datasets') %>% 
  dplyr::select(-Type) 

totN <- totN[order(totN$Value),]

bar1 <- ggplot(data = totN) +
  geom_bar(
    mapping = aes(x = Province, y = Value, fill = Province),
    stat = 'identity'
    ) +
ggtitle("Number of Open Datasets") + 
  #labs(caption = paste0("Report produced by Abracadata team on ",Sys.Date())) +
  ggthemes::theme_wsj(base_size = 20)

# 2 published count

pubCnt <- data %>% 
  dplyr::filter(Province == 'NS' | Province == 'NB' | Province == 'PEI') %>% 
  dplyr::filter(Type == '2014'| Type == '2015' | Type == '2016'
         |Type == '2017' | Type == '2018')

bar2_h <- ggplot(data = pubCnt) +
  geom_bar(
    mapping = aes(x = Province, y = Value, fill = Type),
    stat = 'identity',  position=position_dodge()
  ) +
  ggtitle("Published Dataset Count") + 
  #labs(caption = paste0("Report produced by Abracadata team on ",Sys.Date())) +
  ggthemes::theme_wsj(base_size = 20)

bar2 <- ggplot(data = pubCnt) +
  geom_bar(
    mapping = aes(x = Type, y = Value, fill = Province),
    stat = 'identity'
  ) +
  ggtitle("bar1",subtitle="total count" ) + 
  #labs(caption = paste0("Report produced by Abracadata team on ",Sys.Date())) +
  ggthemes::theme_wsj(base_size = 20)
bar_v <- bar2 + coord_flip()


# 3 detail plot
detail <- data %>% 
  dplyr::filter(Type!= 'open datasets' & Type != '2014' & Type != '2015' 
         &Type != '2016' & Type != '2017' &
           Type != '2018')

for(i in 3:length(unique(detail$Province))){
  p <- unique(detail$Province)[i]
  data_tmp <- detail %>% 
    dplyr::filter(Province == p)

# -51 for BC and NS, 
png(filename= paste0("Picture/Province/",i,".png"), width = def_wid-105, height = def_wid,
    units = "px", pointsize = 12)

pie <- ggplot(data=data_tmp, 
               aes(x=Type, y=Value, fill = Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar() +
  ggtitle(paste0("Provice: ",p)) +
  ggthemes::theme_wsj(base_size = 30)
  
print(pie)
dev.off()
}

# Read reminding data

region <- readxl::read_excel("infographic - open data provinces v2.xlsx", 
                           sheet = "region")
view <- readxl::read_excel("infographic - open data provinces v2.xlsx", 
                           sheet = "view")
growth <- readxl::read_excel("infographic - open data provinces v2.xlsx", 
                             sheet = "growth")

colnames(region) <- c("Region","Visits Count","Visits Percentage")
region_subset <- region %>% 
  dplyr::filter(Region == "Nova Scotia" | Region == "New Brunswick" | Region == "Prince Edward Island")

# 4 region chart
pop <- ggplot(data = region_subset, aes(x= Region, y = `Visits Count`, color = Region)) +
  geom_point(size=15) + 
  geom_segment(aes(x=Region, 
                   xend=Region, 
                   y=0, 
                   yend=`Visits Count`,color = Region)) + 
  ggtitle("Viewers on Canada Open Data") + 
  #labs(title="region", 
  #     subtitle="", 
  #     caption=(paste0("Report produced by Abracadata team on ",Sys.Date()))) +
  ggthemes::theme_wsj(base_size = 20) + 
  theme(legend.position = "none")

nb_want_data <- ggplot(data = region_subset) +
  geom_bar(
    mapping = aes(x = Region, y = `Visits Count`, fill = Region),
    stat = 'identity'
  ) +
  ggthemes::theme_wsj(base_size = 20)

# view
bar3 <- ggplot(data = view) +
  geom_bar(
    mapping = aes(x = Province, y = Value, fill = Views),
    stat = 'identity'
  ) +
  ggtitle("Number of Views") + 
  #labs(caption = paste0("Report produced by Abracadata team on ",Sys.Date())) +
  ggthemes::theme_wsj(base_size = 20)# +
  #coord_flip()

# growth
growth <- tidyr::gather(growth, province, value, -time)
line1 <- ggplot(data = growth, aes(x = time, y = value, colour = province)) +
  geom_point() +
  geom_line(size = 2) +
  ggtitle("Number of Datasets Added") +
  #labs(caption = paste0("Report produced by Abracadata team on ",Sys.Date())) +
  ggthemes::theme_wsj(base_size = 20)


# output all
png(filename= "Picture/pop.png",width = def_wid, height = def_hgt,
    units = "px", pointsize = 12)
print(pop)
dev.off()

png(filename= "Picture/nb_want_data.png",width = def_wid, height = def_hgt,
    units = "px", pointsize = 12)
print(nb_want_data)
dev.off()

png(filename= "Picture/bar_v.png",width = def_wid, height = def_hgt,
    units = "px", pointsize = 12)
print(bar_v)
dev.off()

png(filename= "Picture/pop.png",width = def_wid, height = def_hgt,
    units = "px", pointsize = 12)
print(pop)
dev.off()

png(filename= "Picture/bar2_h.png",width = def_hgt+50, height = def_hgt,
    units = "px", pointsize = 12)
print(bar2_h)
dev.off()

png(filename= "Picture/bar1.png",width = def_wid, height = def_hgt,
    units = "px", pointsize = 12)
print(bar1)
dev.off()

png(filename= "Picture/bar3.png",width = def_hgt+200, height = def_hgt,
    units = "px", pointsize = 12)
print(bar3)
dev.off()

png(filename= "Picture/line.png",width = def_wid, height = def_hgt,
    units = "px", pointsize = 12)
print(line1)
dev.off()

# map
library(ggmap)
library(ggalt)
library(rgdal)

can = map_data("world", "Canada")
ggplot() + 
  geom_polygon(data = can, aes(x = long, y = lat, group = group)) + 
  coord_quickmap()

ggplot(data = can) + 
  geom_polygon(aes(x = long, y = lat, fill = group, group = group), color = ftcolor) + 
  coord_quickmap() +
  guides(fill = FALSE)
