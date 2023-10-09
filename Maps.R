library(ggplot2)
library(maps)
library(mapdata)
library(ggstar)

usa <- map_data('usa')

ggplot(data=usa, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill='lightblue') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map') + 
  coord_fixed(1.3)

state <- map_data("state")
mississippi <- subset(state, region=="mississippi")
us_map <- ggplot(data=state, aes(x=long, y=lat, group=group)) + 
  geom_polygon(color = "black",fill="white") + 
  guides(fill=FALSE) + 
  geom_polygon(data=mississippi, fill="black")
theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
      axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) #+ 
#coord_fixed(1.3)
us_map
ggsave(filename="USLower48Map.tiff",us_map,
       width=7.5,height=4,units="in")

mississippi <- subset(state, region=="mississippi")
counties <- map_data("county")
mississippi_county <- subset(counties, region=="mississippi")

ms_map <- ggplot(data=mississippi, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=mississippi_county, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  ggtitle('Location of Limnocorral Experiment') + 
  geom_star(aes(y=34.428060,x=-89.391507),colour="black",fill="black",size=7,starshape=1) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ms_map
ggsave(filename="LCLocation.tiff",ms_map,
       width=4,height=7.5,units="in")