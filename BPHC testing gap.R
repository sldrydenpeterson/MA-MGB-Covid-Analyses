testingmatchbos %>%
  filter(date==as.Date("2020-06-10"))
library(ggsci)
testingmatchbos%>% filter(town_population >36000) %>%
  ggplot( aes(x=date, y=testingrate.gap, group=Town, color=Town) )+
  theme_classic() +  theme(plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(size=1.5)+
  scale_color_npg(palette = c("nrc"), alpha = 1)+
  #geom_smooth(alpha=.7, size=1.5, method="loess", span= 0.1, se=FALSE) +
  labs(x="Date", y="Testing Gap (per 100,000)",
       title="Relative SARS-CoV-2 Weekly Testing Gap\nCity of Boston, 2020")+
  guides(color=FALSE) +
  coord_cartesian(xlim=c(as.Date("2020-06-01"), Sys.Date()+35), ylim=c(0,NA))+
  geom_label_repel(data=testingmatchbos %>% dplyr::filter(date==as.Date("2020-12-10") & town_population > 36000), 
                   aes(label=paste0(Town),  x = date, y = testingrate.gap, color=Town),
                   min.segment.length = 0) +
  scale_x_date(breaks = scales::pretty_breaks(10)) 

testingmatchbos %>% filter(!is.na(positivity1wk)) %>%
  group_by(date) %>%
  summarise(mean=mean(testingrate.gap) )%>%
  ggplot( aes(x=date, y=mean), color=as.factor(1))+
  theme_classic() +  theme(plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_col(fill="#374e55ff", size=0.15, color="#80796bff")+
  #geom_smooth(alpha=.7, size=2.5, method="loess", span= 0.7, se=FALSE) +
  geom_smooth( method="loess", color="#B24745FF",fill=NA, span=0.5, fill=NA)+
#  scale_color_viridis(discrete = TRUE, end=0.85, option = "C")+
  labs(x="Date", y="Testing Gap (per 100,000)",
       title="Relative SARS-CoV-2 Weekly Testing Gap\nCity of Boston, 2020")+
  guides(color=FALSE) +
  coord_cartesian(xlim=c(as.Date("2020-06-01"), Sys.Date()+35), ylim=c(0,NA))+
  scale_x_date(breaks = scales::pretty_breaks(10)) 


#maps
testingmatchbos

EquitytownbosGeo <-  left_join(townsgeobos, testingmatchbos %>% filter(date==as.Date("2020-12-10")), by = "Town")

library(data.table)
library(geosphere)
cent<- EquitytownbosGeo %>% 
  dplyr::select(long, lat, Town)
setDT(cent)

#make function make matrix (https://stackoverflow.com/questions/38699761/getting-the-centroids-of-lat-and-longitude-in-a-data-frame)
findCentroid <- function(long, lat, ...){
  centroid(cbind(long, lat), ...)
}

centroidsbos<-cent[, c("xname", "yname") := as.list(findCentroid(long, lat)), by = Town]
centroidsbos<-centroidsbos %>%
  distinct(Town, xname, yname) %>%
  mutate(xname= ifelse(Town=="Nantucket", xname+0.04, xname),
         yname= ifelse(Town=="Nantucket", yname-0.02, yname),
         yname= ifelse(Town=="Chilmark", yname+0.02, yname),
         yname= ifelse(Town=="Boston", yname-0.02, yname),
         xname= ifelse(Town=="Boston", xname+0.04, xname),
         xname= ifelse(Town=="Salem", xname-0.02, xname),
         xname= ifelse(Town=="Westport", xname-0.02, xname),
         yname= ifelse(Town=="Provincetown", yname+0.01, yname))

colors<-c(  "#80796BFF","#DF8F44FF", "#B24745FF", "#B24745FF")

bos1<-ggplot()+
  geom_polygon(data = EquitytownbosGeo,
               aes(x = long, y = lat, group = group, fill=positivity1wk), 
               colour = "white",  size = 0.05) +
  coord_quickmap() + theme_void() +
  theme(legend.position = c(.8, .3), 
        legend.direction = "vertical",
        panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(0,0.15),breaks = c(0, 0.05, 0.1),
                       name="Test Positivity\nin past week") 

bos2<-ggplot()+
  geom_polygon(data = EquitytownbosGeo,
               aes(x = long, y = lat, group = group, fill=testingrate), 
               colour = "white",  size = 0.05) +
  coord_quickmap() + theme_void() +
  theme(legend.position = c(.8, .3), 
        legend.direction = "vertical",
        panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(1000,8000),#breaks = c(0, 0.05, 0.1),
                       name="Testing Rate\nper 100,000\nin past week") 

bos3<-ggplot()+
  geom_polygon(data = EquitytownbosGeo,
               aes(x = long, y = lat, group = group, fill=testingrate.gap), 
               colour = "white",  size = 0.05) +
  coord_quickmap() + theme_void() +
  theme(legend.position = c(.8, .3), 
        legend.direction = "vertical",
        panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(0, NA),#breaks = c(0, 0.05, 0.1),
                       name="Testing Gap\nper 100,000\nin past week") 
library(patchwork)
bos1 + bos2 + bos3 + plot_layout(ncol=3)
ggsave("BOSgap.pdf", units = "in", width = 18, height=6)

  