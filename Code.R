library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-09-14')
library(tidyverse)
library(dplyr)
library(patchwork)
tuesdata$billboard->billboard
tuesdata$audio_features->audio
library(cowplot)

glimpse(billboard)
glimpse(audio)
glimpse(song)


billboard%>%left_join(audio, by="song_id")->song
lubridate::mdy(song$week_id)->song$week_id

song%>%filter(performer.x=="Taylor Swift")->ts
ts%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->tssongs1

ggplot(tssongs1, 
       aes(x=weeks_on_chart,
           y=reorder(song.x,weeks_on_chart, decreasing=TRUE)))+
  geom_col(fill="coral1")+
  geom_text(
    aes(label = weeks_on_chart), 
    hjust = 1, nudge_x = -.5, size=4, fontface="bold", colour="black"
  )+
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme(axis.text.y = element_text(size = 10, colour="white", hjust = 1, face="bold", family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+
  theme(axis.line = element_blank())+
  labs(title="Taylor Swift's Top Hits")+
  theme(plot.title = element_text(hjust = 0.5, size = 21, colour="white", face = "bold"))+
  theme(panel.grid = element_blank() )+
  theme(axis.title = element_blank(), axis.text.x = element_blank())+
  theme(plot.background  = element_rect(fill = "black"), panel.background = element_rect(fill="black"))->tsgraph
tsgraph

song%>%filter(performer.x=="Billie Eilish")->be
be%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->besongs1


ggplot(besongs1, 
       aes(x=weeks_on_chart,
           y=reorder(song.x,weeks_on_chart, decreasing=TRUE)))+
  geom_col(fill="springgreen1")+
  geom_text(
    aes(label = weeks_on_chart), 
    hjust = 1, nudge_x = -.5, size=6, fontface="bold", colour="black"
  )+
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme(axis.text.y = element_text(size = 10, colour="white", hjust = 1, face="bold", family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+
  theme(axis.line = element_blank())+
  labs(title="Billie Eilish's Top Hits")+
  theme(plot.title = element_text(hjust = 0.5, size = 21, colour="white", face = "bold"))+
  theme(panel.grid = element_blank() )+
  theme(axis.title = element_blank(), axis.text.x = element_blank())+
  theme(plot.background  = element_rect(fill = "black"), panel.background = element_rect(fill="black"))->begraph
begraph

song%>%filter(performer.x=="Dua Lipa")->dl
dl%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->dlsongs1
dlsongs1
ggplot(dlsongs1,
       aes(x=weeks_on_chart,
           y=reorder(song.x,weeks_on_chart, decreasing=TRUE)))+
  geom_col(fill="skyblue1", width=0.5)+
  geom_text(
    aes(label = weeks_on_chart), 
    hjust = 1, nudge_x = -.5, size=6, fontface="bold", colour="black"
  )+
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme(axis.text.y = element_text(size = 10, colour="white", hjust = 1, face="bold", family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+
  theme(axis.line = element_blank())+
  labs(title="Dua Lipa's Top Hits")+
  theme(plot.title = element_text(hjust = 0.5, size = 21, colour="white", face = "bold"))+
  theme(panel.grid = element_blank() )+
  theme(axis.title = element_blank(), axis.text.x = element_blank())+
  theme(plot.background  = element_rect(fill = "black"), panel.background = element_rect(fill="black"))->dlgraph
dlgraph

song%>%filter(performer.x=="Megan Thee Stallion")->mts
mts%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->mtssongs1
mtssongs1
ggplot(mtssongs1,
       aes(x=weeks_on_chart,
           y=reorder(song.x,weeks_on_chart, decreasing=TRUE)))+
  geom_col(fill="yellow", width=0.5)+
  geom_text(
    aes(label = weeks_on_chart), 
    hjust = 1, nudge_x = -.5, size=6, fontface="bold", colour="black"
  )+
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme(axis.text.y = element_text(size = 10, colour="white", hjust = 1, face="bold", family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+
  theme(axis.line = element_blank())+
  labs(title="Megan Thee Stallion's Top Hits")+
  theme(plot.title = element_text(hjust = 0.5, size = 21, colour="white", face = "bold"))+
  theme(panel.grid = element_blank() )+
  theme(axis.title = element_blank(), axis.text.x = element_blank())+
  theme(plot.background  = element_rect(fill = "black"), panel.background = element_rect(fill="black"))->mtsgraph1
mtsgraph1


song%>%filter(performer.x=="Lizzo")->l
l%>%filter(peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->lsongs1
lsongs1
ggplot(lsongs1,
       aes(x=weeks_on_chart,
           y=reorder(song.x,weeks_on_chart, decreasing=TRUE)))+
  geom_col(fill="wheat", width=0.5)+
  geom_text(
    aes(label = weeks_on_chart), 
    hjust = 1, nudge_x = -.5, size=6, fontface="bold", colour="black"
  )+
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme(axis.text.y = element_text(size = 10, colour="white", hjust = 1, face="bold", family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+
  theme(axis.line = element_blank())+
  labs(title="Lizzo's Top Hits")+
  theme(plot.title = element_text(hjust = 0.5, size = 21, colour="white", face = "bold"))+
  theme(panel.grid = element_blank() )+
  theme(axis.title = element_blank(), axis.text.x = element_blank())+
  theme(plot.background  = element_rect(fill = "black"), panel.background = element_rect(fill="black"))->lgraph1
lgraph1


song%>%filter(performer.x=="Ariana Grande")->ag
ag%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->agsongs1
agsongs1
ggplot(agsongs1,
       aes(x=weeks_on_chart,
           y=reorder(song.x,weeks_on_chart, decreasing=TRUE)))+
  geom_col(fill="lightsalmon")+
  geom_text(
    aes(label = weeks_on_chart), 
    hjust = 1, nudge_x = -.5, size=6, fontface="bold", colour="black"
  )+
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme(axis.text.y = element_text(size = 10, colour="white", hjust = 1, face="bold", family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+
  theme(axis.line = element_blank())+
  labs(title="Ariana Grande's Top Hits")+
  theme(plot.title = element_text(hjust = 0.5, size = 21, colour="white", face = "bold"))+
  theme(panel.grid = element_blank() )+
  theme(axis.title = element_blank(), axis.text.x = element_blank())+
  theme(plot.background  = element_rect(fill = "black"), panel.background = element_rect(fill="black"))->aggraph
aggraph

song%>%filter(performer.x=="Halsey")->h
h%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))->hsongs1
hsongs1
ggplot(hsongs1,
       aes(x=weeks_on_chart,
           y=reorder(song.x,weeks_on_chart, decreasing=TRUE)))+
  geom_col(fill="olivedrab",width=0.4)+
  geom_text(
    aes(label = weeks_on_chart), 
    hjust = 1, nudge_x = -.5, size=6, fontface="bold", colour="black"
  )+
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme(axis.text.y = element_text(size = 10, colour="white", hjust = 1, face="bold", family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+
  theme(axis.line = element_blank())+
  labs(title="Halsey's Top Hits")+
  theme(plot.title = element_text(hjust = 0.5, size = 21, colour="white", face = "bold"))+
  theme(panel.grid = element_blank() )+
  theme(axis.title = element_blank(), axis.text.x = element_blank())+
  theme(plot.background  = element_rect(fill = "black"), panel.background = element_rect(fill="black"))->hgraph
hgraph

song%>%filter(performer.x=="Tones And I")->ti
ti%>%
  filter(peak_position>=1 & peak_position<=10)%>%
  select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%
  slice(which.max(weeks_on_chart))->tisongs1
tisongs1

ggplot(tisongs1,
       aes(x=weeks_on_chart,
           y=reorder(song.x,weeks_on_chart, decreasing=TRUE)))+
  geom_col(fill="thistle", width=0.2)+
  geom_text(
    aes(label = weeks_on_chart), 
    hjust = 1, nudge_x = -.5, size=6, fontface="bold", colour="black"
  )+
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme(axis.text.y = element_text(size = 10, colour="white", hjust = 1, face="bold", family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+
  theme(axis.line = element_blank())+
  labs(title="Tones And I's Top Hits")+
  theme(plot.title = element_text(hjust = 0.5, size = 21, colour="white", face = "bold"))+
  theme(panel.grid = element_blank() )+
  theme(axis.title = element_blank(), axis.text.x = element_blank())+
  theme(plot.background  = element_rect(fill = "black"), panel.background = element_rect(fill="black"))->tigraph
tigraph

tsgraph+(begraph/dlgraph)->p
p

aggraph+(mtsgraph1/lgraph1)->p1

hgraph+tigraph->p2

p/p1/p2->p3

p3+ plot_annotation(
  title = 'Top female artists in Billboard 2020',
  subtitle = "These plots map the songs - that peaked in 1-10 spots - of the top 10 Billboard female artists  in Billboard. They are ordered based on the number of weeks spent in that spot",
  caption = 'Data: Data.World via Sean Miller|Design and Analysis: @annapurani93',
  theme = theme(plot.title = element_text(size = 16, face="bold", colour = "white"), 
                plot.subtitle = element_text(size = 12, colour = "white" ),
                plot.caption = element_text(size = 10, colour = "white"))
)->p4
p4&
  theme(plot.background = element_rect(color  = 'black', fill ="black"))->p4
p4

ggsave("p4.png",p4, width=21, height=21)
