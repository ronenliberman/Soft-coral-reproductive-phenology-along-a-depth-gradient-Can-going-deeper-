install.packages("tidyverse")
library(reshape2)
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(scales)
library(gtable)
library(grid)
library(tidyverse)
library(ggsci) 
library(ggpubr)
library(ggfittext)
install.packages("ggpubr")
install.packages("ggfittext")
#d <- read_csv('All temp with depth.csv')
setwd("")
df <- read.csv("Data of raw temperature at 4 depth zones.csv")

#df <- All_temp_with_depth

View(df)

#str(d)
df <- df[c(1,3,4)]

df$date <- as.Date( df$date,format="%d/%m/%Y")
str(df)
#View(df)

#choosing colors for the temperature line in the plots 
mycolo <- c('#d7191c','#2ca25f','#2b83ba','#252525')
mylabes <- c( "5 m", "15 m", "30 m" , "45 m")
#filter the dates i want AND
# summarizing the temperature data for each year separately. 
t2016_mean <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2016-06-10")& date <= as.Date("2016-08-10")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(date, depth) %>%
  summarise(mean_temp= mean(temp)
            , sd= sd(temp,na.rm=TRUE),
            daily_max= max(temp),
            daily_min=min(temp))%>% 
  ungroup() 


view(t2016_mean)
#write.csv(file='summary_daily_temp_jun_jul_2017.csv', t2017_mean)
#
#t2017_mean <- read_csv('summary_daily_temp_jun_jul_2017.csv')
#t2019_mean <- summary_daily_temp_jun_jul_2019

t2016_mean$depth <- ordered(t2016_mean$depth, levels = c( "5", "15", "30" , "45"))

####################################################
t2017_mean <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2017-06-10")& date <= as.Date("2017-08-10")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(date, depth) %>%
  summarize(mean_temp= mean(temp)
            , sd= sd(temp,na.rm=TRUE),
            daily_max= max(temp),
            daily_min=min(temp)) %>% 
  ungroup() 

#write.csv(file='summary_daily_temp_jun_jul_2017.csv', t2017_mean)
#
#t2017_mean <- read_csv('summary_daily_temp_jun_jul_2017.csv')
#t2019_mean <- summary_daily_temp_jun_jul_2019

t2017_mean$depth <- ordered(t2017_mean$depth, levels = c( "5", "15", "30" , "45"))
#######################################################
t2018_mean <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2018-06-10")& date <= as.Date("2018-08-10")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(date, depth) %>%
  summarize(mean_temp= mean(temp)
            , sd= sd(temp,na.rm=TRUE),
            daily_max= max(temp),
            daily_min=min(temp)) %>% 
  ungroup()
t2018_mean$depth <- ordered(t2018_mean$depth, levels = c( "5", "15", "30" , "45"))
#############3####################################3  

 t2019_mean <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2019-06-10")& date <= as.Date("2019-08-10")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(date, depth) %>%
  summarize(mean_temp= mean(temp)
          , sd= sd(temp,na.rm=TRUE),
            daily_max= max(temp),
            daily_min=min(temp)) %>% 
            ungroup() 

 t2019_mean$depth <- ordered(t2019_mean$depth, levels = c( "5", "15", "30" , "45"))
 
 
##################################################
   t2020_mean <- df %>%
     mutate(depth = as.factor(depth)) %>% 
     filter(date >= as.Date("2020-06-10")& date <= as.Date("2020-08-10")) %>%  
     #  mutate(floor_date(ymd('2016-08-26'), "hour"))
     group_by(date, depth) %>%
     summarize(mean_temp= mean(temp)
               , sd= sd(temp,na.rm=TRUE),
               daily_max= max(temp),
               daily_min=min(temp)) %>% 
     ungroup() 

   #write.csv(file='summary_daily_temp_jun_jul_2017.csv', t2017_mean)
   #
   #t2017_mean <- read_csv('summary_daily_temp_jun_jul_2017.csv')
   #t2019_mean <- summary_daily_temp_jun_jul_2019
   
   t2020_mean$depth <- ordered(t2020_mean$depth, levels = c( "5", "15", "30" , "45"))
#write.csv(file='summary_daily_temp_jun_jul_2017.csv', t2017_mean)
#
#t2017_mean <- read_csv('summary_daily_temp_jun_jul_2017.csv')
#t2019_mean <- summary_daily_temp_jun_jul_2019


#view(t2019_mean)

#Here i create the annual average temp and s.brooding timing plot - 
#For duplicating this figure you MUST change the data and the dates according 
#to the year you want to produce

   avg_temp<- ggplot( t2019_mean, aes(date, mean_temp, group=depth))+
  geom_line(aes(color=depth), size=0.7) +
  geom_point(aes(color=depth), size=0.6)+ 
#  geom_smooth(aes(color=depth))+
  scale_color_manual(values = mycolo, labels= mylabes) +
#  scale_color_npg()+
  #  scale_x_datetime(date_breaks = "1 month",labels = date_format("%m"))+
  xlab("June                                            July")+
  ylab("Average daily temperature (°C)")+
  scale_x_date(breaks = seq(as.Date("2019-06-10"), as.Date("2019-08-10"), by="5 days")
               , date_labels = "%e")+  theme_bw() +
  scale_y_continuous(limits  = c(23, 29), breaks = seq(23, 29, by = 1))+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=10),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))
  #geom_hline(yintercept=26, linetype="dashed", 
   #          color = "black", size=0.3)
avg_temp  

avg_temp2016 <-  avg_temp  +  annotate("rect", xmin = as.Date("2016-06-21"), xmax = as.Date("2016-06-28"),
                                       ymin = -Inf, ymax = Inf, fill= "#E5AE86", alpha = .7)+
  #annotate("rect", xmin = as.Date("2016-07-08"), xmax = as.Date("2016-07-09"),
           #ymin = -Inf, ymax = Inf, fill= "#E5AE86", alpha = .7)+
  annotate("rect", xmin = as.Date("2016-07-08"), xmax = as.Date("2016-07-16"),
           ymin = -Inf, ymax = Inf, fill= "#54278f", alpha = .6) +
  annotate("text", x = as.Date("2016-06-24"),
         y = 28.5, color= "black", label = "<23 m", size = 3)+
  annotate("text", x = as.Date("2016-07-12"),
           y = 28.5, color= "black", label = "<45 m", size = 3)+
  annotate("text", x = as.Date("2016-08-10"),
           y = 28.9, color= "black", label = "2016",fontface=2, size = 4)+
  annotate("text", x = as.Date("2016-06-10"),
           y = 28.9, color= "black", label = "a",fontface=2, size = 4)+
  theme(axis.title.y=element_blank())
  avg_temp2016

  avg_temp2017 <-  avg_temp  +   annotate("rect", xmin = as.Date("2017-07-04"), xmax = as.Date("2017-07-11"),
                                        ymin = -Inf, ymax = Inf, fill= "#E5AE86", alpha = .7)+
  annotate("rect", xmin = as.Date("2017-07-19"), xmax = as.Date("2017-07-25"),
           ymin = -Inf, ymax = Inf, fill= "#778899", alpha = .6)+
  annotate("text", x = as.Date("2017-07-07"),
                y = 28.25, color= "black", label = "<30 m", size = 3)+
  annotate("text", x = as.Date("2017-07-22"),
           y = 28.25, color= "black", label = "30-45 m", size = 3)+
  annotate("text", x = as.Date("2017-08-10"),
           y = 28.9, color= "black", label = "2017",fontface=2, size = 4)+
annotate("text", x = as.Date("2017-06-10"),
         y = 28.9, color= "black", label = "b",fontface=2, size = 4)+
  theme(axis.title.y=element_blank())
avg_temp2017


avg_temp2018 <-  avg_temp  +annotate("rect", xmin = as.Date("2018-06-18"), xmax = as.Date("2018-06-26"),
         ymin = -Inf, ymax = Inf, fill= "#E5AE86", alpha = .7)+
  annotate("text", x = as.Date("2018-06-22"),
           y = 28.25, color= "black", label = "<30 m", size = 3)+
  annotate("text", x = as.Date("2018-08-10"),
           y = 28.9, color= "black", label = "2018",fontface=2, size = 4)+
  annotate("text", x = as.Date("2018-06-10"),
           y = 28.9, color= "black", label = "c",fontface=2, size = 4)+
  theme(axis.title.y=element_blank())
avg_temp2018

avg_temp2019 <-  avg_temp  +
  annotate("rect", xmin = as.Date("2019-06-18"), xmax = as.Date("2019-06-25"),
           ymin = -Inf, ymax = Inf, fill= "#E5AE86", alpha = .7)+
  annotate("rect", xmin = as.Date("2019-07-10"), xmax = as.Date("2019-07-16"),
           ymin = -Inf, ymax = Inf, fill= "#54278f", alpha = .6)+
  annotate("text", x = as.Date("2019-06-21"),
           y = 28.25, color= "black", label = "<10 m", size = 3)+
  annotate("text", x = as.Date("2019-07-13"),
           y = 28.25, color= "black", label = "<45 m", size = 3)+
  annotate("text", x = as.Date("2019-08-10"),
           y = 28.9, color= "black", label = "2019",fontface=2, size = 4)+
  annotate("text", x = as.Date("2019-06-10"),
           y = 28.9, color= "black", label = "d",fontface=2, size = 4)+
  theme(axis.title.y=element_blank())
avg_temp2019


avg_temp2020 <-  avg_temp  +  annotate("rect", xmin = as.Date("2020-06-24"), xmax = as.Date("2020-06-29"),
                                       ymin = -Inf, ymax = Inf, fill= "#E5AE86", alpha = .7)+
  annotate("rect", xmin = as.Date("2020-07-08"), xmax = as.Date("2020-07-13"),
           ymin = -Inf, ymax = Inf, fill= "#E5AE86", alpha = .7)+
  annotate("rect", xmin = as.Date("2020-07-23"), xmax = as.Date("2020-07-29"),
           ymin = -Inf, ymax = Inf, fill= "#778899", alpha = .6) +
  #annotate("rect", xmin = as.Date("2020-08-3"), xmax = as.Date("2020-08-09"),
   #        ymin = -Inf, ymax = Inf, fill= "#778899", alpha = .6) +
  annotate("text", x = as.Date("2020-06-26"),
           y = 28.5, color= "black", label = "<8 m", size = 3)+
  annotate("text", x = as.Date("2020-07-10"),
           y = 28.5, color= "black", label = "<25 m", size = 3)+
  annotate("text", x = as.Date("2020-07-26"),
           y = 28.5, color= "black", label = "25-40 m", size = 3)+
  #annotate("text", x = as.Date("2020-08-06"),
   #        y = 28.5, color= "black", label = "35-45 m", size = 3)+
  annotate("text", x = as.Date("2020-08-10"),
           y = 28.9, color= "black", label = "2020",fontface=2, size = 4)+
  annotate("text", x = as.Date("2020-06-10"),
           y = 28.9, color= "black", label ="e",fontface=2, size = 4)+
  theme(axis.title.y=element_blank())+
 xlab("June                   July                 August")
avg_temp2020 

#combine everything to one big figure
figure_2 <- ggarrange(avg_temp2016+rremove("x.text")+rremove("xlab"),avg_temp2017+rremove("x.text")+rremove("xlab"),
                    avg_temp2018+rremove("x.text")+rremove("xlab"),avg_temp2019+rremove("x.text")+rremove("xlab"),avg_temp2020+rremove("x.text")+rremove("xlab"),
                    #                 labels = c("a", "b","c","d","e"),
                    ncol = 1, nrow = 5)
figure_2


ggsave("all reproduction in one_MARCH2021.tiff", plot = figure_2, width = 6, height = 7,dpi=600, 
       units = "in")
##keeping it simple, only temp 
ggsave("temp_2018_with_annotation_Feb2021.tiff", plot = avg_temp2018, width = 6, height = 1.75,dpi=600, 
       units = "in")
ggsave("temp_2016_with_annotation_Feb2021.tiff", plot = avg_temp2016, width = 6, height = 1.75,dpi=600, 
       units = "in")
ggsave("temp_2017_with_annotation_Feb2021.tiff", plot = avg_temp2017, width = 6, height = 1.75,dpi=600, 
       units = "in")
ggsave("temp_2019_with_annotation_Feb2021.tiff", plot = avg_temp2019,width = 6, height = 1.75,dpi=600, 
       units = "in")
ggsave("temp_2020_with_annotation_Feb2021.tiff", plot = avg_temp2020, width = 6, height = 1.75,dpi=600, 
       units = "in")

######Histogram of spawning - Fig 4.##############

#going to use a long data set with all the three years together
setwd("C:/Users/Ronen/Documents/PhD/Colaboration with Tom_rhytisma connection/Submission/Data/")

d <- read_csv('Data of s.brooding intensity_.csv')
head(df)
str(d)
df <- d[c(1:6)]
df$with <- as.numeric(df$brood)
df$without <- as.numeric(df$no_brood) 
df$total= df$with+df$without
view(df)
  
sum <-df %>%
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(depth, year,event) %>%
  summarize(mean_int= 100*(mean(intensity, na.rm = TRUE))
            , sd= 100*(sd(intensity,na.rm=TRUE)), n= n(), se= sqrt(sd)/n, 
            intensity_max= 100*(max(intensity)),
            intensity_min=100*(min(intensity)), sum_with=sum(with), sum_total=sum(total),
            intensity_total=100*(sum_with/sum_total)) 
# purrr::modify_at(c("depth", "year","event"), factor) %>% 

sum$depth <- as.factor(sum$depth)
sum$year <- as.factor(sum$year)
sum$event <- as.character(sum$event)
View(sum)

#write.csv(file="sum of spawning events.csv", sum)
#sum <- read.csv("sum of spawning events.csv")

#set the names of the lables in x axis
mydepths <- c('0-10','10-20','20-30','30-40', '40-45')
#mycolo <- c("#E5AE86", "grey75","#8c6bb1")
mycolo <- c("#2c7fb8","#41b6c4", "#bdc9e1")

#sum$event <- revalue(sum$event, c("A"="1","B"= "2","C"= "3"))
#sum$depth <- revalue(sum$depth, c("1"="0-10","2"= "10-20","3"= "20-30", "4"= "30-40", "5"= "40-45"))
#sum$event <- ordered(sum$event, levels = c(A="1",B= "2",C= "3"))
str(sum)
view(sum)
p <- ggplot(sum, aes(x = depth,y = intensity_total,label= sum_total))+
  geom_bar(aes( fill=event), stat='identity', position = position_stack(reverse = TRUE), colour= 'black',width=0.95)+
  #  geom_errorbar(aes(ymin = mean_int-se, ymax= mean_int+se),  
  #               width=.2,                    # Width of the error bars
  #              position=position_dodge(.9),stat = "identity")+
  facet_wrap(.~year)+
  # geom_text(aes(label= sum_total ,x = depth), position = position_stack(vjust = .3))+
  geom_bar_text(position = 'identity',color="black", contrast = FALSE,size=8) +
  #put n exactly on each bar
  scale_fill_manual(values = mycolo, aes(alpha=0.09))+
  ylab("Surface-brooding colonies (%)")+xlab("Depth (m)")+
  theme_transparent() +
  theme(legend.position="none",
        axis.line = element_line(colour = "black"), panel.border = element_rect(fill = NA, color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        axis.title =element_text(size=14),
        axis.text=element_text(color="black", size=12),
        axis.text.x = element_text(color="black", size=12))+
  scale_y_continuous(limits = c(0, 55),expand = expansion(mult = c(0, 0.3)))+
  scale_x_discrete(labels = mydepths)
p
p+ facet_wrap(.~year,scales = 'free')

pp <- p+ geom_text(x = 5, y = 60, aes(label = year),fontface=2, size=4, data = sum)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
library(egg)
pp <- tag_facet(pp, open = "", close="",fontface=2, size=4)
pp
ggsave("Figure 3.Tiff", plot = pp, width = 6, height = 3.5,dpi=600, 
       units = "in")

#Figure 5
############range and wind ##########

setwd("C:/Users/Ronen/Documents/PhD/Colaboration with Tom_rhytisma connection/Submission/Data/")

data <- read.csv("Data of raw temperature at 6 depth zones.csv")

head(data)
#arrange data in right format
data$date <- as.Date( data$date,format="%m/%d/%Y")
str(data)
data$year <-  format(as.Date(data$date),format="%Y")
data <- data[c(1,2,7,3:6)]

#name it columns
colnames(data) <- c("Date", "mean_wind", "year", "5m", "15m", "30m", "45m")
data$year <- as.factor(data$year)

long_data <- data %>% pivot_longer(4:7, names_to = "depth", values_to = "difference")
View(long_data)
long_data$depth <- ordered(as.factor(long_data$depth), levels = c("5m", "15m", "30m", "45m"))

dat <- na.omit(long_data)
#install.packages("wesanderson")
#library(wesanderson)

windtemp_bydepth <- ggplot(dat, aes(x=mean_wind, y = difference)) + geom_point(aes(col = year),size=1) + 
  #scale_color_manual(values = wes_palette("GrandBudapest1"))+
  #scale_color_brewer(palette = "Dark2")+
  scale_color_nejm()+
  stat_smooth(method="lm", aes(col = year), se = F) + stat_smooth(method="lm", col = "black") + 
  xlab("Wind speed (m/s)") + ylab("Δdaily temperature (°C)") +
  facet_grid(~ depth) + 
  scale_y_continuous(labels=c(seq(-1,1,0.5)), breaks = c(seq(-1,1,0.5))) + theme_bw() +
  #theme(axis.title = element_text(size = 20,face="bold"),
        #axis.text=element_text(size = 20, color = "black"),
        #strip.text.x = element_text(size=18, face="bold"),
        #strip.background = element_rect(color = "white", fill="white"),
        #legend.position="top",
       # panel.spacing = unit(0, "lines"),
      #  panel.grid.major=element_blank(),
     #   panel.grid.minor=element_blank(),
    #    axis.line = element_line(colour = "black"),
   #     panel.background = element_blank())
theme(legend.position="bottom",legend.text=element_text(size=10),
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      strip.background = element_rect(color = "white", fill="white"),
      strip.text.x = element_text(size=12,color="black", face="bold"),
      axis.title =element_text(size=12),
      axis.text=element_text(color="black", size=12),
      axis.text.x = element_text(color="black", size=12))

windtemp_bydepth
library(ggplot2)

g <- windtemp_bydepth + guides(color = guide_legend(override.aes = list(size = 1.5)))
g
ggsave("Figure 5.tiff", plot = g , width = 6, height = 3.5,dpi=600, 
       units = "in")
ggsave("Figure 5_no_legend.tiff", plot = windtemp_bydepth , width = 18, height = 12,dpi=600, 
       units = "cm")

cor_all

###Appendices figures ######################
setwd("C:/Users/Ronen/Documents/PhD/Colaboration with Tom_rhytisma connection/usful datasets")
df <- read.csv("Data of raw temperature at 4 depth zones.csv")
#df <- All_temp_with_depth

View(df)

#str(d)
df <- df[c(1,3,4)]

df$date <- as.Date( df$date,format="%d/%m/%Y")
#View(df)

#choosing colors for the temperaute line in the plots 
mycolo <- c('#d7191c','#2ca25f','#2b83ba','#252525')
mylabes <- c( "5 m", "15 m", "30 m" , "45 m")
#filter the dates i want AND
# summerizing the data to get average and other stuff
t2016_mean <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2016-01-01")& date <= as.Date("2016-12-31")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(date, depth) %>%  
  summarize(mean_temp= mean(temp)
            , sd= sd(temp,na.rm=TRUE),
            daily_max= max(temp),
            daily_min=min(temp)) %>% 
  ungroup() 


View(t2016_mean)
#making the plot - chage accoriding to the year 
avg_temp_2016<- ggplot( t2016_mean, aes(date, mean_temp, group=depth))+
  geom_line(aes(color=depth), size=0.7) +
  geom_point(aes(color=depth), size=0.6)+ 
  #  geom_smooth(aes(color=depth))+
  scale_color_manual(values = mycolo, labels= mylabes) +
  #  scale_color_npg()+
  #    scale_x_datetime(date_breaks = "1 month",labels = date_format("%m"))+
  #xlab("June                                            July")+
  ylab("")+xlab("")+
  scale_x_date(breaks = seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by="1 month")
               , date_labels = "%b")+  theme_bw() +
  scale_y_continuous(limits  = c(21, 29.5), breaks = seq(21, 29, by = 2))+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=12),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))
#+
#  annotate("text", x = as.Date("2016-01-01"),
#          y = 28.9, color= "black", label = "a",fontface=2, size = 4)+
#geom_hline(yintercept=26, linetype="dashed", 
#geom_hline(yintercept=26, linetype="dashed", 
#          color = "black", size=0.3)
avg_temp_2016

avg_temp_2016 <- avg_temp_2016+
  annotate("text", x = as.Date("2016-12-31"), y = 28.9, color= "black", label = "2016",fontface=2, size = 4)+
  annotate("text", x = as.Date("2016-01-01"),y = 28.9, color= "black", label = "a",fontface=2, size = 4)

##############################################
t2017_mean <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2017-01-01")& date <= as.Date("2017-12-31")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(date, depth) %>%  
  summarize(mean_temp= mean(temp)
            , sd= sd(temp,na.rm=TRUE),
            daily_max= max(temp),
            daily_min=min(temp)) %>% 
  ungroup()

#View(t2017_mean)
#making the plot - chage accoriding to the year 
avg_temp_2017<- ggplot( t2017_mean, aes(date, mean_temp, group=depth))+
  geom_line(aes(color=depth), size=0.7) +
  geom_point(aes(color=depth), size=0.6)+ 
  #  geom_smooth(aes(color=depth))+
  scale_color_manual(values = mycolo, labels= mylabes) +
  #  scale_color_npg()+
  #    scale_x_datetime(date_breaks = "1 month",labels = date_format("%m"))+
  #xlab("June                                            July")+
  ylab("")+xlab("")+
  scale_x_date(breaks = seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by="1 month")
               , date_labels = "%b")+  theme_bw() +
  scale_y_continuous(limits  = c(21, 29.5), breaks = seq(21, 29, by = 2))+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=12),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))

avg_temp_2017 <- avg_temp_2017+
  annotate("text", x = as.Date("2017-12-31"), y = 28.9, color= "black", label = "2017",fontface=2, size = 4)+  annotate("text", x = as.Date("2017-01-01"),y = 28.9, color= "black", label = "b",fontface=2, size = 4)
avg_temp_2017


##############################################
t2018_mean <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2018-01-01")& date <= as.Date("2018-12-31")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(date, depth) %>%  
  summarize(mean_temp= mean(temp)
            , sd= sd(temp,na.rm=TRUE),
            daily_max= max(temp),
            daily_min=min(temp)) %>% 
  ungroup() 

#View(t2017_mean)
#making the plot - chage accoriding to the year 
avg_temp_2018<- ggplot( t2018_mean, aes(date, mean_temp, group=depth))+
  geom_line(aes(color=depth), size=0.7) +
  geom_point(aes(color=depth), size=0.6)+ 
  #  geom_smooth(aes(color=depth))+
  scale_color_manual(values = mycolo, labels= mylabes) +
  #  scale_color_npg()+
  #    scale_x_datetime(date_breaks = "1 month",labels = date_format("%m"))+
  #xlab("June                                            July")+
  ylab("")+xlab("")+
  scale_x_date(breaks = seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="1 month")
               , date_labels = "%b")+  theme_bw() +
  scale_y_continuous(limits  = c(21, 29.5), breaks = seq(21, 29, by = 2))+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=12),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))

avg_temp_2018 <- avg_temp_2018+
  annotate("text", x = as.Date("2018-12-31"), y = 28.9, color= "black", label = "2018",fontface=2, size = 4)+  annotate("text", x = as.Date("2018-01-01"),y = 28.9, color= "black", label = "c",fontface=2, size = 4)
avg_temp_2018

##############################################
t2019_mean <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2019-01-01")& date <= as.Date("2019-12-31")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(date, depth) %>%  
  summarize(mean_temp= mean(temp)
            , sd= sd(temp,na.rm=TRUE),
            daily_max= max(temp),
            daily_min=min(temp)) %>% 
  ungroup() 

#View(t2017_mean)
#making the plot - chage accoriding to the year 
avg_temp_2019<- ggplot( t2019_mean, aes(date, mean_temp, group=depth))+
  geom_line(aes(color=depth), size=0.7) +
  geom_point(aes(color=depth), size=0.6)+ 
  #  geom_smooth(aes(color=depth))+
  scale_color_manual(values = mycolo, labels= mylabes) +
  #  scale_color_npg()+
  #    scale_x_datetime(date_breaks = "1 month",labels = date_format("%m"))+
  #xlab("June                                            July")+
  ylab("")+xlab("")+
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="1 month")
               , date_labels = "%b")+  theme_bw() +
  scale_y_continuous(limits  = c(21, 29.5), breaks = seq(21, 29, by = 2))+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=12),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))

avg_temp_2019 <- avg_temp_2019+
  annotate("text", x = as.Date("2019-12-31"), y = 28.9, color= "black", label = "2019",fontface=2, size = 4)+  annotate("text", x = as.Date("2019-01-01"),y = 28.9, color= "black", label = "d",fontface=2, size = 4)
avg_temp_2019

##############################################
t2020_mean <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2020-01-01")& date <= as.Date("2020-12-31")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(date, depth) %>%  
  summarize(mean_temp= mean(temp)
            , sd= sd(temp,na.rm=TRUE),
            daily_max= max(temp),
            daily_min=min(temp)) %>% 
  ungroup() 

#View(t2017_mean)
#making the plot - chage accoriding to the year 
avg_temp_2020<- ggplot( t2020_mean, aes(date, mean_temp, group=depth))+
  geom_line(aes(color=depth), size=0.7) +
  geom_point(aes(color=depth), size=0.6)+ 
  #  geom_smooth(aes(color=depth))+
  scale_color_manual(values = mycolo, labels= mylabes) +
  #  scale_color_npg()+
  #    scale_x_datetime(date_breaks = "1 month",labels = date_format("%m"))+
  #xlab("June                                            July")+
  ylab("")+xlab("")+
  scale_x_date(breaks = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="1 month")
               , date_labels = "%b")+  theme_bw() +
  scale_y_continuous(limits  = c(21, 29.5), breaks = seq(21, 29, by = 2))+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=12),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))

avg_temp_2020 <- avg_temp_2020+
  annotate("text", x = as.Date("2020-12-31"), y = 28.9, color= "black", label = "2020",fontface=2, size = 4)+annotate("text", x = as.Date("2020-01-01"),y = 28.9, color= "black", label = "e",fontface=2, size = 4)
avg_temp_2020

### merging all on the same plot
#install.packages("cowplot")
library(cowplot)

avg_temp__all <- plot_grid(avg_temp_2016+theme(axis.title = element_blank(),axis.text.x = element_blank()),avg_temp_2017+theme(axis.title = element_blank(),axis.text.x = element_blank())
                           ,avg_temp_2018+theme(axis.title = element_blank(),axis.text.x = element_blank()), avg_temp_2019+theme(axis.title = element_blank(),axis.text.x = element_blank()),
                           avg_temp_2020,ncol = 1,align = "v")
avg_temp__all

+theme(axis.title = element_blank())
ggsave(file="Fig S1.tiff",plot= avg_temp__all, width =20 , height = 20 ,dpi=600, 
       units = "cm") 
ggsave(file="Fig S1_with x axis.tiff",plot= avg_temp__all, width =20 , height = 20 ,dpi=600, 
       units = "cm") 
###############SOLAR irradiance################
####Figure S2

##solar irradiance
#get data
setwd("C:/Users/Ronen/Documents/PhD/Colaboration with Tom_rhytisma connection/usful datasets/")

d_all <- read_csv('metrological_5_year.csv')

#set date format
#d$datetime <- as.POSIXct(paste(d$Date, d$Time), format="%m/%d/%Y %H:%M:%S")
d_all$Date <- as.Date(d_all$Date, "%m/%d/%Y")

head(d_all)
view(d_all)
#filter data by year and plot each year seperately. sorry, my understanding in loops is quite small
d_2016 <- d_all %>% 
  #filter(Date >= as.Date("2019-06-01")& Date <= as.Date("2019-08-01"))
  filter(Date >= as.Date("2016-06-10")& Date <= as.Date("2016-08-10"))
head(d_2016)

t2016_mean <- d_all %>%
  filter(Date >= as.Date("2016-06-10")& Date <= as.Date("2016-08-10")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(Date) %>%
  summarize(mean_solar= mean(Solar)
            , sd= sd(Solar,na.rm=TRUE),
            daily_max= max(Solar,na.rm=TRUE),
            daily_min=min(Solar)) %>% 
  ungroup() 
head(t2016_mean)
md <- d_2016
md[md==0] <- NA
solar_2016 <- ggplot(data= d_2016, aes(Date, Solar))+
  geom_point( size=1,color="#bdc9e1")+
  geom_line(data=t2016_mean,aes(x=Date,y=mean_solar),size=1,color='red')+
  # geom_line(data=summer_2017_mean,aes(x=Date,y=daily_max_Solar),size=1,color='black')+
  #geom_smooth(colour="red",size=0.75)+
  xlab("June-August (2016)")+ylab("Solar radiation W/m²")+ 
  scale_x_date(breaks = seq(as.Date("2016-06-10"), as.Date("2016-08-10"), by="week")
               , date_labels = "%e")+theme_bw() +
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=10),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))

solar_2016

d_2017 <- d_all %>% 
  #filter(Date >= as.Date("2019-06-01")& Date <= as.Date("2019-08-01"))
  filter(Date >= as.Date("2017-06-10")& Date <= as.Date("2017-08-10"))


t2017_mean <- d_all %>%
  filter(Date >= as.Date("2017-06-10")& Date <= as.Date("2017-08-10")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(Date) %>%
  summarize(mean_solar= mean(Solar)
            , sd= sd(Solar,na.rm=TRUE),
            daily_max= max(Solar,na.rm=TRUE),
            daily_min=min(Solar)) %>% 
  ungroup() 
#head(t2016_mean)
md <- d_2017
md[md==0] <- NA
solar_2017 <- ggplot(data= d_2017, aes(Date, Solar))+
  geom_point( size=1,color="#bdc9e1")+
  geom_line(data=t2017_mean,aes(x=Date,y=mean_solar),size=1,color='red')+
  # geom_line(data=summer_2017_mean,aes(x=Date,y=daily_max_Solar),size=1,color='black')+
  #geom_smooth(colour="red",size=0.75)+
  xlab("June-August (2017)")+ylab("Solar radiation W/m²")+ 
  scale_x_date(breaks = seq(as.Date("2017-06-10"), as.Date("2017-08-10"), by="week")
               , date_labels = "%e")+theme_bw() +
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=10),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))

solar_2017

d_2018 <- d_all %>% 
  #filter(Date >= as.Date("2019-06-01")& Date <= as.Date("2019-08-01"))
  filter(Date >= as.Date("2018-06-10")& Date <= as.Date("2018-08-10"))
head(d_2016)

t2018_mean <- d_all %>%
  filter(Date >= as.Date("2018-06-10")& Date <= as.Date("2018-08-10")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(Date) %>%
  summarize(mean_solar= mean(Solar)
            , sd= sd(Solar,na.rm=TRUE),
            daily_max= max(Solar,na.rm=TRUE),
            daily_min=min(Solar)) %>% 
  ungroup() 
head(t2018_mean)
md <- d_2018
md[md==0] <- NA
solar_2018 <- ggplot(data= d_2018, aes(Date, Solar))+
  geom_point( size=1,color="#bdc9e1")+
  geom_line(data=t2018_mean,aes(x=Date,y=mean_solar),size=1,color='red')+
  # geom_line(data=summer_2017_mean,aes(x=Date,y=daily_max_Solar),size=1,color='black')+
  #geom_smooth(colour="red",size=0.75)+
  xlab("June-August (2018)")+ylab("Solar radiation W/m²")+ 
  scale_x_date(breaks = seq(as.Date("2018-06-10"), as.Date("2018-08-10"), by="week")
               , date_labels = "%e")+theme_bw() +
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=10),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))

solar_2018

d_2019 <- d_all %>% 
  #filter(Date >= as.Date("2019-06-01")& Date <= as.Date("2019-08-01"))
  filter(Date >= as.Date("2019-06-10")& Date <= as.Date("2019-08-10"))
head(d_2019)

t2019_mean <- d_all %>%
  filter(Date >= as.Date("2019-06-10")& Date <= as.Date("2019-08-10")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(Date) %>%
  summarize(mean_solar= mean(Solar)
            , sd= sd(Solar,na.rm=TRUE),
            daily_max= max(Solar,na.rm=TRUE),
            daily_min=min(Solar)) %>% 
  ungroup() 
head(t2019_mean)

md <- d_2019
md[md==0] <- NA
solar_2019 <- ggplot(data= d_2019, aes(Date, Solar))+
  geom_point( size=1,color="#bdc9e1")+
  geom_line(data=t2019_mean,aes(x=Date,y=mean_solar),size=1,color='red')+
  # geom_line(data=summer_2017_mean,aes(x=Date,y=daily_max_Solar),size=1,color='black')+
  #geom_smooth(colour="red",size=0.75)+
  xlab("June-August (2019)")+ylab("Solar radiation W/m²")+ 
  scale_x_date(breaks = seq(as.Date("2019-06-10"), as.Date("2019-08-10"), by="week")
               , date_labels = "%e")+theme_bw() +
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=10),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))

solar_2019

d_2020 <- d_all %>% 
  #filter(Date >= as.Date("2019-06-01")& Date <= as.Date("2019-08-01"))
  filter(Date >= as.Date("2020-06-10")& Date <= as.Date("2020-08-10"))
head(d_2020)

t2020_mean <- d_all %>%
  filter(Date >= as.Date("2020-06-10")& Date <= as.Date("2020-08-10")) %>%  
  #  mutate(floor_date(ymd('2016-08-26'), "hour"))
  group_by(Date) %>%
  summarize(mean_solar= mean(Solar)
            , sd= sd(Solar,na.rm=TRUE),
            daily_max= max(Solar,na.rm=TRUE),
            daily_min=min(Solar)) %>% 
  ungroup() 
head(t2020_mean)

md <- d_2020
md[md==0] <- NA
#view(md)
#solar_2020[d_2020$Solar == 0] <- NA

solar_2020 <- ggplot(data= d_2020, aes(Date, Solar))+
  geom_point( size=1,color="#bdc9e1")+
  geom_line(data=t2020_mean,aes(x=Date,y=mean_solar),size=1,color='red')+
  # geom_line(data=summer_2017_mean,aes(x=Date,y=daily_max_Solar),size=1,color='black')+
  #geom_smooth(colour="red",size=0.75)+
  xlab("June-August (2020)")+ylab("Solar radiation W/m²")+ 
  scale_x_date(breaks = seq(as.Date("2020-06-10"), as.Date("2020-08-10"), by="week")
               , date_labels = "%e")+theme_bw() +
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title =element_text(size=10),
        axis.text=element_text(color="black", size=10),
        axis.text.x = element_text(color="black", size=10))
solar_2020

### merging all on the same plot
solar_all <- plot_grid(solar_2016,solar_2017,solar_2018, solar_2019,solar_2020, labels = "auto", label_size = 12)
solar_all
ggsave(file="Figure S2.Solar_all.tiff",plot= solar_all, width =20 , height = 18 ,dpi=600, 
       units = "cm") 


#Figure S3 - pearson correlation of model parameters 

#load the df
setwd("C:/Users/Ronen/Documents/PhD/Colaboration with Tom_rhytisma connection/THE MODEL")
d <- read.csv("Full MODEL variables.csv")

head(d)

# Modify specified columns to a factor class
df<-d %>%
  # select(-X) %>% 
  purrr::modify_at(c("Jul", "depth", "brooding","lunar_Day"), factor) %>% 
  rename( daily.range=Var)

df$date <- as.Date(df$date,format="%m/%d/%Y")
df$year <- format(df$date, "%Y")

view(df)

#I have not collected data on 2020 s. brooding in 45 m. Therefore i need to
#discard the the NA's in 2020 in 45 m depth
data <- df %>% 
  dplyr::filter(year!=2020 | depth != 45)
head(data)

#select cont. variables 
continuous <-na.omit(select_if(data, is.numeric))
view(continuous)
###see which variables have a high correlation between them 

#head(continuous)#select only variables that we will want to use-  
continuous <- continuous %>% 
  dplyr::select(mean_temp ,daily.range ,daily_max,Delta.temp.1.day, Delta.temp.2.day,Delta.temp.3.day,five.day.temp.trend.Jul,lunar_Day_number)

#colinearity - MAX temp with daily mean and daily Var
#degree heating days with daily mean
# delta 1 temp and delta 2 temp

M=cor(continuous,method="p") #create Pearson correlation matrix
res1 <- cor.mtest(continuous, conf.level = .95)
res1
#Illustrait it 
corrplot(M, type = "upper", tl.pos = "td",
         method = "color", addCoef.col = "black",tl.cex = 0.75, tl.srt = 45,tl.col = 'black',
         order = "hclust", diag = FALSE)


#filter AND export the relevant Summer dates
censored <- df %>%
  mutate(depth = as.factor(depth)) %>% 
  filter(date >= as.Date("2016-06-10")& date <= as.Date("2016-08-10") |
 date >= as.Date("2017-06-10")& date <= as.Date("2017-08-10")|
  date >= as.Date("2018-06-10")& date <= as.Date("2018-08-10")|
  date >= as.Date("2019-06-10")& date <= as.Date("2019-08-10")|
   date >= as.Date("2020-06-10")& date <= as.Date("2020-08-10")) %>% 
  arrange(date)
View(censored) 
levels(censored$depth)

#export 
write.csv(censored, file = "only relevent data.csv")

t2017_mean$depth <- ordered(t2017_mean$depth, levels = c( "5", "15", "30" , "45"))
#######################################################

