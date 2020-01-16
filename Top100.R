library(tidyjson)
library(tidyverse)
library(ggplot2)
setwd("C:/Users/Daniel/OneDrive - The University of Western Ontario/Projects/Spotify Top Songs 2019")


#Turn JSON into dataframe
#Audio Features of canada top 100 songs
canada100 <- as.data.frame(read_json("canadaTop100Features.json") %>% 
  enter_object("audio_features") %>% 
  gather_array %>% 
  spread_all)

#Removing irrelevant columns
#document.id, type, id, track_href, analysis_url 
canada100 <- canada100[,-c(1,14,15,17,18)]  

#Audio Features of my top 100 songs
my100 <- as.data.frame(read_json("MyTop100Features.json") %>% 
                             enter_object("audio_features") %>% 
                             gather_array %>% 
                             spread_all)

#Removing irrelevant columns
#document.id, type, id, track_href, analysis_url 
my100 <- my100[,-c(1,14,15,17,18)] 

#Information about canada top 100 songs (minus artists, since artist name is nested deeper
#in the JSON file 
canada100_info <- as.data.frame(read_json("canadaTop100Info.json") %>%
  enter_object("tracks") %>%
  gather_array %>% 
  spread_all)

my100_info <- as.data.frame(read_json("MyTop100Info.json") %>%
  enter_object("tracks") %>%
  gather_array %>% 
  spread_all)

#extracting artist names associated with canada/my 100 songs
#Note: Some songs have multiple artists, so this can't be 
#added directly to a data frame since rows > 100
canada100_artists <- as.data.frame(read_json("canadaTop100Info.json") %>% 
                                  enter_object("tracks") %>% 
                                  gather_array(column.name = "rank") %>% #this column preserves which of the 100 songs these 158 artists are associated with
                                  enter_object("artists") %>% 
                                  gather_array %>% 
                                  spread_all)

canada100_artists <- canada100_artists[,c(2,6,8)]     


my100_artists <- as.data.frame(read_json("MyTop100Info.json") %>% 
                                  enter_object("tracks") %>% 
                                  gather_array(column.name = "rank") %>% 
                                  enter_object("artists") %>% 
                                  gather_array %>% 
                                  spread_all)

my100_artists <- my100_artists[,c(2,6,8)]      
              
              


#adding song names to my top 100 and canada 100
my100 <- cbind(my100,my100_info[,c(5,9,10,12,13,15,18:21,23)])

canada100 <- cbind(canada100,canada100_info[,c(5,9,10,12,13,15,18:21,23)])

ggplot() +
  geom_bar(data=canada100, aes(x = popularity, y = ..count..,fill="black"), alpha = 0.8) +
  geom_bar(data=my100, aes(x = popularity, y = ..count.., fill="#1ed761"), alpha=0.8) + 
  labs(title = "Popularity of My Top 100 vs canada Top 100 Spotify Songs", y = "# of Songs") +
  scale_fill_identity(name = 'Songs', guide = 'legend',labels = c('My 100',"Canada's 100"))


#Formatting release dates into year-month
myyear_month <- format(as.Date(my100$album.release_date),format="%Y-%m")
myyear_month <- paste0(myyear_month,"-1")
myyear_month <- as.Date(myyear_month, format="%Y-%m-%d")

canadayear_month <- format(as.Date(canada100[-96,]$album.release_date),format="%Y-%m") #96 = bohemian rhapsody, released in 1975
canadayear_month <- paste0(canadayear_month,"-1")
canadayear_month <- as.Date(canadayear_month, format="%Y-%m-%d")


ggplot(my100, aes(myyear_month)) +
  geom_bar(fill="#1ed761",col="black") + scale_x_date(date_breaks="3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks=seq(0,9,1))

ggplot(canada100[-96,], aes(canadayear_month)) +
  geom_bar(fill="#1ed761",col="black") + scale_x_date(date_breaks="3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks=seq(0,14,1))
#OVERLAY THESE 2 PLOTS ^^^ ???

(my100_artists %>% group_by(name) %>% count(sort=T)%>% print(n=1000))[1:10,]
(canada100_artists %>% group_by(name) %>% count(sort=T)%>% print(n=1000))[1:10,]


intersect(my100$uri,canada100$uri)
intersect(my100$name,canada100$name)

ggplot()+geom_ribbon(data=ribbon,aes(ymin=min,ymax=max,x=x.ribbon,fill='lightgreen'))+
  geom_line(data=ribbon,aes(x=x.ribbon,y=avg,color='black'))+
  geom_line(data=data,aes(x=x,y=new.data,color='red'))+
  xlab('x')+ylab('density') + 
  scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
  scale_colour_manual(name = 'the colour', 
                      values =c('black'='black','red'='red'), labels = c('c2','c1'))

canada100 %>% select(name, album.release_date) %>% arrange(desc(album.release_date)) 
my100 %>% select(name, album.release_date) %>% arrange(desc(album.release_date)) 

#unlimited release dates
ggplot() +
  geom_bar(data=canada100[-96,], aes(canadayear_month,fill="black"), color = "black", alpha = 0.6)+
  geom_bar(data=my100, aes(myyear_month,fill="#1ed761"), color = "black", alpha = 0.6)+
  scale_x_date(date_breaks="3 months", date_labels = "%b %Y") + 
  scale_y_continuous(breaks=seq(0,14,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Release Date of My Top 100 vs canada Top 100 Spotify Songs", y = "# of Songs") +
  scale_fill_identity(name = 'Songs', guide = 'legend',labels = c('My 100',"Canada's 100"))

#2016+ release dates
ggplot() +
  geom_bar(data=canada100[-96,], aes(canadayear_month,fill="black"), color = "black", alpha = 0.6)+
  geom_bar(data=my100, aes(myyear_month,fill="#1ed761"), color = "black", alpha = 0.6)+
  scale_x_date(date_breaks="3 months", date_labels = "%b %Y") + 
  xlim(as.Date(c('1/1/2016', '1/1/2020'), format="%d/%m/%Y") )+
  scale_y_continuous(breaks=seq(0,14,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Release Date of My Top 100 vs canada Top 100 Spotify Songs", y = "# of Songs") +
  scale_fill_identity(name = 'Songs', guide = 'legend',labels = c('My 100',"Canada's 100"))

