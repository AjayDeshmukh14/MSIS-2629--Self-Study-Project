#libraries used
library(lubridate)
library(RColorBrewer)
library(tidyverse)

#import the annual tempratures for Mumbai and check
temp_mum <-read_csv("station.csv")
str(temp_mum)

#select the temprature and year columns and check summary
temp_mum_yr <- select(temp_mum, YEAR, metANN)
temp_mum_yr <- rename(temp_mum_yr, ta= metANN)
summary(temp_mum_yr)

#Change the values of 999.9 and change format to ymd format
temp_mum_yr <- mutate(temp_mum_yr, ta= ifelse(ta==999.9, NA, ta))
temp_mum_yr <- mutate(temp_mum_yr, date=str_c(YEAR,"01-01",sep = "-")%>%ymd())


theme_strip <- theme_minimal()+
     theme(axis.text.y = element_blank(),
           axis.line.y = element_blank(),
           axis.title = element_blank(),
           panel.grid.major=element_blank(),
           legend.title = element_blank(),
           axis.text.x=element_text(vjust=3),
           panel.grid.minor=element_blank(),
           plot.title=element_text(size=14,face="bold")
     )

col_strip <- brewer.pal(11, "RdBu")

#plot the warming strips
ggplot(temp_mum_yr,
        aes(x=date,y=1,fill=ta))+
     geom_tile()+
     scale_x_date(date_breaks = "6 years",
                  date_labels = "%Y",
                  expand=c(0,0))+
     scale_y_continuous(expand=c(0,0))+
     scale_fill_gradientn(colors=rev(col_strip))+
     guides(fill=guide_colorbar(barwidth = 1))+
     labs(title="Mumbai 1880-2018",
          caption="Datos: GISS Surface Temperature Analysis")+
     theme_strip 

#https://dominicroye.github.io/en/2018/how-to-create-warming-stripes-in-r/