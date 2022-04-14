---
output: 
  pdf_document:
    keep_md: yes
    keep_tex: yes
    fig_caption: yes
    number_sections: yes
geometry: margin=2.54cm
title: "Precipitation Change in Beaufort, NC"
subtitle: https://github.com/tarokatayama/KatayamaNgenziThornton_ENV872_EDA_FinalProject.git
author: "Karen Thornton, Lambert Ngenzi, Taro Katayama"
fontsize: 12pt
mainfont: Times New Roman
---

\newpage
\tableofcontents 
\newpage
\listoftables 
\newpage
\listoffigures 
\newpage

# Rationale and Research Questions

**Question 1:** Has there been a significant increase in precipitation in Beaufort, NC from 1980 to 2016? 

**Question 2:** Has there been a significant increase in 1-year/24h precipitation event in Beaufort, NC from decade to decade (1997 to 2006 and 2007 to 2016)? 

**Null Hypothesis 1:** There is no significant change in precipitation from 1980 to 2016.

**Null Hypothesis 2:** There is no significant change in 1-year/24h precipitation events from decade to decade (1997 to 2006 and 2007 to 2016). 

\newpage

# Dataset Information
Data was originally attained online from DAYMET for a Hydrology assignment during Fall Semester. Since then, DAYMET has been taken down, so the excel sheet from class was used. The data shows historical precipitation data from Beaufort, NC for the years 1980 to 2016. This dataset was chosen because it is the most complete and consistent precipitation dataset that we could find. 

"Significant 1-year/24hr Precipitation Events" are considered 1 year/24h events using the NOAA threshold of 3.66 inches for Beaufort, NC. This means that in 24h the probability of it raining 3.66 inches or more should only happen once a year. These events are considered large storm events.

Get the working directory

```r
# Get your working directory
getwd()
```


```r
# Load your datasets
Beaufort_RAW<-read.csv("./Data/Raw/Beaufort_precip_1980-present_HUC_030203010503_dayMet_split-dates-columns.csv", stringsAsFactors = TRUE)
```

Load all packages

```r
# Load your packages
#install.packages(webshot)
library(webshot)
library(tidyverse)
library(dplyr)
library(lubridate)
#library(mapview)
#mapviewOptions(fgb = FALSE)
#library(sf)
library(knitr)
library(Kendall)
library(zoo)
library(gridExtra)
options(scipen = 4) # limit number of digits 
```

\newpage

# Exploratory Analysis

**Initial Wrangling**

```r
#Structure of the dataset
str(Beaufort_RAW)
```

```
## 'data.frame':	13514 obs. of  5 variables:
##  $ Date                                         : Factor w/ 13514 levels "1980-01-01","1980-01-02",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Area.Weighted.Mean.Precipitation..mm.per.day.: num  0 0 0 12 9 0 0 7 9 0 ...
##  $ year                                         : int  1980 1980 1980 1980 1980 1980 1980 1980 1980 1980 ...
##  $ month                                        : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ day_of_month                                 : int  1 2 3 4 5 6 7 8 9 10 ...
```

```r
#Head of the dataset
head(Beaufort_RAW)
```

```
##         Date Area.Weighted.Mean.Precipitation..mm.per.day. year month
## 1 1980-01-01                                             0 1980     1
## 2 1980-01-02                                             0 1980     1
## 3 1980-01-03                                             0 1980     1
## 4 1980-01-04                                            12 1980     1
## 5 1980-01-05                                             9 1980     1
## 6 1980-01-06                                             0 1980     1
##   day_of_month
## 1            1
## 2            2
## 3            3
## 4            4
## 5            5
## 6            6
```

```r
#Dimension of dataset
dim(Beaufort_RAW)
```

```
## [1] 13514     5
```

```r
#Class of the dataset
class(Beaufort_RAW)
```

```
## [1] "data.frame"
```

```r
# Column names of the dataset
colnames(Beaufort_RAW)
```

```
## [1] "Date"                                         
## [2] "Area.Weighted.Mean.Precipitation..mm.per.day."
## [3] "year"                                         
## [4] "month"                                        
## [5] "day_of_month"
```

```r
#summarize the data
summary(Beaufort_RAW)
```

```
##          Date       Area.Weighted.Mean.Precipitation..mm.per.day.
##  1980-01-01:    1   Min.   :  0.00                               
##  1980-01-02:    1   1st Qu.:  0.00                               
##  1980-01-03:    1   Median :  0.00                               
##  1980-01-04:    1   Mean   :  4.14                               
##  1980-01-05:    1   3rd Qu.:  2.00                               
##  1980-01-06:    1   Max.   :195.00                               
##  (Other)   :13508   NA's   :9                                    
##       year          month         day_of_month  
##  Min.   :1980   Min.   : 1.000   Min.   : 1.00  
##  1st Qu.:1989   1st Qu.: 4.000   1st Qu.: 8.00  
##  Median :1998   Median : 7.000   Median :16.00  
##  Mean   :1998   Mean   : 6.522   Mean   :15.73  
##  3rd Qu.:2007   3rd Qu.:10.000   3rd Qu.:23.00  
##  Max.   :2016   Max.   :12.000   Max.   :31.00  
## 
```

```r
# Rename columns to logical names
names(Beaufort_RAW)[1] <- "Date"
names(Beaufort_RAW)[2] <- "Mean_Precip_mm"

#Set Date as a Date
Beaufort_RAW$Date<-as.Date(Beaufort_RAW$Date, "%Y-%m-%d") 

# Set your ggplot theme
Theme <- theme_classic(base_size = 9) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "right")
theme_set(Theme)

#Save processed Raw to processed folder
write.csv(Beaufort_RAW, row.names = FALSE, 
          file = "./Data/Processed/Beaufort_Processed.csv")

#Read in processed data
Beaufort_Processed<-read.csv("./Data/Processed/Beaufort_Processed.csv")
```

**Create a Map of Beaufort for Visualization Aid**

```r
#Create a Beaufort point using long and lat 
#mapviewOptions(fgb=FALSE)

#Beaufort_pt <-  st_point(c(-76.6515, 34.7192))

#Convert the the two points into spatial data
#Beaufort_Pol <- st_sfc(Beaufort_pt, crs = 4326)

#view the map
#mapview(Beaufort_Pol)

#repeat the steps for the next layer
#Beaufort_Town <- st_as_sf(Beaufort_Pol)

#mapview(Beaufort_Town, col.regions = "red")

# Load NC counties info 
#NC_data <- st_read("./Data/Spatial/cb_2018_us_county_20m.shp") %>% 
 # filter(STATEFP == 37)

# Visualize MC data
#mapview(NC_data)

# Filter Beaufort county and map it 
#Beaufort_County <- NC_data %>% 
  #filter(NAME %in% "Carteret")

#mapview(Beaufort_County, col.regions = "orange")

# Combine all map 
#mapview(NC_data, alpha.regions = 0.2) +
  #mapview(Beaufort_County, col.regions = "orange") +
  #mapview(Beaufort_Town, col.regions = "red")

#plot in ggplot to put in presentation 
#ggplot()+
  #geom_sf(data = NC_data, aes(), alpha=0.2)+
  #geom_sf(data = Beaufort_County, fill= "orange", alpha = 0.7)+
  #geom_sf(data= Beaufort_Town, col = "red")+
  #ggtitle("Beaufort, NC")
```

# Wrangle Data for Analysis

```r
#Create a monthly mean precipitation and total monthly precipitation dataset for Beaufort. This dataset is only for visualization purposes. 
Beaufort_Clean<- Beaufort_Processed%>%
  group_by(year,month)%>%
   summarise(meanmonthlyprecip= mean(Mean_Precip_mm),
             sumMonthlyPrecip= sum(Mean_Precip_mm))%>%
  mutate(Date= my(paste0(month,"-", year)))

#Plot total monthly precipitation data to see rough trend (Clean dataset)
ggplot(Beaufort_Clean, aes(x=Date, y=sumMonthlyPrecip))+
  geom_line()+
  labs(y= "Sum Monthly Precip. (mm)", caption = "Total Monthly Precipitation")+
  geom_smooth(method = lm) 
```

![Total Monthly Precipitation](Final_Project_Thornton_Katayama_Ngenzi_files/figure-latex/clean-1.pdf) 

**Create a dataset for the early decade (1997-2006)**

```r
#This dataset has a 10 year time frame with precipitation in inches (1997-01-01 to 2006-12-31) and significant 1-year/24hr precipitation events in magnitude and number of significant precipitation events

Beaufort_early<- Beaufort_Processed%>%
  mutate(PrecipInches= Mean_Precip_mm*0.0394)%>%
  filter(Date >("1996-12-31"), Date < ("2007-01-01")) %>% 
  mutate(sigPrecip= ifelse(PrecipInches>3.66,PrecipInches,0),
         NumSigPrecip= ifelse(PrecipInches>3.66, 1,0))%>%
  select(Date , year, month, 
         day_of_month, PrecipInches, sigPrecip, NumSigPrecip)%>%
  drop_na()
  
#Create a Beaufort early dataset, where non-significant 1-year/24h precipitation events were classified as NA.
Beaufort_earlyNo0Precip<-Beaufort_Processed%>%
  mutate(PrecipInches=Mean_Precip_mm*0.0394)%>%
  filter(Date >("1996-12-31"), Date < ("2007-01-01")) %>% 
  mutate(sigPrecip= ifelse(PrecipInches>3.66,PrecipInches,NA),
         NumSigPrecip= ifelse(PrecipInches>3.66, 1,0))%>%
  select(Date , year, month, 
         day_of_month, PrecipInches, sigPrecip, NumSigPrecip)%>%
  drop_na()

#Summary of number of 1-year/24hr events per year
Beaufort_early_summary<- Beaufort_early%>%
  group_by(year)%>%
  summarise(SigPrecipEvents= sum(NumSigPrecip))

#Create a table of number of 1-year/24hr events per year.
EarlyTable<- kable(Beaufort_early_summary, 
                   caption = "1-year/24hr Events Over Year", 
                   col.names = c("Year", "1-year/24hr event"))
EarlyTable
```



Table: 1-year/24hr Events Over Year

| Year| 1-year/24hr event|
|----:|-----------------:|
| 1997|                 0|
| 1998|                 1|
| 1999|                 1|
| 2000|                 0|
| 2001|                 0|
| 2002|                 0|
| 2003|                 0|
| 2004|                 0|
| 2005|                 1|
| 2006|                 1|


```r
#Create a figure with number and magnitude of significant events per year.
Plot_early_sig <- ggplot(Beaufort_earlyNo0Precip, 
                         aes(x=Date , y=sigPrecip))+
  geom_point()+
  ylim(c(0,8))+
  labs(y="1-year/24hr storm events (in)", x= "Date", 
       caption = "1-year/24hr storm events (in)") +
  ggtitle("Early Signficant Events")
Plot_early_sig
```

![1 year 24 hr storm events (early decade)](Final_Project_Thornton_Katayama_Ngenzi_files/figure-latex/early plot 1-1.pdf) 


```r
#Create a figure showing the overall precipitation for the early decade
Plot_early_overall <- ggplot(Beaufort_early, 
                             aes(x=Date , y=PrecipInches))+
  geom_point()+
  labs(y="Precipitation (in)", x= "Date")
Plot_early_overall
```

![Overall Precipitation (early decade)](Final_Project_Thornton_Katayama_Ngenzi_files/figure-latex/early plot 2-1.pdf) 

**Create a dataset for the late decade (2007-2016)**

```r
#This dataset has a 10 year time frame, precipitation in inches (2007-01-01 to 2016-12-30) and significant 1-year/24hr precipitation events in magnitude and number of significant precipitation events

Beaufort_Late<- Beaufort_Processed%>%
  mutate(PrecipInches= Mean_Precip_mm*0.0394)%>%
  filter(Date > "2006-12-31")%>%
  mutate(sigPrecip= ifelse(PrecipInches>3.66,PrecipInches,0),
         NumSigPrecip= ifelse(PrecipInches>3.66, 1,0))%>%
  select(Date, year, month, 
         day_of_month, PrecipInches, sigPrecip, NumSigPrecip)%>%
  drop_na()

#Create a Beaufort late dataset, where non significant 1-year/24hr precipitation events were classified as NA.
Beaufort_LateNo0Precip<- Beaufort_Processed%>%
  mutate(PrecipInches= Mean_Precip_mm*0.0394)%>%
  filter(Date > "2006-12-31")%>%
  mutate(sigPrecip= ifelse(PrecipInches>3.66,PrecipInches,NA),
         NumSigPrecip= ifelse(PrecipInches>3.66, 1,0))%>%
  select(Date, year, month, 
         day_of_month, PrecipInches, sigPrecip, NumSigPrecip)%>%
  drop_na()

#Summary of number of significant 1-year.24hr events per year (Late)
Beaufort_late_summary<- Beaufort_Late%>%
  group_by(year)%>%
  summarise(SigPrecipEvents= sum(NumSigPrecip))
```


```r
#Create a figure with number and magnitude of significant 1-year/24hr events per year.
Plot_late_sig <- ggplot(Beaufort_LateNo0Precip, 
                        aes(x=Date , y=sigPrecip))+
  geom_point()+
  ylim(c(0,8))+
  labs(y="1-year/24h storm events (in) ", x= "Date") +
  ggtitle("Late Signficant Events")
Plot_late_sig
```

![1 year 24 hr storm events (late decade)](Final_Project_Thornton_Katayama_Ngenzi_files/figure-latex/late plot 1-1.pdf) 


```r
#Create a figure showing the overall precipitation for the late decade
Plot_late_overall <- ggplot(Beaufort_Late, aes(x=Date , y=PrecipInches))+
  geom_point()+
  labs(y="Precipitation (in)", x= "Date")
Plot_late_overall
```

![Overall Precipitation (late decade)](Final_Project_Thornton_Katayama_Ngenzi_files/figure-latex/late plot 2-1.pdf) 

```r
#Create a table of number of significant 1-year/24hr events per year.
LateTable<- kable(Beaufort_late_summary, 
                  caption = "Significant Events Over Year", 
                  col.names = c("Year", "1-year/24hr event"))
LateTable
```



Table: Significant Events Over Year

| Year| 1-year/24hr event|
|----:|-----------------:|
| 2007|                 2|
| 2008|                 1|
| 2009|                 2|
| 2010|                 2|
| 2011|                 2|
| 2012|                 0|
| 2013|                 1|
| 2014|                 2|
| 2015|                 3|
| 2016|                 2|

**Compare overall precipitation for each decade**

```r
grid.arrange(Plot_early_overall, Plot_late_overall, ncol=2)
```

\begin{figure}

{\centering \includegraphics{Final_Project_Thornton_Katayama_Ngenzi_files/figure-latex/overall grid-1} 

}

\caption{Overall Precipitation by decade}\label{fig:overall grid}
\end{figure}

**Compare the 1-year/24 hour events for each decade**

```r
grid.arrange(Plot_early_sig, Plot_late_sig, ncol=2)
```

\begin{figure}

{\centering \includegraphics{Final_Project_Thornton_Katayama_Ngenzi_files/figure-latex/combined one year storms-1} 

}

\caption{1 year 24 hr storm event decade comparison}\label{fig:combined one year storms}
\end{figure}

\newpage 

# Analysis

Perform t-test and seasonal Mann-Kendall for overall dataset

```r
t.test(Beaufort_Processed$Mean_Precip_mm)
```

```
## 
## 	One Sample t-test
## 
## data:  Beaufort_Processed$Mean_Precip_mm
## t = 43.492, df = 13504, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  3.953508 4.326684
## sample estimates:
## mean of x 
##  4.140096
```

```r
#significant (p-value=2.2e-16) <- This means that there is a significant change in precipitation over the years 1980 to 2016

#Using Seasonal Mann-Kendall to look at trend excluding seasonality. This will be a better indicator of significant change than the t-test. That being said, both tests were run.
Beaufort_RAW_2<-Beaufort_Processed

#Used linear interpolation to fill missing data for precipitation data.
Beaufort_RAW_2$Mean_Precip_mm<-
  na.approx(Beaufort_RAW_2$Mean_Precip_mm)

#Created a time series analysis of the precipitation data at Beaufort.
firstday<- day(first(Beaufort_RAW_2$Date))
firstmonth<- month(first(Beaufort_RAW_2$Date))
firstyear<- year(first(Beaufort_RAW_2$Date))


Beaufort_TS<- ts(Beaufort_RAW_2$Mean_Precip_mm, 
                 start = c(firstyear, firstmonth, firstday),
                 frequency = 365)

#Decomposed the time series to see components.
Beaufort_decompose<- stl(Beaufort_TS, s.window = "periodic")
plot(Beaufort_decompose)
```

![Decomposed time series](Final_Project_Thornton_Katayama_Ngenzi_files/figure-latex/t-test Overall and Seasonal Mann-Kendall Ovearall-1.pdf) 

```r
#Ran a seasonal Mann Kendall to see if there is a change in precipitation over the time of the data frame. Used seasonal Mann-Kendall to exclude seasonality of precipitation. 
Beaufort.trend<-Kendall::SeasonalMannKendall(Beaufort_TS)
Beaufort.trend
```

```
## tau = 0.0189, 2-sided pvalue =0.0056612
```

```r
#Significant! (p-value = 0.0056612) <- This means that when you exclude seasonality there is still a significant change in precipitation from 1980 to 2016
```

**T-test was run to compare the two decades**

```r
#Here we are looking to see if there is a change in precipitation amount comparing two decades (1999-2006) & (2007-2016)
t.test(Beaufort_early$PrecipInches, Beaufort_Late$PrecipInches)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Beaufort_early$PrecipInches and Beaufort_Late$PrecipInches
## t = -0.64906, df = 7133.8, p-value = 0.5163
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.02812072  0.01413102
## sample estimates:
## mean of x mean of y 
## 0.1627598 0.1697546
```

```r
#not significant (p-value=0.5163)<- meaning precipitation amount not significantly different over the two decades

#Here we compared the significant 1-year/24hr precipitation events for the two decades (1999-2006) & (2007-2016).
t.test(Beaufort_early$sigPrecip, Beaufort_Late$sigPrecip)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Beaufort_early$sigPrecip and Beaufort_Late$sigPrecip
## t = -2.7068, df = 5451.7, p-value = 0.006815
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.028681849 -0.004586864
## sample estimates:
##   mean of x   mean of y 
## 0.005537589 0.022171945
```

```r
#significant! (p-value=0.006815) <- There are more statistically significant 1 year precipitation events in later decade. 
```


\newpage

# Summary and Conclusions

After careful analysis, it has been concluded that there is a significant increase in precipitation from 1980 to 2016 in Beaufort, NC. When comparing decade 1 (1997 to 2006) to decade 2 (2007 to 2016) there was no significant increase in precipitation.That being said, when looking at significant 1-year/24hr storm events with a threshold of 3.66 inches there was a significant increase from decade 1 to decade 2. 

While there isn't enough information to draw conclusions based on precipitation alone, our prediction is climate change as the driver behind the increase in 1-year/24hr storm events between decades. This project shows the importance of tracking large storm events in future climate change studies. 

It would also be interesting to use a longer period of data (more than 30 years) 
to see if there is any new or different trends that could not be captured with our data.

\newpage

# Resources

__NOAA Website__
https://hdsc.nws.noaa.gov/hdsc/pfds/pfds_map_cont.html?bkmrk=nc
