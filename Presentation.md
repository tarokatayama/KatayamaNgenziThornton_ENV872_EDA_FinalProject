Presentation
========================================================
author: Taro Katayama, Karen Thornton, Lambert Ngenzi
date: 04/11/22
autosize: true

Map of Beaufort
========================================================


```r
#insert map
```

Question and Hypothesis
========================================================
Question 1: Has there been a significant increase in precipitation in Beaufort, NC from 1980 to 2016? 

Question 2: Has there been a significant increase in 1-year precipitation event in Beaufort, NC from decade to decade (1997 to 2006 and 2007 to 2016)? 

Null Hypothesis 1: There is no significant change in precipitation from 1980 to 2016.

Null Hypothesis 2: There is no significant change in 1-year precipitation events from decade to decade (1997 to 2006 and 2007 to 2016). 

Data
========================================================
- Data from DAYMET, attained from Hydrology class 
- Historical precipitation data from Beaufort, NC


See column headers below

```
[1] "Date"                                         
[2] "Area.Weighted.Mean.Precipitation..mm.per.day."
[3] "year"                                         
[4] "month"                                        
[5] "day_of_month"                                 
```

Data Wrangling 
========================================================


- Grouped by month and year
- Got mean and sum of precipitation 

```r
Beaufort_Clean<- Beaufort_RAW%>%
  group_by(year,month)%>%
   summarise(meanmonthlyprecip= mean(Area.Weighted.Mean.Precipitation..mm.per.day.),
             sumMonthlyPrecip= sum(Area.Weighted.Mean.Precipitation..mm.per.day.))%>%
  mutate(Date= my(paste0(month,"-", year)))
```

Data Wrangling: Early decade
========================================================
- Convert from mm to inches
- Filtered the date to show first decade (1997 to 2007)
- Created column for "1-year precipitation events" (Threshold = 3.66 inches per day)

```r
Beaufort_early<- Beaufort_RAW%>%
  mutate(PrecipInches= Area.Weighted.Mean.Precipitation..mm.per.day.*0.0394)%>%
  filter(Date >("1996-12-31"), Date < ("2007-01-01")) %>% 
  mutate(sigPrecip= ifelse(PrecipInches>3.66,PrecipInches,0),
         NumSigPrecip= ifelse(PrecipInches>3.66, 1,0))%>%
  select(Date , year, month, 
         day_of_month, PrecipInches, sigPrecip, NumSigPrecip)%>%
  drop_na()
```

Analysis of Data
========================================================



Results
========================================================
- Hi
- Bullet 2
- Bullet 3

Summary
========================================================
- Hi
- Bullet 2
- Bullet 3
