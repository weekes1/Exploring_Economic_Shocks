library(tidyverse)
library(mdsr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(mosaic)
library(VGAM)

#Overall price increase average and price decrease  average for the entire time period. 
US_OIL_PRICES <- read.csv("~/R programming class/US_OIL_PRICES.csv", header=TRUE)

US_OIL_PRICES<-US_OIL_PRICES%>%
  mutate(
    OIL_PRICE_BASE_1982_1984=CUSR0000SETB01,
    DATE=mdy(DATE)
  )
US_OIL_PRICES

Oil_Price_plot<-ggplot(US_OIL_PRICES,aes(x=DATE,y=OIL_PRICE_BASE_1982_1984))+
  geom_line()+ 
  scale_x_date(date_labels = " %Y",breaks = "2 years")+
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  labs(title = "Consumer Price Index for All Urban Consumers: Gasoline",
       subtitle = "Price Reference for Oil",
       caption = "Data source: fred.stlouisfed.org",
       x = "Year", y = "Oil Price Index (1982-1984=100)",
       tag = "Fig 1")

Oil_Price_plot

df<-data.frame(US_OIL_PRICES)

differences_in_CPI<-diff(df$OIL_PRICE_BASE_1982_1984)  
differences_in_CPI[1]

diff_container<-vector("numeric",670)

for(i in 1:669){
 diff_container[i]=differences_in_CPI[i]
}
diff_container[670]=0
diff_container



US_OIL_PRICES<-US_OIL_PRICES%>%
  mutate(
    differences=diff_container,
    pct_df=differences/OIL_PRICE_BASE_1982_1984
  )
US_OIL_PRICES

df2<-data.frame(US_OIL_PRICES)
pct_diff_df<-df2$pct_df
pct_diff_df
pos_diff_container<- vector()
neg_diff_container<- vector()

for(i in 1:670){
  if(pct_diff_df[i]>0){
    pos_diff_container[i]<-pct_diff_df[i]
  }
  if(pct_diff_df[i]<0){
    neg_diff_container[i]<-pct_diff_df[i]
  }
}


neg_diff_container[670]=0
pos_diff_container[670]=0
US_OIL_PRICES<-US_OIL_PRICES%>%
  mutate(
    pos_pct_diff=pos_diff_container,
    neg_pct_diff=neg_diff_container
  )
US_OIL_PRICES

Positive_Percent_Changes<-US_OIL_PRICES%>%
  select(pos_pct_diff)%>%
  filter(pos_pct_diff!="NA")%>%
  summarize(
    mean_percent_increases_OIL_CPI=mean(pos_pct_diff)
  )

Positive_Percent_Changes

Negative_Percent_Changes<-US_OIL_PRICES%>%
  select(neg_pct_diff)%>%
  filter(neg_pct_diff!="NA")%>%
  summarize(
    mean_percent_decreases_OIL_CPI=mean(neg_pct_diff)
  )

Negative_Percent_Changes

#Examining oil price shock from 1972-1974 

OilPrices_1972_sample<-US_OIL_PRICES%>%
mutate(year=year(as.Date(DATE, format="%d/%m/%Y")))%>%
  filter(year>=1972 & year<=1976)

OilPrices_1972_sample

Oil_Price_plot_72_shock<-ggplot(OilPrices_1972_sample,aes(x=DATE,y=OIL_PRICE_BASE_1982_1984))+
  geom_line()+ 
  scale_x_date(date_labels = "%B %Y",breaks = "6 months")+
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  labs(title = "Consumer Price Index for All Urban Consumers: Gasoline (Oil Embargo of 1973)",
  subtitle = "Price Reference for Oil",
  caption = "Data source: fred.stlouisfed.org",
  x = "Year and Month", y = "Oil Price Index (1982-1984=100)",
  tag = "Fig 2")

Oil_Price_plot_72_shock

Positive_Percent_Changes_1972_sample<-OilPrices_1972_sample%>%
  select(pos_pct_diff)%>%
  filter(pos_pct_diff!="NA")%>%
  summarize(
    mean_percent_increases_OIL_CPI=mean(pos_pct_diff)
  )

Positive_Percent_Changes_1972_sample

Negative_Percent_Changes_1972_sample<-OilPrices_1972_sample%>%
  select(neg_pct_diff)%>%
  filter(neg_pct_diff!="NA")%>%
  summarize(
    mean_percent_decreases_OIL_CPI=mean(neg_pct_diff)
  )

Negative_Percent_Changes_1972_sample

#Fairly small amount of observations; bootsrapping might be useful?

Positive_changes_sample<-OilPrices_1972_sample%>%
  select(pos_pct_diff)%>%
  filter(pos_pct_diff!="NA")

Positive_changes_sample
nboot=1000
boot_Positive_percent_changes_1972<-vector("numeric",1000)

for(i in 1:nboot){
  bootsample<-Positive_changes_sample%>%
   slice_sample(n=39, replace = T)
  boot_Positive_percent_changes_1972[i]=mean(bootsample$pos_pct_diff)
}
  mean(boot_Positive_percent_changes_1972)
  
  Negative_changes_sample<-OilPrices_1972_sample%>%
    select(neg_pct_diff)%>%
    filter(neg_pct_diff!="NA")
  
  Negative_changes_sample
  nboot=1000
  boot_Negative_percent_changes_1972<-vector("numeric",1000)
  
  for(i in 1:nboot){
    bootsample<-Negative_changes_sample%>%
      slice_sample(n=14, replace = T)
    boot_Negative_percent_changes_1972[i]=mean(bootsample$neg_pct_diff)
  }
  mean(boot_Negative_percent_changes_1972)

#######################################################################################################
  #Examining oil price shock from 1990-1992 
  
  OilPrices_1990_sample<-US_OIL_PRICES%>%
    mutate(year=year(as.Date(DATE, format="%d/%m/%Y")))%>%
    filter(year>=1990 & year<=1992)
  
  OilPrices_1990_sample
  
  Oil_Price_plot_90_shock<-ggplot(OilPrices_1990_sample,aes(x=DATE,y=OIL_PRICE_BASE_1982_1984))+
    geom_line()+ 
    scale_x_date(date_labels = "%B %Y",breaks = "6 months")+
    theme(axis.text.x=element_text(angle=50, hjust=1))+
    labs(title = "Consumer Price Index for All Urban Consumers: Gasoline (Iraq-Kuwait War)",
    subtitle = "Price Reference for Oil",
    caption = "Data source: fred.stlouisfed.org",
    x = "Year and Month", y = "Oil Price Index (1982-1984=100)",
    tag = "Fig 3")
  
  Oil_Price_plot_90_shock
  
  Positive_Percent_Changes_1990_sample<-OilPrices_1990_sample%>%
    select(pos_pct_diff)%>%
    filter(pos_pct_diff!="NA")%>%
    summarize(
      mean_percent_increases_OIL_CPI=mean(pos_pct_diff)
    )
  
  Positive_Percent_Changes_1990_sample
  
  Negative_Percent_Changes_1990_sample<-OilPrices_1990_sample%>%
    select(neg_pct_diff)%>%
    filter(neg_pct_diff!="NA")%>%
    summarize(
      mean_percent_decreases_OIL_CPI=mean(neg_pct_diff)
    )
  
  Negative_Percent_Changes_1990_sample
  
  #Fairly small amount of observations; bootsrapping might be useful?
  
  Positive_changes_sample<-OilPrices_1990_sample%>%
    select(pos_pct_diff)%>%
    filter(pos_pct_diff!="NA")
  
  Positive_changes_sample
  nboot=1000
  boot_Positive_percent_changes_1990<-vector("numeric",1000)
  
  for(i in 1:nboot){
    bootsample<-Positive_changes_sample%>%
      slice_sample(n=21, replace = T)
    boot_Positive_percent_changes_1990[i]=mean(bootsample$pos_pct_diff)
  }
  mean(boot_Positive_percent_changes_1990)
  
  Negative_changes_sample<-OilPrices_1990_sample%>%
    select(neg_pct_diff)%>%
    filter(neg_pct_diff!="NA")
  
  Negative_changes_sample
  nboot=1000
  boot_Negative_percent_changes_1990<-vector("numeric",1000)
  
  for(i in 1:nboot){
    bootsample<-Negative_changes_sample%>%
      slice_sample(n=15, replace = T)
    boot_Negative_percent_changes_1990[i]=mean(bootsample$neg_pct_diff)
  }
  mean(boot_Negative_percent_changes_1990)

##################Exploring consumption shocks and how they affect the economy.#######################################

Consumption<- read.csv("~/R programming class/PCEC96.csv", header=TRUE)
Consumption

Consumption<-Consumption%>%
  mutate(DATE=mdy(DATE))

Consumption_plot<-ggplot(Consumption,aes(x=DATE,y=PCEC96))+
  geom_line()+ 
  scale_x_date(date_labels = " %Y",breaks = "1 years")+
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  labs(title = "Real Personal Consumption Expenditures",
   subtitle = "Total Consumption in Billions of Chained 2012 Dollars ",
   caption = "Data source: fred.stlouisfed.org",
   x = "Year", y = "Consumption",
   tag = "Fig 4")

Consumption_plot

cons_df<-data.frame(Consumption)

diff_in_cons<-diff(cons_df$PCEC96)

diff_in_cons[250]=0
diff_in_cons

Consumption_pct_change<-Consumption%>%
  mutate(Change_in_Cons=diff_in_cons)%>%
  mutate(Percent_change_in_cons=Change_in_Cons/PCEC96)
Consumption_pct_change

fav_stats(Consumption_pct_change$Percent_change_in_cons)#sd=0.01177263 mean=0.001916097

median_shock<-0.002006939

Percent_Change_In_Consumption_dist<- ggplot(Consumption_pct_change, aes(x=Percent_change_in_cons))+
  geom_density()+
  labs(title = "Density Plot of Percent Changes in Consumption",
       subtitle = "Approximation of Probability Distribution ",
       caption = "Data source: fred.stlouisfed.org",
       x = "Percent Change in Consumption", y = "Counts",
       tag = "Fig 5")



Percent_Change_In_Consumption_dist #Remove outliers? 

qqnorm(Consumption_pct_change$Percent_change_in_cons,main= "Figure 6: Normal Q-Q Plot")
qqline(Consumption_pct_change$Percent_change_in_cons,main= "Figure 6: Normal Q-Q Plot")

###################################### Using Normal distribution ####################################################
consumption_shocks_normal_dist<-vector(mode="numeric", length=1000)

for(i in 1:1000){
  consumption_shocks_normal_dist[i]=rnorm(1,0.001916097,0.01177263)
}

consumption_shocks_normal_dist_df<-data.frame(x=consumption_shocks_normal_dist)
consumption_shocks_normal_dist_df

hist_consumption_shocks_normal_distribution<-ggplot(consumption_shocks_normal_dist_df, aes(x=x))+
geom_density()+
  labs(title = "Simulated Density Plot of Percent Changes in Consumption (assuming normality)",
       subtitle = "Approximation of Probability Distribution",
       caption = "Simulated Using a normal distribution with mean=0.001916097 and sd=0.01177263",
       x = "Percent Change in Consumption", y = "Counts",
       tag = "Fig 7")

hist_consumption_shocks_normal_distribution

hist(consumption_shocks_normal_dist)

#Probability of consumption shock during covid under normal dist.

successes<- vector(mode="logical", length=1000)#sd=0.01177263 mean=0.001916097

for(i in 1:1000){
  draw<-rnorm(1,0.001916097,0.01177263)
  successes[i]= ifelse(
    draw>0.08554384,T,F
  )
}
successesdist<-tibble(
  results=successes
)
mosaic::binom.test(~results, 1000, success = T, data = successesdist)

#Pobability of consumption shock during 2008-2009 financial crisis under normal dist.

successes<- vector(mode="logical", length=1000)#sd=0.01177263 mean=0.001916097

for(i in 1:1000){
  draw<-rnorm(1,0.001916097,0.01177263)
  successes[i]= ifelse(
    draw>.009222152,T,F#Number taken from 2009-08-01
  )
}
successesdist<-tibble(
  results=successes
)
mosaic::binom.test(~results, 1000, success = T, data = successesdist)

# Probability of a consumption shock being bigger than a randomly gerorated shock under normal dist
set.seed(111)
Random_consumption_change<-runif(1,8.34463e-05,0.003851768)#Using Q1 and Q3 from the data
Random_consumption_change

successes<- vector(mode="logical", length=1000)#sd=0.01177263 mean=0.001916097

for(i in 1:1000){
  draw<-rnorm(1,0.001916097,0.01177263)
  successes[i]= ifelse(
    draw>Random_consumption_change,T,F
  )
}
successesdist<-tibble(
  results=successes
)
mosaic::binom.test(~results, 1000, success = T, data = successesdist)

###################################### Using Laplace distribution ####################################################
fav_stats(Consumption_pct_change$Percent_change_in_cons)#sd=0.01177263 mean=0.001916097

median_shock<-0.002006939#estimator#1

deviations_from_median<-c()

for(i in 1:250){
  deviations_from_median[i]=Consumption_pct_change$Percent_change_in_cons[i]-median_shock
}
deviations_from_median
Scale_estimator<--1*mean(deviations_from_median)
Scale_estimator#estimator#2

consumption_shocks_laplace_dist<-vector(mode="numeric", length=1000)

for(i in 1:1000){
  consumption_shocks_laplace_dist[i]=rlaplace(1,median_shock,Scale_estimator)
}

consumption_shocks_laplace_dist_df<-data.frame(x=consumption_shocks_laplace_dist)
consumption_shocks_laplace_dist_df

hist_consumption_shocks_laplace_distribution<-ggplot(consumption_shocks_laplace_dist_df, aes(x=x))+
  geom_density()+
  labs(title = "Simulated Density Plot of Percent Changes in Consumption (assuming Laplace Distribution)",
       subtitle = "Approximation of Probability Distribution",
       caption = "Simulated Using a Laplace distribution with med=0.002006939 and scale=9.08423e-05",
       x = "Percent Change in Consumption", y = "Counts",
       tag = "Fig 8")
hist_consumption_shocks_laplace_distribution

hist(consumption_shocks_laplace_dist)

#Covid consumption shock probability under laplace dist
successes<- vector(mode="logical", length=1000)

for(i in 1:1000){
  draw<-rlaplace(1,median_shock,Scale_estimator) 
  successes[i]= ifelse(
    draw>0.08554384,T,F #figure out how to scale
  )
}
successesdist<-tibble(
  results=successes
)
mosaic::binom.test(~results, 1000, success = T, data = successesdist)

#2008-2009 financial crisis consumption shock probability under laplace dist
successes<- vector(mode="logical", length=1000)

for(i in 1:1000){
  draw<-rlaplace(1,median_shock,Scale_estimator) 
  successes[i]= ifelse(
    draw>.009222152,T,F#Number taken from 2009-08-01
  )
}
successesdist<-tibble(
  results=successes
)
mosaic::binom.test(~results, 1000, success = T, data = successesdist)

# Probability of a consumption shock being bigger than a randomly generated shock under Laplace dist
set.seed(111)
Random_consumption_change<-runif(1,8.34463e-05,0.003851768)#Using Q1 and Q3 from the data
Random_consumption_change

successes<- vector(mode="logical", length=1000)

for(i in 1:1000){
  draw<-rlaplace(1,median_shock,Scale_estimator) 
  successes[i]= ifelse(
    draw>Random_consumption_change,T,F
  )
}
successesdist<-tibble(
  results=successes
)
mosaic::binom.test(~results, 1000, success = T, data = successesdist)

