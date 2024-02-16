library(readxl)
library(tidyverse)
library(magrittr)

#https://data.worldbank.org/indicator/AG.LND.FRST.ZS?end=2021&start=1990&view=chart

data <- read_excel("data/forest_land_cover_data.xls", 
                                     col_types = c("text", "text", "text", 
                                                   "text", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric"), 
                                     skip = 2)

data %<>% 
    select(`Country Name`, `1990`:`2022`) %>% 
    pivot_longer(-`Country Name`, names_to="year") %>% 
    rename(country=`Country Name`)


data %>% 
    group_by(country) %>% 
    summarise(
        change = value[year==2020]/value[year==1990] -1
    ) %>% 
    mutate(
        century_change=(change/30)*100
    ) %>% 
    arrange(century_change) %>% print(n=60)

#model produce 58% without soil deg 
#IDA: International Development Association 

#Arab World -82%
#IDA countrues -60%
#Africa Eastern and Southern -56%
#Low income countries -56%
# Sub-Saharan Africa -52%
# Latin America & Caribbean -43%
# Africa Western and Central -43%
# Lower middle income -41%