library(shiny)
library(tidyverse)
library(tibbletime)
library(cowplot)
library(sf)
library(gganimate)
library(transformr)

# Load Data ----
## MD ZIP Shapes -----
st<- st_read('www/Maryland_Political_Boundaries_-_ZIP_Codes_-_5_Digit-shp/BNDY_ZIPCodes5Digit_MDP.shp')

# https://data.imap.maryland.gov/datasets/mdcovid19-master-zip-code-cases/data
md_zip_temporal <- read_csv('https://opendata.arcgis.com/datasets/5f459467ee7a4ffda968139011f06c46_0.csv')
md_zip_temporal[is.na(md_zip_temporal)] <- 0
md_zip_population <- read_csv('www/Maryland_Census_Data_-_ZIP_Code_Tabulation_Areas__ZCTAs_.csv') %>% 
  select(ZCTA5CE10, POP100) %>% mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
colnames(md_zip_population) <- c('ZIP_CODE', 'Population')

dates <- grep('^F\\d|^total\\d', colnames(md_zip_temporal), value = TRUE)

md_total_temporal <- md_zip_temporal %>% 
  pivot_longer(cols = all_of(dates)) %>% 
  select(-ZIP_CODE) %>% 
  group_by(name) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  mutate(date = gsub('^F','',name) %>% gsub('^total','',.) %>% 
           lubridate::parse_date_time(orders = 'mdy'))
min_date <- as.Date('2020-04-20')
max_date <- md_zip_temporal %>% 
  pivot_longer(cols = all_of(dates)) %>% 
  mutate(date = gsub('^F','',name) %>% gsub('^total','',.) %>% 
           lubridate::parse_date_time(orders = 'mdy')) %>% pull(date) %>% max()

md_zip_cases_over_time <- md_zip_temporal %>% 
  pivot_longer(cols = all_of(dates)) %>% 
  mutate(date = gsub('^F','',name) %>% gsub('^total','',.) %>% 
           lubridate::parse_date_time(orders = 'mdy')) %>% 
  mutate(Diff = value - lag(value)) %>% 
  filter(!is.na(Diff), Diff >= 0) %>% 
  mutate(ZIP_CODE = as.character(ZIP_CODE)) %>% 
  group_by(ZIP_CODE) %>% 
  mutate(rm = zoo::rollmean(x = Diff, 7, 
                            na.pad=TRUE, align="right")) %>% 
  left_join(md_zip_population) %>% 
  mutate(rmPop = (rm/Population)  * 100000) %>% 
  filter(!is.na(rmPop)) %>% 
  mutate(Date = as.Date(date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by="day")) # fill in missing dates

ani_data <-  st %>%
  left_join(md_zip_cases_over_time, by = c('ZIPCODE1' = 'ZIP_CODE')) %>%
  #filter(!is.na(rmPop)) %>% 
  mutate(C = case_when(rmPop > 25 ~ 25, TRUE ~ rmPop)) %>%
  mutate('New Cases\nper Day' = C) 

# ani_data$`New Cases\nper Day`[is.na(ani_data$`New Casesper Day` )] <- 0 # replace missing cases with 0 (which they presumably are)


animation <- ani_data %>% filter(Date >= '2020-04-20', Date <= '2020-07-30') %>% 
  ggplot(aes(fill = `New Cases\nper Day`)) + geom_sf(size = 0.1) + cowplot::theme_map() +
  scale_fill_viridis_c(option = 'magma', na.value = 'gray60') +
  transition_manual(Date) + 
  labs(title = '{current_frame} Maryland Zip Code\nLevel Cases (Smoothed over 7 days)\nPer Capita (Scaled to 100,000)')
  
  
animate(animation, 
        height = 400, 
        width = 600, 
        end_pause = 40, 
        nframes = 200)
  
  
  
  
  
  