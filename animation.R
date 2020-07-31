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
  mutate(Date = as.Date(date))

ani_data <-  st %>%
  left_join(md_zip_cases_over_time, by = c('ZIPCODE1' = 'ZIP_CODE')) %>%
  filter(!is.na(rmPop)) %>% 
  mutate(C = case_when(rmPop > 25 ~ 25, TRUE ~ rmPop)) %>%
  mutate('Average of\nNew COVID19 Cases\nper 100,000 per Day' = C) 

ani_data <- na.omit(ani_data)

animation <- ani_data %>% filter(Date > '2020-05-01', Date < '2020-07-30') %>% 
  ggplot(aes(fill = `Average of\nNew COVID19 Cases\nper 100,000 per Day`)) + geom_sf(size = 0.1) + cowplot::theme_map() +
  scale_fill_viridis_c() +
  transition_time(Date) + 
  labs(title = '{frame_time} ZIP CODE Level Cases Smoothed over 7 days')
  
  
animate(animation, 
        height = 400, 
        width = 600, 
        end_pause = 30)
  
  
  
  
  
  