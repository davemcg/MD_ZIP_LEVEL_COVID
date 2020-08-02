library(ggplot2)
library(shiny)
library(dplyr)
library(tibbletime)
library(sf)
library(tidyr)
library(Cairo)
library(readr)

# Load Data ----
## MD ZIP Shapes -----
st<- st_read('www/Maryland_Political_Boundaries_-_ZIP_Codes_-_5_Digit-shp/BNDY_ZIPCodes5Digit_MDP.shp')
st_county <- st_read('www/Maryland_Physical_Boundaries_-_County_Boundaries__Generalized_-shp/BNDY_CountyPhyBoundaryGen_DoIT.shp')
# https://data.imap.maryland.gov/datasets/mdcovid19-master-zip-code-cases/data
md_zip_temporal <- read_csv('https://opendata.arcgis.com/datasets/5f459467ee7a4ffda968139011f06c46_0.csv')

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


# Define UI ----
ui <-   fluidPage( 
                  theme = shinythemes::shinytheme(theme = 'flatly'),
                  title = 'MD Zip Code Level COVID19 Case Trends',
                  br(),
                  fluidRow(column(8, offset = 1, h2('MD Zip Code Level COVID19 Case Trends'))),
                  br(),
                  fluidRow(column(8, offset = 1, 'Data taken from ', tags$a(href ='https://data.imap.maryland.gov/datasets/mdcovid19-master-zip-code-cases/data', 'Maryland GIS Data Catalog.'),' Both plots shows cases per day with the following adjustments to smooth out noise and correct for population differences in the zip codes: ')),
                  fluidRow(column(8, offset = 1, tags$ul( tags$li("Each day is averaged with the previous 6 days to create a ", tags$a(href = 'https://en.wikipedia.org/wiki/Moving_average', 'rolling mean.')), 
                                                          tags$li("Cases are divided by the 2010 census population and multiplied by 100,000 to adjust for population in each zip code.")))),  
                  br(),
                  fluidRow(column(8, offset = 1, selectizeInput('md_zip_codes', strong('Select up to 5 ZIP Codes: '),
                                                                choices = NULL, selected = NULL, multiple = TRUE))),
                  fluidRow(column(8, offset = 1, sliderInput('md_date_range', label = 'Date Range',
                                                             value = c(as.Date('2020-06-14'), as.Date(max_date)),
                                                             min = as.Date(min_date),
                                                             max = as.Date(max_date)))),
                  fluidRow(column(8, offset = 1, plotOutput('zip_temporal'))),
                  br(),
                  fluidRow(''),
                  fluidRow(column(8, offset = 1, plotOutput('chloropleth',
                                                            dblclick = "map_dblclick",
                                                            brush = brushOpts(
                                                              id = "map_brush",
                                                              resetOnNew = TRUE
                                                            )))),
                  fluidRow(column(8, offset = 1, checkboxInput('zip_label', strong('Label zip codes on zoom in'), value = FALSE))),
                  fluidRow(column(8, offset = 1, 'To zoom, click and drag to set box, double-click to zoom in. Double click again to re-set full state view.')),
                  br(),
                  fluidRow(column(8, offset = 1, 'Source code for this app ', tags$a(href = 'https://github.com/davemcg/MD_ZIP_LEVEL_COVID_APP', 'here.'))),
                  br(),
                  br()
                  
                  
)

# Define server logic ----
server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'md_zip_codes',
                       choices = md_zip_population$ZIP_CODE %>% unique() %>% sort(),
                       options = list(placeholder = 'Type to search'),
                       selected = c('20782', '20910', '21201'),
                       server = TRUE)
  
  
  
  output$zip_temporal <- renderPlot({ 
    # Zip codes with largest shifts from mid July to early July
    # taking latest date - or / 7/1 with 7 day rolling mean
    
    # plotting cases per day (7 day rolling mean) by zip code
    md_total_temporal <- md_total_temporal %>% 
      mutate(Diff = value - lag(value)) %>% 
      filter(!is.na(Diff), Diff >= 0) %>% 
      mutate(rm = zoo::rollmean(x = Diff, 7, 
                                na.pad=TRUE, align="right")) %>% 
      mutate(rmPop = (rm/6e6)  * 100000) %>% 
      mutate(ZIP_CODE = 'MD')
    
    md_zip_cutdown <- md_zip_temporal %>% 
      pivot_longer(cols = all_of(dates)) %>% 
      filter(ZIP_CODE %in% c('MD', input$md_zip_codes)) %>% 
      mutate(date = gsub('^F','',name) %>% gsub('^total','',.) %>% 
               lubridate::parse_date_time(orders = 'mdy')) %>% 
      mutate(Diff = value - lag(value)) %>% 
      filter(!is.na(Diff), Diff >= 0) %>% 
      mutate(ZIP_CODE = as.character(ZIP_CODE)) %>% 
      group_by(ZIP_CODE) %>% 
      mutate(rm = zoo::rollmean(x = Diff, 7, 
                                na.pad=TRUE, align="right")) %>% 
      left_join(md_zip_population) %>% 
      mutate(rmPop = (rm/Population)  * 100000) 
    
    bind_rows(md_total_temporal, md_zip_cutdown) %>% 
      filter(date >= input$md_date_range[1],
             date <= input$md_date_range[2]) %>% 
      ggplot(aes(x=date,y=rmPop, color = ZIP_CODE)) + 
      #geom_point() + 
      geom_line() + 
      cowplot::theme_cowplot() +
      ylab('New cases per 100,000 per day') +
      xlab('Date') +
      scale_color_brewer(palette = 'Set1') 
  })
  
  map_scatter_ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$map_dblclick, {
    brush <- input$map_brush
    if (!is.null(brush)) {
      map_scatter_ranges$x <- c(brush$xmin, brush$xmax)
      map_scatter_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      map_scatter_ranges$x <- NULL
      map_scatter_ranges$y <- NULL
    }
    cat(map_scatter_ranges$x)
    cat('\n')
    cat(map_scatter_ranges$y)
  })
  
  output$chloropleth <- renderPlot({
    hot_spots <- md_zip_temporal %>%
      pivot_longer(cols = all_of(dates)) %>%
      #filter(ZIP_CODE %in% c(20814, 202722, 20782,20783,20740,20742, 20912,20781)) %>%
      mutate(date = gsub('^F','',name) %>% gsub('^total','',.) %>%
               lubridate::parse_date_time(orders = 'mdy')) %>%
      mutate(value = case_when(is.na(value) ~ 0, TRUE ~ value)) %>%
      mutate(Diff = value - lag(value)) %>%
      filter(!is.na(Diff), Diff >= 0) %>%
      mutate(ZIP_CODE = as.character(ZIP_CODE)) %>%
      group_by(ZIP_CODE) %>%
      mutate(rm = zoo::rollmean(x = value, 7,
                                na.pad=TRUE, align="right")) %>%
      filter(date == as.Date(input$md_date_range[1], origin = '1970-01-01') | 
               date == as.Date(input$md_date_range[2], origin = '1970-01-01')) %>%
      select(-name,  -OBJECTID, -value, -Diff) %>%
      pivot_wider(values_from = 'rm', names_from = 'date') 
    colnames(hot_spots)[c(2,3)] <- c('start','end')
    hot_spots <- hot_spots %>%
      mutate(Ratio = (end + 1) / (start + 1),
             Delta = end - start) %>%
      left_join(md_zip_population, by = 'ZIP_CODE') %>%
      arrange(-Ratio)
    
    p <- st %>%
      left_join(hot_spots, by = c('ZIPCODE1' = 'ZIP_CODE')) %>%
      mutate(C = (Delta / Population / as.numeric(as.Date(input$md_date_range[2], origin = '1970-01-01') - as.Date(input$md_date_range[1], origin = '1970-01-01'))) * 100000, 
             C = case_when(C > 25 ~ 25, TRUE ~ C)) %>%
      mutate('Average of\nNew COVID19 Cases\nper 100,000 per Day' = C) %>% 
      ggplot() + 
      geom_sf(aes(fill = `Average of\nNew COVID19 Cases\nper 100,000 per Day`, 
                  label = ZIPCODE1),
              size = 0.1) +
      geom_sf(data = st_county, color = 'white', size = 0.2, fill = NA, aes(label = NA)) +
      theme_void() +
      scale_fill_viridis_c(breaks = c(0, 5, 10, 15, 20, 25),
                           labels = c(0, 5, 10, 15, 20, "25 or more")) +
      coord_sf(xlim = map_scatter_ranges$x, ylim = map_scatter_ranges$y) + 
      ggtitle(paste0(as.Date(input$md_date_range[1]), ' to ', as.Date(input$md_date_range[2])))
    cat(map_scatter_ranges$x)
    sum_zoom <- ((as.numeric(map_scatter_ranges$x[1]) - as.numeric(map_scatter_ranges$x[2])) %>% abs()) +
      ((as.numeric(map_scatter_ranges$y[1]) - as.numeric(map_scatter_ranges$y[2])) %>% abs())
    #()
    cat('\n')
    cat(sum_zoom)
    cat('\n')
    if (length(sum_zoom) == 0){
      p
    } else if (sum_zoom < 100000 & input$zip_label){
      p + geom_sf_label(size = 4, alpha = 0.3, aes(label = ZIPCODE1), data = st)
    } else {p}
    #p
  })
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)