#datapasta::tribble_paste()
announcements <- tibble::tribble(
           ~Date,                                                                                                                    ~Announcement,
  "2020-03-30",                                                                                             "Hogan Stay at home order",
  "2020-05-06",                                                                                              "Ease Stay-at-home order",
  "2020-06-01",                                                                           "Montgomery and PG County begin to ease restrictions",
  "2020-06-12",                                                                      "Indoor dining restrictions lifted in most of MD",
  "2020-06-19",                                                                      "Day cares, gyms, indoor dining partially opened",
  "2020-07-14", "Hogan urges local leaders across his state to take bolder<br>steps to enforce physical distancing and mask requirements",
  "2020-07-22",                                                                                     "Baltimore suspends indoor dining",
  "2020-07-29",                                                                                           "Hogan expands mask mandate",
  "2020-08-04", "Tropical Storm Isaias passes through MD, case reports may fluctuate"
  )
announcements$Date <- as.character(announcements$Date, origin = '1970-01-01')
announcements$Label <- seq(1:9)
save(announcements, file = 'www/announcements.Rdata')
