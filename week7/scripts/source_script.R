
# clean object names ------------------------------------------------------

# Clean object names:

clean_names <- 
  function(df) {
    df %>% 
      set_names(
        names(.) %>% 
          str_replace_all("[:blank:]", "_") %>% 
          str_replace_all("([a-z])([A-Z]){1}", "\\1_\\2") %>% 
          tolower())
  }


# lonlat to utm -----------------------------------------------------------

# Function for getting an epsg code from lonlat data (from Lovelace, chapter 7,
# https://geocompr.robinlovelace.net/reproj-geo-data.html):

lonlat_to_utm <-
  function(lonlat) {
    utm <-
      (floor((lonlat[1] + 180) / 6) %% 60) + 1
    if(lonlat[2] > 0) {
      utm + 32600
    } else{
      utm + 32700
    }
  }

# a plot theme ------------------------------------------------------------

universal_plot_theme <-
  function() {
    theme_bw() %+replace%
      theme(
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        complete = TRUE)
  }
