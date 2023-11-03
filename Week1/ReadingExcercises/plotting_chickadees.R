
# setup -------------------------------------------------------------------

library(tidyverse)

# read in data

chickadees <-
  read_csv('C:/Users/User/Documents/Week1/gis/gis/data/raw/chickadees.csv')

# plotting chickadees -----------------------------------------------------

# sample size

ggplot(
  data = chickadees,
  mapping = aes(x = spp)) +
  geom_bar(mapping = aes(fill = sex),
           color = 'black') +
  scale_y_continuous(limits = c(0, 1000),
                     expand = c(0,0))

# mass, histogram

ggplot(
  data = chickadees,
  mapping = aes(x = mass)) +
  geom_histogram(mapping = aes(fill = spp)) +
  facet_wrap(~spp,
             nrow = 2)

#mass, boxplot

ggplot(
  data = chickadees,
  mapping = aes(x = spp, 
                y = mass, 
                fill = sex)) +
  geom_boxplot() +
  scale_fill_manual("Sex",
    values = c('#9EB8C5', '#F94C40')) +
  scale_y_continuous(limits = c(7, 15,
                                expand = c(0, 0))) +
  labs(title = "Mass of Male and Female Black-capped Carolina Chickadees",
       x = "Species",
       y = "Mass (g)")  +
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "#E9E9E9"),
        panel.grid.minor = element_line(linetype = "dashed"),
        axis.line = element_line(color = "black"))

# write plot to file

ggsave(filename = "chickadee_mass.png",
       path = "output/plots",
       width = 7.5,
       height = 5,
       units = "in")
