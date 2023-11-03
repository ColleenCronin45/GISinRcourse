
# setup -------------------------------------------------------------------

library(tidyverse)

# Read in data

chickadees <-
  read_csv('C:/Users/User/Documents/Week1/gis/gis/data/raw/chickadees.csv')

read_rds('C:/Users/User/Documents/Week1/gis/gis/data/raw/district_birds.rds') %>% 
  list2env(.GlobalEnv)

captures

birds

ggplot(data = chickadees)

chickadees %>% 
  ggplot()

chickadees %>% 
  ggplot(mapping = aes(x = wing, y = mass))

chickadees %>% 
  ggplot(aes(x = wing, y = mass))

chickadees %>% 
  ggplot(aes(x = wing, y = mass)) +
  geom_point()

chickadees %>% 
  ggplot(aes(x = wing, y = mass)) +
  geom_point() +
  geom_smooth(
    formula = y ~ x, 
    method = 'lm',
    se = FALSE, 
    color = 'black')

# color points by species

chickadees %>% 
  ggplot(aes(x = wing, y = mass)) +
  geom_point(aes(color = spp)) +
  geom_smooth(
    formula = y ~ x, 
    method = 'lm',
    se = FALSE, 
    color = 'black')

# linear model/ line separate by species and color
chickadees %>% 
  ggplot(aes(x = wing, y = mass)) +
  geom_point(aes(color = spp)) +
  geom_smooth(
    aes(color = spp),
    method = 'lm',
    formula = y ~ x, 
    se = FALSE)

# simplifying

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE)

# larger points

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE)

#adding transparency

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE)

# widening the line of best fit

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5)

# Bar Plots

captures %>% 
  ggplot(aes(x = spp)) +
  geom_bar()

#summary table

captures %>% 
  group_by(spp) %>% 
  summarize(n = n())

#pipe the above into the bar graph

captures %>% 
  group_by(spp) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = spp, y = n)) +
  geom_bar(stat = "identity")

# joining tables for species names

captures %>% 
  left_join(
    birds %>% 
      select(species:common_name),
    by = c("spp" = "species"))

#reducing to columns we are interested in

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species:common_name),
    by = c("spp" = "species")) %>% 
  select(common_name)

# calculating counts again

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species:common_name),
    by = c("spp" = "species")) %>% 
  group_by(common_name) %>% 
  summarize(n = n())

#plotting it

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species:common_name),
    by = c("spp" = "species")) %>% 
  group_by(common_name) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = common_name, y = n)) +
  geom_bar(stat = "identity")

#plotting based on dietary guilds

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(diet) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = diet, y = n)) +
  geom_bar(stat = "identity")

#adding species info

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      color = common_name)) +
  geom_bar(stat = "identity")

# filling in the bars

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity")

#adding back in black outline

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity",
           color = "black")

#change line width

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity",
           color = "black",
           linewidth = 0.3)

#modify colors

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity",
           color = "#424242",
           linewidth = 0.3)

#subset data with just the ten species with most observations

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  slice_max(n, n = 10) %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity",
           color = "#424242",
           linewidth = 0.3)

# check data limits before scaling

summary(chickadees)

# changing limits to reflect mass range

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0))

#scaling wing axis 50-70

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0))

#removing the wiggle room in height/ y-axis

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  slice_max(n, n = 10) %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity",
           color = "#424242",
           linewidth = 0.3) +
  scale_y_continuous(
    expand = c(0, 0))

#checking the distribution of the data

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>%
  summary()

#simply data grouping

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>%
  summary()

#based on above, changing y-axis scale to 0-4000

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  slice_max(n, n = 10) %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity",
           color = "#424242",
           linewidth = 0.3) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 4000))

# using expansion function to simplify the scaling limits process above

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  slice_max(n, n = 10) %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity",
           color = "#424242",
           linewidth = 0.3) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)))

#using add instead of mult in the expansion function above

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  slice_max(n, n = 10) %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity",
           color = "#424242",
           linewidth = 0.3) +
  scale_y_continuous(
    expand = expansion(add = c(0, 100)))

#manually changing the color sclae for scatterplot

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    values = c('orange', 'green'))

#tweaking the colors

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    values = c('#595B18', '#CA621E'))

#using RColorBrewer palette for bar chart

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name, diet) %>% 
  summarize(
    n = n(),
    .groups = "drop") %>% 
  slice_max(n, n = 10) %>% 
  ggplot(
    aes(
      x = diet, 
      y = n,
      fill = common_name)) +
  geom_bar(stat = "identity",
           color = "#424242",
           linewidth = 0.3) +
  scale_y_continuous(
    expand = expansion(add = c(0, 100))) +
  scale_fill_brewer(palette = "Spectral")

# flip coordinate system

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name) %>% 
  summarize(n = n()) %>% 
  slice_max(n, n = 10) %>% 
  ggplot(aes(x = common_name, y = n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    expand = expansion(add = c(0, 100))) +
  coord_flip()

#remember that the x and y axes have NOT changed, only the coordinate system has

#add lables to scatterplot

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length', 
    y = 'Mass')

#adding units

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length (mm)', 
    y = 'Mass (g)')

#changing the legend title

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    'Species',
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length (mm)', 
    y = 'Mass (g)')

#changing labels on bar chart

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(common_name) %>% 
  summarize(n = n()) %>% 
  slice_max(n, n = 10) %>% 
  ggplot(aes(x = common_name, y = n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    expand = expansion(add = c(0, 100))) +
  coord_flip() +
  labs(
    title = "Banded birds by species",
    x = "Common name",
    y = "Number banded")

#using piping

captures %>% 
  select(spp) %>% 
  left_join(
    birds %>% 
      select(species, common_name, diet),
    by = c("spp" = "species")) %>% 
  group_by(`Common name` = common_name) %>% 
  summarize(
    `Number banded` = n(),
    .groups = "drop") %>% 
  slice_max(`Number banded`, n = 10) %>% 
  ggplot(aes(x = `Common name`, y = `Number banded`)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    expand = expansion(add = c(0, 100))) +
  coord_flip() +
  labs(title = "Banded birds by species")

#facet wrap by sex for scatterplot

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    'Species',
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length (mm)', 
    y = 'Mass (g)') +
  facet_wrap(~sex)

#changing the data to a factor and using the factor labels

table(chickadees$sex)

#changing variable to a factor

chickadees$sex <-
  factor(chickadees$sex, 
         levels = c('F', 'M'),
         labels = c('Female', 'Male'))

#plotting again with updated labels based on data changes

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    'Species',
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length (mm)', 
    y = 'Mass (g)') +
  facet_wrap(~sex)

#themes

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    'Species',
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length (mm)', 
    y = 'Mass (g)') +
  facet_wrap(~sex) +
  theme_bw()

#modifying size of axes text

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    'Species',
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length (mm)', 
    y = 'Mass (g)') +
  facet_wrap(~sex) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12)
  )

#changing facet box to yellow

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    'Species',
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length (mm)', 
    y = 'Mass (g)') +
  facet_wrap(~sex) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    strip.background = element_rect(fill = '#F6CD58')
  )

#heavier axis lines

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    'Species',
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length (mm)', 
    y = 'Mass (g)') +
  facet_wrap(~sex) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    strip.background = element_rect(fill = '#F6CD58'),
    axis.line = element_line(size = 1)
  )

#changing panel spacing

chickadees %>% 
  ggplot(
    aes(x = wing, 
        y = mass,
        color = spp)) +
  geom_point(size = 2.75,
             alpha = 0.25) +
  geom_smooth(
    method = 'lm',
    formula = y ~ x, 
    se = FALSE,
    size = 1.5) +
  scale_y_continuous(
    limits = c(7, 13),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(50, 70),
    expand = c(0,0)) +
  scale_color_manual(
    'Species',
    values = c('#595B18', '#CA621E')) +
  labs(
    title = 'Wing length and mass of Black-capped and Carolina chickadees',
    x = 'Wing length (mm)', 
    y = 'Mass (g)') +
  facet_wrap(~sex) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    strip.background = element_rect(fill = '#F6CD58'),
    axis.line = element_line(size = 1),
    panel.spacing = unit(1.5, 'lines'))

#saving the plot

ggsave(
  'C:/Users/User/Documents/week2/output/chickadee_wing_chord.png',
  height = 5,
  width = 7,
  units = 'in')
