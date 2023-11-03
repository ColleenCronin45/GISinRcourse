install.packages("installr")
library(installr)
updateR()

library(tidyverse)

## Wickham & Grolemund 3: Data Visualization

ggplot2::mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg)
?mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

mpg

#continuous variables with color, size and shape

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy, color = displ))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy, size = displ))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy, shape = displ))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy, color = displ, size = displ))

? geom_point

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy, stroke = displ))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = drv, color = displ < 5))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = drv, color = displ))

## Facets

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

# facet on a continuous variable

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(~ displ)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

## geoms
# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

## Smooth line displays just a subset of the mpg dataset, the subcompact cars. The local data argument in geom_smooth() overrides the global data argument in ggplot() for that layer only.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

## exercises

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, group = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy,)) + 
  geom_point(mapping = aes(color = drv)) + 
  geom_smooth(se = FALSE)

## I think I did this one wrong even though it looks right
ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy, group = drv), color = 'blue', se = FALSE)

## I could not figure the last one out - maybe it's a shape with a border
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point(se = TRUE)

## statistical transformations

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

?stat_bin

## why we use group = 1
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop)))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = after_stat(prop)))

## color

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

## position - above is stacking, below are options for identity, dodge and fill

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

## fill is like stacking, but makes each set of stacked bars the same height. This makes it easier to compare proportions across groups.

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

##dodge places overlapping objects directly beside one another. This makes it easier to compare individual values.

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

##position = jitter prevents overplotting

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

?geom_jitter

##coodinate systems

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

?labs()

?coord_quickmap

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()

?geom_abline

?coord_fixed
