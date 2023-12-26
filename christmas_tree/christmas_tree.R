library(ggplot2)
library(sf)
library(dplyr)
library(here)
library(showtext)
library(ggtext)
library(ggfx)
here("christmas_tree/utils-christmas_tree.R") |> source()

font_add_google("Great Vibes", "great_vibes")
font_add_google("Source Sans 3", "source_sans")
font_add(family = "fa",
         regular = "fontawesome/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

col_trunk <- "#63250e"
col_tree <- "#075600"
col_star <- "#f0db4d"
col_ornament <- "#aa0000"
col_sky <- "#131862"

social <- paste0(
  "<span style='font-family:fa;'>&#xf09b;</span>",
  tab(2),
  "sarahzeller",
  tab(4),
  "<span style='font-family:fa;'>&#xf099;</span>",
  tab(2),
  "@sarah_y_zeller"
)

ground <- tibble::tribble( ~ x, ~ y,-4, 0,-4, 1.25) |>
  mirror() |>
  make_polygon()

trunk <- tibble::tribble( ~ x, ~ y,-.4, 1,-.4, 2,) |>
  mirror() |>
  make_polygon()

tree <- tibble::tribble( ~ x, ~ y,-4, 1.5,-2, 4,-3, 4,-1, 6,-2, 6,
                         0, 8) |>
  mutate(x = .6 * x) |>
  mirror() |>
  make_polygon()

set.seed(123)

snow_flakes <- data.frame(
  x = runif(75, min = -4, max = 4),
  y = runif(75, min = 2, max = 10)
) |>
  st_as_sf(coords = c("x", "y"))


ornaments <-
  data.frame(x = rep(-2.5:2.5, 7),
             y = rep(2:8, 6),
             colour = sample(c(col_ornament, col_star), 42, replace = TRUE),
             ornament_size = sample(c(1.25, 1.5, 2), 42, replace = TRUE)) |>
  st_as_sf(coords = c("x", "y")) |>
  st_intersection(tree) |> 
  st_jitter(.25) |> 
  st_intersection(tree)

text_position <- c(0, 9) |> st_point()


ggplot() +
  geom_sf(data = ground,
          fill = "white",
          col = "white") +
  geom_sf(data = trunk, fill = col_trunk, col = col_trunk) +
  geom_sf(data = snow_flakes, col = "white", shape = 8, size = .5) +
  geom_sf(data = tree, fill = col_tree, col = col_tree) +
  geom_sf(
    data = ornaments,
    aes(fill = colour, col = colour, size = ornament_size),
  ) |> 
  with_outer_glow(colour = col_star, sigma = 10, expand = 3)+
  geom_sf_text(data = text_position,
               label = "Merry Christmas!",
               col = col_ornament,
               family = "great_vibes",
               size = 15)  +
  scale_shape_identity() +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_size_identity() +
  coord_sf(expand = FALSE) +
  labs(caption = social) +
  theme_void() +
  theme(panel.background = element_rect(fill = col_sky),
        plot.caption = element_textbox_simple(colour = col_sky,
                                              lineheight = .5,
                                              size = 15,
                                              family = "source_sans",
                                              margin = margin(b = 10, l = 20)))

here("christmas_tree/christmas_tree.png") |> ggsave(bg = "white")
