# This is a script used to calculate models estimating convergent & divergent validity metrics.

rm( list = ls() ) # clean environment

# set-up libraries
library(here)
library(tidyverse)
library(ggdag)
library(ggraph)
library(patchwork)


# NAMES ----

nms <-
  
  data.frame(
    name = c("NBD", "N", "V", "M", "E", "J", "S", "CMS", "ROCFT", "VF", "BNT", "ToM"),
    x = c(3, 1:6, 1:2, 4:6),
    y = c(1, rep(2,6), rep(1,5) )
  ) %>%
  mutate( col = ifelse(name == "NBD", "white", "black") )


# PRE-RESULTS ----

# prepare the DAG
dag0 <- dagify(

  NBD ~ N + V + M + E + J + S,
  CMS ~ N + V,
  ROCFT ~ N + V + M + E,
  VF ~ E + J,
  BNT ~ J,
  ToM ~ J + S,

  coords = nms[ , c("name","x","y")]

) %>%

  tidy_dagitty() %>%
  arrange(name)

# plot it
fig_A <- dag0 %>%
  
  mutate(
    latent = if_else(is.na(direction), "0", "1"),
    NBD_node = if_else(name == "NBD", "0", "1"),
    NBD_edge = if_else(to == "NBD", "0", "1")
  ) %>%
  
  ggplot() +
  aes(x = x, y = y, xend = xend, yend = yend, shape = latent, colour = NBD_node) +
  geom_dag_point( size = 20 ) +
  geom_dag_edges( aes(edge_linetype = NBD_edge, edge_colour = NBD_edge) ) +
  scale_edge_linetype_manual( values = c(`1` = 1, `0` = 2) ) +
  scale_edge_colour_manual( values = c(`1` = "black", `0` = "red2") ) +
  scale_colour_manual( values = c("black","grey87") ) +
  scale_shape_manual( values = c(`1` = 19, `0` = 15) ) +
  geom_dag_text(
    label = arrange(nms, name)$name,
    colour = arrange(nms, name)$col,
    size = 3.6
  ) +
  theme_dag() +
  theme(legend.position = "none")


# POST-RESULTS ----

# prepare the DAG
dag1 <- dagify(
  
  NBD ~ N + V + M + E,
  CMS ~ N + V,
  ROCFT ~ N + V + M + E,
  VF ~ E + J,
  BNT ~ J,
  ToM ~ J + S,
  
  coords = nms[ , c("name","x","y")]
  
) %>%
  
  tidy_dagitty() %>%
  arrange(name)

# plot it
fig_B <- dag1 %>%
  
  mutate( latent = if_else(is.na(direction), "0", "1") ) %>%
  ggplot() +
  aes(x = x, y = y, xend = xend, yend = yend, shape = latent) +
  geom_dag_point(size = 20, colour =  "grey87") +
  geom_dag_edges() +
  scale_shape_manual( values = c(`1` = 19, `0` = 15) ) +
  geom_dag_text(
    label = arrange(nms, name)$name,
    colour = "black",
    size = 3.6
  ) +
  theme_dag() +
  theme(legend.position = "none")

# put them together
(fig_A / fig_B) + plot_annotation( tag_levels = "A", theme = theme( plot.tag = element_text(face = "bold") ) )

# save it
ggsave(
  plot = last_plot(),
  filename = here("figs","nomologic_net.jpg"),
  dpi = 300,
  width = 9,
  height = 9
)
