

#Base de datos y librerias ----
install.packages("reshape")
library(tidyverse)
library(reshape)
library(scales)


#Cargar datos ----
data <- tidytuesdayR::tt_load("2022-09-27")

artist <- data$artists

# Crear data_artist ----
data_artist <- artist |> 
  mutate(raza = if_else(race == "Hispanic", "Hispanic", "Non-Hispanic"))|>
  mutate(type = as.factor(type))|> 
  filter(state == "California")|>
  group_by(type, state, raza) |> 
  summarize(artist = sum(artists_n, na.rm = T))|>
  mutate(porcen = round(artist / sum(artist), 2))


orden_categ <- data_artist |> 
  filter(raza == "Non-Hispanic") |>
  mutate(type = as.character(type)) |> 
  arrange(desc(porcen)) |> 
  pull(type)

# Grafica ----
ggplot(data_artist, aes(x = porcen, y = fct_relevel(type, orden_categ), fill = raza, label = porcen)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(color = raza),
            size = 5, 
            fontface = "bold", 
            position = position_stack(vjust = 0.5),
            show.legend = F)+
  labs(
    x = NULL,
    y = NULL,
    title = "Desagregación de artistas en California",
    fill = "Raza",
    caption = "Fuente: Elaborado por María Fernanda Henao con base en datos de arts.gov"
  ) +
  scale_x_continuous(expand = c(0,0))+
  scale_color_manual(
    values = c("#000000", "#ffffff")
  ) +
  scale_fill_manual(
    breaks = c("Hispanic", "Non-Hispanic"), 
    labels = c("Hispanos", "No Hispanos"),
    values = c("#DCD6F7", "#424874")
  )+
  scale_y_discrete(
    labels = c(
      "Announcers" = "Locutores",
      "Dancers And Choreographers" = "Bailarines y Coreográfos",
      "Photographers" = "Fotógrafos",
      "Musicians" = "Músicos",
      "Entertainers" = "Animadores",
      "Designers" = "Diseñadores", 
      "Music Directors And Composers" = "Directores de Música y Compositores",
      "Architects" = "Arquitectos",
      "Actors" = "Actores",
      "Fine Artists, Art Directors, And Animators" = "Artista Plástico, Directores Artísticos",
      "Producers And Directors" = "Productores y Directores",
      "Landscape Architects" = "Arquitectos de paisajes",
      "Writers And Authors" = "Escritores y Autores"
    )
  )+
  theme(
    plot.title = element_text(size = 25, hjust = 0.5, face = "bold"),
    plot.title.position = "plot",
    plot.caption = element_text(size = 13),
    plot.margin = margin(t = 0, b = 0.25, r = 0.5, l = 0.5, unit = "cm"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 13),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = c(0.23,0),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  )
 
