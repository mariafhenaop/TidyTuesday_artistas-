ggplot(data_artist, aes(x = porcen, y = type, fill = raza)) + 
  geom_col(position = "stack") +
  theme_minimal()

ggplot(data_artist, aes(x= porcen, y= type)) + 
  facet_wrap(~ raza, scales = "free_x") + 
  geom_col(aes(fill= raza),stat = "identity", position = "identity") + 
  theme(panel.spacing.x = unit(0, "mm"))+
  theme_minimal()  