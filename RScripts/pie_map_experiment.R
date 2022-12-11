library(tmaptools)


airports_inter$id <- 1:nrow(airports_inter)
airports_inter$pass_2019_TRUE[is.na(airports_inter$pass_2019_TRUE)] <- 0
airports_inter$pass_2019_FALSE[is.na(airports_inter$pass_2019_FALSE)] <- 0
airports_inter$pass_2019_total <- airports_inter$pass_2019_TRUE + airports_inter$pass_2019_FALSE
airports_inter <- airports_inter[airports_inter$pass_2019_total > 0,]


ai_plot <- st_drop_geometry(airports_inter)
ai_plot <- ai_plot[,c("id","airport","country","pass_2019_TRUE","pass_2019_FALSE")]
ai_plot <- pivot_longer(ai_plot, cols = c("pass_2019_TRUE","pass_2019_FALSE"), 
                        values_to = "pass_2019", names_to = "in_study", 
                        names_prefix = "pass_2019_")
ai_plot$pass_2019[is.na(ai_plot$pass_2019)] <- 0

origin_cols <- get_brewer_pal("Dark2", 2)

grobs <- lapply(split(ai_plot, ai_plot$id), function(x) {
  ggplotGrob(ggplot(x, aes(x="", y=pass_2019, fill=in_study)) +
               geom_bar(width=1, stat="identity") +
               scale_fill_manual(values=origin_cols) +
               coord_polar("y", start=0) +
               theme_ps(plot.axes = FALSE))
})

ggplot(ai_plot[ai_plot$airport == "Barcelona",], 
       aes(x="", y=pass_2019, fill=in_study)) +
  geom_bar(width=1, stat="identity") +
  scale_fill_manual(values=origin_cols) +
  coord_polar("y", start=0) +
  theme_ps(plot.axes = FALSE)


names(grobs) <- airports_inter$airport

tm_shape(airports_inter) +
  tm_symbols(shape = "airport",
             size = "pass_2019_total",
             sizes.legend=c(.5, 1,3)*1e6, 
             scale=1,
             border.lwd = NA,
             legend.shape.show = FALSE, 
             legend.size.is.portrait = TRUE, 
             shapes=grobs) +
  tm_shape(bounds) +
  tm_lines()
