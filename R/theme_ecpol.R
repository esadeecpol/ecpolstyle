theme_ecpol <- function(){
  theme(
    #text elements
    panel.grid.major = element_line(color = "#f0f0f0", size=.2),
    panel.grid.minor = element_line(color = "#f0f0f0", size=.1),
    plot.background = element_rect(color="#ffffff", fill="#ffffff"),
    panel.background = element_rect(color="#ffffff", fill="#ffffff"),
    plot.title = element_text(hjust = 0, size = 13,
                              family="Mabry Pro Medium",
                              color = "#000000"),
    plot.subtitle = element_text(hjust = 0, size = 12,
                                  family="Mabry Pro Light",
                                  color = "#000000"),
    plot.caption = element_text(size = 9,
                                vjust = .5,
                                family="Mabry Pro Light",
                                color = "#777777"),
    legend.position="none",
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10,
                               family="Mabry Pro Light"),
    axis.text.y = element_text(size = 10,
                               color = "grey27",
                               family="Mabry Pro Light"),
    strip.text = element_text(family = "Mabry Pro Light", size = 10),
    strip.background = element_rect(fill = "transparent")
  )
}