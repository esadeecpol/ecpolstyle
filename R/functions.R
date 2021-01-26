#This is the EsadeEcPol theme & palette script!

##Theme

theme_ecpol <- function(){
  theme(
    #text elements
    panel.grid.major = element_line(color = "#f0f0f0", size=.2),
    panel.grid.minor = element_line(color = "#f0f0f0", size=.1),
    plot.background = element_rect(color="#ffffff", fill="#ffffff"),
    panel.background = element_rect(color="#ffffff", fill="#ffffff"),
    plot.title = element_text(hjust = 0, size = 14,
                              family="Mabry Pro Medium",
                              color = "#000000"),
    plot.subtitle = element_text(hjust = 0, size = 14,
                                  family="Mabry Pro Light",
                                  color = "#000000"),
    plot.caption = element_text(hjust = 0, size = 9.5,
                                family="Mabry Pro Light",
                                color = "#777777"),
    legend.position="top",
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.title = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 9.5,
                                family="Mabry Pro Light",
                                color = "#444444"),
    axis.title.y = element_text(angle = 0, size= 9.5,
                                family="Mabry Pro Light",
                                color = "#444444"),
    axis.text.x = element_text(size = 8.5,
                               family="Mabry Pro Light"),
    axis.text.y = element_text(size = 8.5,
                               color = "grey27",
                               family="Mabry Pro Light"),
  )
}


##Palette

palettes_ecpol <- list(
  ksnet_ecpol = c("#00B2A9"),
  ksnet_ecpol_two = c("#00B2A9", "#E05656"),
  twocolors = c("#00A47D","#EFB43F"),
  threecolors = c("#225E9C","#00A47D","#EFB43F"),
  semaforo_inverted = c("#00A47D","#EFB43F","#E05656"),
  semaforo = c("#00A47D","#EFB43F","#E05656"),
  fourcolors = c("#E05656","#EFB43F","#00A47D","#225E9C"),
  fivecolors = c("#E05656","#EFB43F","#00A47D","#225E9C","#404040"),
  sixcolors = c("#E6007E","#E05656","#EFB43F","#00A47D","#225E9C","#404040"),
  eightcolors = c("#E6007E","#E05656","#F27200","#EFB43F","#00A47D","#225E9C","#404040","#1a1a1a"),
  tencolors = c("#952EA0","#E6007E","#E05656","#F27200","#EFB43F","#00A47D","#225E9C","#4B2991","#404040","#1a1a1a")
)
palette_ecpol <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- palettes_ecpol[[name]]
  if (is.null(pal))
    stop("De que me hablas nano")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("No hay tanto color")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

palette_ksnet <- palette_ecpol("ksnet_ecpol")
palette_ksnet_two <- palette_ecpol("ksnet_ecpol_two")
palette_two <- palette_ecpol("twocolors")
palette_semaforoinverted <- palette_ecpol("semaforo_inverted")
palette_semaforo <- palette_ecpol("semaforo")
palette_three <- palette_ecpol("threecolors")
palette_four <- palette_ecpol("fourcolors")
palette_ive <- palette_ecpol("fivecolors")
palette_six <- palette_ecpol("sixcolors")
palette_eight <- palette_ecpol("eightcolors")
palette_ten <- palette_ecpol("tencolors")


