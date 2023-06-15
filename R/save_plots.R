# Función para guardar gráficos en PNG

png_ecpol <- function(plot, filename = "", width = 18, height = 12, unit = "cm", dpi = 300) {
  ggsave(
    plot = plot,
    filename = filename,
    device = 'png',
    width = width,
    height = height,
    unit = unit,
    dpi = dpi
  )
}

# Función para guardar gráfcos en JPG
jpg_ecpol <- function(plot, filename = "", width = 18, height = 12, unit = "cm", dpi = 300) {
  ggsave(
    plot = plot,
    filename = filename,
    device = 'jpg',
    width = width,
    height = height,
    unit = unit,
    dpi = dpi
  )
}

# Función para guardar gráficos en PDF
pdf_ecpol <- function(plot, filename = "", width = 18, height = 12, unit = "cm", dpi = 300) {
  ggsave(
    plot = plot,
    filename = filename,
    device = cairo_pdf,
    width = width,
    height = height,
    unit = unit,
    dpi = dpi
  )
}