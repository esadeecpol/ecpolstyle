
# Funcion para crear toda la estructura de las carpetas de un paper

directory_ecpol <- function() {
  # Directorio base
  base_dir <- getwd() # Obtenemos el directorio actual
  
  # Lista de carpetas
  folders <- c("code", "input/raw", "input/data", "output/tables", "output/png", "output/svg", "content")
  
  # Crear carpetas
  for (folder in folders) {
    dir.create(file.path(base_dir, folder), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Crear archivo .gitignore
  gitignore_content <- "*.csv\n*.dta\n*.rds"
  cat(gitignore_content, file = file.path(base_dir, ".gitignore"), sep = "\n")
  
  cat("Directorio creado exitosamente.\n")
}

# Llama a la función para crear las carpetas y el archivo .gitignore
directory_ecpol()
