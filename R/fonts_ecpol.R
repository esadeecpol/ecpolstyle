fonts_ecpol <- function() {
  library(dplyr)
  library(stringr)
  library(systemfonts)
  
  mabry <- system_fonts() %>%
    filter(family == "Mabry Pro") %>%
    transmute(family, style, file = str_extract(path, "[\\w-]+\\.ttf$"))
  
  mabry_light_path <- system_fonts() %>%
    filter(family == "Mabry Pro", style == "Light") %>%
    pull(path)
  
  assign("mabry_light", mabry_light_path, envir = .GlobalEnv)
  systemfonts::register_font(name = "Mabry Pro Light", plain = mabry_light_path)
  
  mabry_med_path <- system_fonts() %>%
    filter(family == "Mabry Pro", style == "Medium") %>%
    pull(path)
  
  assign("mabry_med", mabry_med_path, envir = .GlobalEnv)
  systemfonts::register_font(name = "Mabry Pro Medium", plain = mabry_med_path)
}
