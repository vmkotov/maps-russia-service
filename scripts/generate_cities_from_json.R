#!/usr/bin/env Rscript

# ============================================================
# ГЕНЕРАЦИЯ КАРТЫ С ГОРОДАМИ ПО JSON
# ============================================================
# Использование:
#   Rscript scripts/generate_cities_from_json.R <json_file>
# Пример:
#   Rscript scripts/generate_cities_from_json.R data/input/user_1_cities.json
#
# Результат:
#   - output/plots/cities_map.png
# ============================================================

library(sf)
library(ggplot2)
library(jsonlite)

load_data_cities <- function() {
  list(
    combined = readRDS("data/rds/combined_split.rds"),
    rivers = readRDS("data/rds/russian_rivers.rds"),
    lakes = readRDS("data/rds/selected_lakes.rds"),
    azov = readRDS("data/rds/azov_sea.rds")
  )
}

generate_cities_map <- function(json_data, output_file = NULL) {
  data_env <- load_data_cities()
  combined <- data_env$combined
  rivers <- data_env$rivers
  lakes <- data_env$lakes
  azov <- data_env$azov
  
  cities_df <- json_data$cities
  if (is.null(cities_df) || nrow(cities_df) == 0) {
    stop("В JSON нет поля 'cities' или оно пустое.")
  }
  required_cols <- c("lat", "lon")
  if (!all(required_cols %in% colnames(cities_df))) {
    stop("В поле 'cities' отсутствуют колонки lat или lon")
  }
  
  combined <- combined[!duplicated(combined$name_en), ]
  
  p <- ggplot() +
    geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = "#E8E8E8") +
    geom_sf(data = rivers, color = "#00BFFF", size = 0.4, fill = NA) +
    geom_sf(data = lakes, fill = "#00BFFF", color = "#00BFFF", size = 0.2, alpha = 1) +
    geom_sf(data = azov, fill = "#00BFFF", color = "#00BFFF", size = 0.2, alpha = 0.7) +
    geom_point(data = cities_df, aes(x = lon, y = lat), color = "red", size = 0.5, alpha = 0.6) +
    coord_sf() +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA)) +
    labs(title = "Посещённые города")
  
  if (!is.null(output_file)) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    ggsave(output_file, p, width = 12, height = 10, dpi = 300)
    cat("Карта с городами сохранена в", output_file, "\n")
  }
  return(p)
}

generate_from_json_cities <- function(json_file, output_dir = "output/plots") {
  if (!file.exists(json_file)) {
    stop("Файл не найден: ", json_file)
  }
  json_data <- fromJSON(json_file)
  
  if (!"cities" %in% names(json_data)) {
    stop("В JSON отсутствует поле 'cities'")
  }
  
  output_file <- file.path(output_dir, "cities_map.png")
  generate_cities_map(json_data, output_file)
  return(output_file)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Укажите путь к JSON-файлу:\n")
    cat("  Rscript scripts/generate_cities_from_json.R <json_file>\n")
    quit(status = 1)
  }
  json_file <- args[1]
  generate_from_json_cities(json_file)
}
