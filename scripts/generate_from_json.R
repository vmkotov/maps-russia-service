#!/usr/bin/env Rscript

# ============================================================
# ГЕНЕРАЦИЯ КАРТЫ ПО JSON-ФАЙЛУ
# ============================================================
# Использование:
#   Rscript scripts/generate_from_json.R <json_file>
# Пример:
#   Rscript scripts/generate_from_json.R data/input/user_5_1001.json
#
# Результат:
#   - output/plots/map_request_<request_id>.png (или client_id)
# ============================================================

library(sf)
library(ggplot2)
library(jsonlite)

# ---- Загрузка RDS-данных ----
load_data <- function() {
  required_files <- c(
    "data/rds/combined_split.rds",
    "data/rds/russian_rivers.rds",
    "data/rds/selected_lakes.rds",
    "data/rds/azov_sea.rds"
  )
  for (f in required_files) {
    if (!file.exists(f)) {
      stop("Не найден файл: ", f, ". Сначала создайте его с помощью setup.R или проверьте путь.")
    }
  }
  list(
    combined = readRDS("data/rds/combined_split.rds"),
    rivers = readRDS("data/rds/russian_rivers.rds"),
    selected_lakes = readRDS("data/rds/selected_lakes.rds"),
    azov = readRDS("data/rds/azov_sea.rds")
  )
}

# ---- Функция построения карты ----
generate_map_from_regions <- function(data_env, json_data, output_file = NULL) {
  combined <- data_env$combined
  rivers <- data_env$rivers
  selected_lakes <- data_env$selected_lakes
  azov <- data_env$azov
  
  regions_df <- json_data$regions
  if (is.null(regions_df) || nrow(regions_df) == 0) {
    stop("В JSON нет поля 'regions' или оно пустое.")
  }
  
  required_cols <- c("region_name_en", "district_name")
  if (!all(required_cols %in% colnames(regions_df))) {
    stop("В JSON в поле 'regions' отсутствуют колонки: ",
         paste(setdiff(required_cols, colnames(regions_df)), collapse=", "))
  }
  
  # Словарь: регион -> округ
  dist_dict <- setNames(regions_df$district_name, regions_df$region_name_en)
  
  # Добавляем колонку округа в combined (оставляем NA для непосещённых)
  combined$district_visited <- NA_character_
  for (i in 1:nrow(combined)) {
    name <- combined$name_en[i]
    if (name %in% names(dist_dict)) {
      combined$district_visited[i] <- dist_dict[name]
    }
  }
  
  # Преобразуем в фактор с уровнями всех федеральных округов
  all_districts <- c(
    "Дальневосточный", "Приволжский", "Северо-Западный",
    "Северо-Кавказский", "Сибирский", "Уральский",
    "Центральный", "Южный"
  )
  combined$district_visited <- factor(combined$district_visited, levels = all_districts)
  
  # Палитра цветов (полная)
  district_colors <- c(
    "Дальневосточный"      = "#E63946",
    "Приволжский"          = "#F4A261",
    "Северо-Западный"      = "#2A9D8F",
    "Северо-Кавказский"    = "#9B5DE5",
    "Сибирский"            = "#F9C74F",
    "Уральский"            = "#F4845F",
    "Центральный"          = "#B5838D",
    "Южный"                = "#E5989B"
  )
  
  full_name <- paste(json_data$first_name, json_data$last_name)
  title <- paste("Посещённые регионы –", full_name)
  
  p <- ggplot() +
    # 1. Заливка регионов
    geom_sf(data = combined, color = NA, size = 0, aes(fill = district_visited)) +
    scale_fill_manual(
      values = district_colors,
      na.value = "#E8E8E8",
      name = "Федеральный округ",
      drop = FALSE,
      na.translate = FALSE
    ) +
    # 2. Реки (под контурами)
    geom_sf(data = rivers, color = "#00BFFF", size = 0.5, fill = NA) +
    # 3. Контуры регионов
    geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = NA) +
    # 4. Озёра и море (поверх всего)
    geom_sf(data = selected_lakes, fill = "#00BFFF", color = "#00BFFF", size = 0.2, alpha = 1) +
    geom_sf(data = azov, fill = "#00BFFF", color = "#00BFFF", size = 0.2, alpha = 0.7) +
    coord_sf() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    ) +
    labs(title = NULL)
  
  if (is.null(output_file)) {
    return(p)
  }
  
  ggsave(output_file, p, width = 12, height = 10, dpi = 300)
  cat("Карта сохранена в", output_file, "\n")
  return(output_file)
}

# ---- Основная функция ----
generate_from_json <- function(json_file, output_dir = "output/plots") {
  if (!file.exists(json_file)) {
    stop("Файл не найден: ", json_file)
  }
  json_data <- fromJSON(json_file)
  
  required_fields <- c("client_id", "first_name", "last_name", "regions")
  for (f in required_fields) {
    if (!f %in% names(json_data)) {
      stop("В JSON отсутствует поле: ", f)
    }
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!is.null(json_data$request_id)) {
    output_file <- file.path(output_dir, paste0("map_request_", json_data$request_id, ".png"))
  } else {
    output_file <- file.path(output_dir, paste0(json_data$client_id, "_map.png"))
  }
  
  data_env <- load_data()
  generate_map_from_regions(data_env, json_data, output_file)
  return(output_file)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Укажите путь к JSON-файлу:\n")
    cat("  Rscript scripts/generate_from_json.R <json_file>\n")
    quit(status = 1)
  }
  json_file <- args[1]
  generate_from_json(json_file)
}