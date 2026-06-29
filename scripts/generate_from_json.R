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
    "data/rds/combined.rds",
    "data/rds/enissey.rds",
    "data/rds/selected_lakes.rds"
  )
  for (f in required_files) {
    if (!file.exists(f)) {
      stop("Не найден файл: ", f, ". Сначала создайте его с помощью setup.R или проверьте путь.")
    }
  }
  list(
    combined = readRDS("data/rds/combined.rds"),
    enissey = readRDS("data/rds/enissey.rds"),
    selected_lakes = readRDS("data/rds/selected_lakes.rds")
  )
}

# ---- Функция построения карты ----
generate_map_from_regions <- function(data_env, json_data, output_file = NULL) {
  combined <- data_env$combined
  enissey <- data_env$enissey
  selected_lakes <- data_env$selected_lakes
  
  # Извлекаем регионы из JSON
  regions_df <- json_data$regions
  if (is.null(regions_df) || nrow(regions_df) == 0) {
    stop("В JSON нет поля 'regions' или оно пустое.")
  }
  
  # Проверяем наличие нужных колонок
  required_cols <- c("region_name_en", "district_name")
  if (!all(required_cols %in% colnames(regions_df))) {
    stop("В JSON в поле 'regions' отсутствуют колонки: ", 
         paste(setdiff(required_cols, colnames(regions_df)), collapse=", "))
  }
  
  # Создаём словарь: region_name_en -> district_name
  dist_dict <- setNames(regions_df$district_name, regions_df$region_name_en)
  
  # Добавляем колонку district_visited в combined
  combined$district_visited <- NA_character_
  for (i in 1:nrow(combined)) {
    name <- combined$name_en[i]
    if (name %in% names(dist_dict)) {
      combined$district_visited[i] <- dist_dict[name]
    }
  }
  
  # Палитра цветов для округов
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
  
  # Используем только те округа, которые есть в данных
  districts_present <- unique(regions_df$district_name)
  colors_used <- district_colors[names(district_colors) %in% districts_present]
  
  # Если каких-то округов нет в палитре, добавляем дефолтный цвет
  default_color <- "#A9A9A9"  # серый
  missing_districts <- setdiff(districts_present, names(colors_used))
  if (length(missing_districts) > 0) {
    extra_colors <- setNames(rep(default_color, length(missing_districts)), missing_districts)
    colors_used <- c(colors_used, extra_colors)
  }
  
  # Заголовок
  full_name <- paste(json_data$first_name, json_data$last_name)
  title <- paste("Посещённые регионы –", full_name)
  
  # Построение карты
  p <- ggplot() +
    geom_sf(data = combined, color = "#2E4053", size = 0.3, aes(fill = district_visited)) +
    scale_fill_manual(
      values = colors_used,
      na.value = "#E8E8E8",
      name = "Федеральный округ"
    ) +
    geom_sf(data = enissey, color = "dodgerblue", size = 0.4, fill = NA) +
    geom_sf(data = selected_lakes, fill = "dodgerblue", color = "dodgerblue", size = 0.2, alpha = 0.7) +
    coord_sf() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    ) +
    labs(title = title)
  
  # Если output_file не передан, возвращаем ggplot
  if (is.null(output_file)) {
    return(p)
  }
  
  # Иначе сохраняем PNG
  ggsave(output_file, p, width = 12, height = 10, dpi = 300)
  cat("Карта сохранена в", output_file, "\n")
  return(output_file)
}

# ---- Основная функция (для запуска из командной строки) ----
generate_from_json <- function(json_file, output_dir = "output/plots") {
  # Чтение JSON
  if (!file.exists(json_file)) {
    stop("Файл не найден: ", json_file)
  }
  json_data <- fromJSON(json_file)
  
  # Проверка обязательных полей
  required_fields <- c("client_id", "first_name", "last_name", "regions")
  for (f in required_fields) {
    if (!f %in% names(json_data)) {
      stop("В JSON отсутствует поле: ", f)
    }
  }
  
  # Создаём папку для выходных файлов
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Определяем имя выходного файла
  if (!is.null(json_data$request_id)) {
    output_file <- file.path(output_dir, paste0("map_request_", json_data$request_id, ".png"))
  } else {
    output_file <- file.path(output_dir, paste0(json_data$client_id, "_map.png"))
  }
  
  # Загружаем данные
  data_env <- load_data()
  
  # Строим карту (сохраняем)
  generate_map_from_regions(data_env, json_data, output_file)
  
  return(output_file)
}

# ---- Запуск из командной строки ----
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