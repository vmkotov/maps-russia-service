#!/usr/bin/env Rscript

# ============================================================
# ИТОГОВЫЙ СКРИПТ ДЛЯ ГЕНЕРАЦИИ ОТЧЁТА И КАРТЫ ПО ПОЛЬЗОВАТЕЛЮ
# ============================================================
# Использование:
#   Rscript scripts/generate_report.R <user_id>
# Пример:
#   Rscript scripts/generate_report.R 4
#
# Результат:
#   - output/reports/user_<id>_report.txt  (текстовый отчёт)
#   - output/plots/user_<id>_map.png       (карта)
# ============================================================

# ---- 1. Подключение библиотек ----
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(jsonlite)   # на всякий случай

# ---- 2. Конфигурация ----
DB_CONFIG <- list(
  host     = "aws-1-eu-north-1.pooler.supabase.com",
  port     = 5432,
  dbname   = "postgres",
  user     = "postgres.twmbrcbnofrkntblmqkm",
  password = "9OBLWWCCiiIP0m8d",
  sslmode  = "require"
)

# ---- 3. Функция для получения данных о пользователе ----
get_user_data <- function(user_id) {
  con <- dbConnect(
    RPostgres::Postgres(),
    host     = DB_CONFIG$host,
    port     = DB_CONFIG$port,
    dbname   = DB_CONFIG$dbname,
    user     = DB_CONFIG$user,
    password = DB_CONFIG$password,
    sslmode  = DB_CONFIG$sslmode
  )
  on.exit(dbDisconnect(con))
  
  # Получаем имя пользователя
  user_info <- dbGetQuery(con, 
                          "SELECT first_name, last_name FROM vkotov_russian_city_bot.dim_user WHERE id = $1", 
                          params = list(user_id)
  )
  if (nrow(user_info) == 0) {
    stop("Пользователь с ID ", user_id, " не найден.")
  }
  full_name <- paste(user_info$first_name, user_info$last_name)
  
  # Получаем список посещённых регионов (активные, dt_end IS NULL)
  query <- "
    SELECT 
      region_name_ru,
      region_name_en,
      district_name,
      dt_begin
    FROM vkotov_russian_city_bot.v_user_visited_regions_full
    WHERE user_id = $1
    ORDER BY district_name, region_name_ru;
  "
  df <- dbGetQuery(con, query, params = list(user_id))
  
  return(list(
    user_id = user_id,
    full_name = full_name,
    regions = df
  ))
}

# ---- 4. Функция для создания текстового отчёта ----
generate_text_report <- function(user_data, output_file) {
  df <- user_data$regions
  if (nrow(df) == 0) {
    report <- paste("Пользователь", user_data$full_name, "(ID:", user_data$user_id, ") не посетил ни одного региона.")
    writeLines(report, output_file)
    return(report)
  }
  
  # Строим отчёт
  lines <- c(
    paste("Пользователь", user_data$full_name, "(ID:", user_data$user_id, ") посетил следующие регионы:"),
    ""
  )
  
  districts <- unique(df$district_name)
  for (d in districts) {
    lines <- c(lines, paste("=== Федеральный округ:", d, "==="))
    region_rows <- df[df$district_name == d, ]
    for (i in 1:nrow(region_rows)) {
      lines <- c(lines, paste0("  - ", region_rows$region_name_ru[i], 
                               " (с ", region_rows$dt_begin[i], ")"))
    }
    lines <- c(lines, "")
  }
  
  writeLines(lines, output_file)
  return(paste(lines, collapse = "\n"))
}

# ---- 5. Функция для построения карты ----
generate_map <- function(user_data, output_file) {
  required_files <- c(
    "data/rds/combined.rds",
    "data/rds/enissey.rds",
    "data/rds/selected_lakes.rds"
  )
  for (f in required_files) {
    if (!file.exists(f)) {
      stop("Не найден файл: ", f, ". Сначала создайте его с помощью setup.R.")
    }
  }
  
  combined <- readRDS("data/rds/combined.rds")
  enissey <- readRDS("data/rds/enissey.rds")
  selected_lakes <- readRDS("data/rds/selected_lakes.rds")
  
  dist_dict <- setNames(user_data$regions$district_name, user_data$regions$region_name_en)
  combined$district_visited <- NA_character_
  for (i in 1:nrow(combined)) {
    name <- combined$name_en[i]
    if (name %in% names(dist_dict)) {
      combined$district_visited[i] <- dist_dict[name]
    }
  }
  
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
  
  districts_present <- unique(user_data$regions$district_name)
  colors_used <- district_colors[names(district_colors) %in% districts_present]
  
  p <- ggplot() +
    geom_sf(data = combined, color = "#2E4053", size = 0.3, aes(fill = district_visited)) +
    scale_fill_manual(
      values = colors_used,
      na.value = "#E8E8E8",   # здесь изменён цвет непосещённых регионов
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
    labs(title = paste("Посещённые регионы –", user_data$full_name))
  
  ggsave(output_file, p, width = 12, height = 10, dpi = 300)
  return(output_file)
}

# ---- 6. Основная функция, которая всё запускает ----
generate_user_report <- function(user_id, output_dir = "output") {
  # Создаём папки, если их нет
  dir.create(file.path(output_dir, "reports"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
  
  # Получаем данные
  cat("Загрузка данных для пользователя", user_id, "...\n")
  user_data <- get_user_data(user_id)
  
  # Генерируем текстовый отчёт
  report_file <- file.path(output_dir, "reports", paste0("user_", user_id, "_report.txt"))
  cat("Создание текстового отчёта:", report_file, "\n")
  generate_text_report(user_data, report_file)
  
  # Генерируем карту
  map_file <- file.path(output_dir, "plots", paste0("user_", user_id, "_map.png"))
  cat("Построение карты:", map_file, "\n")
  generate_map(user_data, map_file)
  
  # Итоговое сообщение
  cat("\nГотово!\n")
  cat("Отчёт сохранён в:", report_file, "\n")
  cat("Карта сохранена в:", map_file, "\n")
}

# ---- 7. Запуск, если скрипт вызывается из командной строки ----
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Укажите ID пользователя:\n")
    cat("  Rscript scripts/generate_report.R <user_id>\n")
    quit(status = 1)
  }
  user_id <- as.integer(args[1])
  if (is.na(user_id)) {
    cat("ID пользователя должен быть числом.\n")
    quit(status = 1)
  }
  generate_user_report(user_id)
}