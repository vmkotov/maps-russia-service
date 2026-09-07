#!/usr/bin/env Rscript

# ============================================================
# ГЕНЕРАЦИЯ ПОЛНОГО PDF-ОТЧЁТА ИЗ JSON
# ============================================================
# Использование:
#   Rscript scripts/generate_full_report_from_json.R <json_file>
# Пример:
#   Rscript scripts/generate_full_report_from_json.R data/input/user_1_full.json
#
# Результат:
#   - output/report_user_<id>.pdf
# ============================================================

library(sf)
library(ggplot2)
library(jsonlite)
library(ggrepel)
library(showtext)

# ---- Подключаем системный шрифт для кириллицы ----
# Добавляем новое семейство "myfont"
font_add("myfont", regular = "/System/Library/Fonts/Helvetica.ttc")
showtext_auto()
cat("🔤 Шрифты для кириллицы настроены.\n")

# ---- 1. Загрузка пространственных данных (RDS) ----
load_data <- function() {
  cat("📂 Загрузка RDS-файлов...\n")
  list(
    combined = readRDS("data/rds/combined_split.rds"),
    rivers = readRDS("data/rds/russian_rivers.rds"),
    lakes = readRDS("data/rds/selected_lakes.rds"),
    azov = readRDS("data/rds/azov_sea.rds")
  )
}

# ---- 2. Страница 1: карта с заливкой по федеральным округам ----
generate_main_map <- function(json_data) {
  cat("🖼️ Генерация страницы 1: карта округов...\n")
  
  regions_df <- data.frame(
    region_name_en = character(),
    district_name = character(),
    stringsAsFactors = FALSE
  )
  
  for (dist in json_data$districts) {
    dist_name <- dist$district_name
    for (reg in dist$regions) {
      regions_df <- rbind(regions_df, data.frame(
        region_name_en = reg$region_name_en,
        district_name = dist_name,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  temp_json <- list(
    client_id = json_data$client_id,
    first_name = json_data$first_name,
    last_name = json_data$last_name,
    regions = regions_df
  )
  
  source("scripts/generate_from_json.R", local = TRUE)
  data_env <- load_data()
  p <- generate_map_from_regions(data_env, temp_json, output_file = NULL)
  
  # Применяем шрифт к тексту
  p <- p + theme(
    text = element_text(family = "myfont"),
    plot.title = element_text(family = "myfont", hjust = 0.5, face = "bold", size = 14)
  )
  return(p)
}

# ---- 3. Страница 2: карта с посещёнными городами (серый фон) ----
generate_cities_map <- function(json_data) {
  cat("🖼️ Генерация страницы 2: карта городов...\n")
  data_env <- load_data()
  combined <- data_env$combined
  rivers <- data_env$rivers
  lakes <- data_env$lakes
  azov <- data_env$azov
  combined <- combined[!duplicated(combined$name_en), ]
  
  cities_df <- data.frame(lat = numeric(), lon = numeric())
  
  for (dist in json_data$districts) {
    for (reg in dist$regions) {
      for (city in reg$cities) {
        if (isTRUE(city$visited)) {
          cities_df <- rbind(cities_df, data.frame(
            lat = city$lat,
            lon = city$lon
          ))
        }
      }
    }
  }
  
  if (nrow(cities_df) == 0) {
    cat("⚠️ Нет посещённых городов для отображения на карте.\n")
    p <- ggplot() +
      geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = "#E8E8E8") +
      coord_sf() +
      theme_void() +
      labs(title = "Посещённые города (нет данных)") +
      theme(text = element_text(family = "myfont"))
    return(p)
  }
  
  p <- ggplot() +
    geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = "#E8E8E8") +
    geom_sf(data = rivers, color = "#00BFFF", size = 0.4, fill = NA) +
    geom_sf(data = lakes, fill = "#00BFFF", color = "#00BFFF", size = 0.2, alpha = 1) +
    geom_sf(data = azov, fill = "#00BFFF", color = "#00BFFF", size = 0.2, alpha = 0.7) +
    geom_point(data = cities_df, aes(x = lon, y = lat), color = "red", size = 0.8, alpha = 0.7) +
    coord_sf() +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          text = element_text(family = "myfont"),
          plot.title = element_text(family = "myfont", hjust = 0.5, face = "bold", size = 14)) +
    labs(title = "Посещённые города")
  return(p)
}

# ---- 4. Страницы регионов (каждый регион на отдельной странице) ----
generate_region_pages <- function(json_data, combined) {
  cat("🖼️ Генерация страниц регионов (всего", nrow(combined), "регионов)...\n")
  
  combined <- combined[order(combined$name), ]
  
  cities_dict <- list()
  for (dist in json_data$districts) {
    for (reg in dist$regions) {
      reg_en <- reg$region_name_en
      cities_list <- list()
      for (city in reg$cities) {
        cities_list[[length(cities_list)+1]] <- list(
          city_name = city$city_name,
          lat = city$lat,
          lon = city$lon,
          visited = isTRUE(city$visited)
        )
      }
      cities_dict[[reg_en]] <- cities_list
    }
  }
  
  plots <- list()
  for (i in 1:nrow(combined)) {
    region_poly <- combined[i, ]
    region_name <- region_poly$name
    region_name_en <- region_poly$name_en
    
    region_cities_raw <- cities_dict[[region_name_en]]
    if (is.null(region_cities_raw) || length(region_cities_raw) == 0) {
      p <- ggplot() +
        geom_sf(data = region_poly, fill = "#E8E8E8", color = "#2E4053", size = 0.5) +
        coord_sf() +
        theme_void() +
        labs(title = region_name) +
        theme(text = element_text(family = "myfont"),
              plot.title = element_text(family = "myfont", hjust = 0.5, face = "bold", size = 12))
      plots[[i]] <- p
      next
    }
    
    region_cities <- data.frame(
      city_name = sapply(region_cities_raw, function(x) x$city_name),
      lat = sapply(region_cities_raw, function(x) x$lat),
      lon = sapply(region_cities_raw, function(x) x$lon),
      visited = sapply(region_cities_raw, function(x) x$visited),
      stringsAsFactors = FALSE
    )
    
    p <- ggplot() +
      geom_sf(data = region_poly, fill = "#E8E8E8", color = "#2E4053", size = 0.5) +
      geom_point(data = region_cities[region_cities$visited, ], 
                 aes(x = lon, y = lat), color = "red", shape = 16, size = 2) +
      geom_point(data = region_cities[!region_cities$visited, ], 
                 aes(x = lon, y = lat), color = "gray50", shape = 1, size = 2) +
      geom_text_repel(data = region_cities, 
                      aes(x = lon, y = lat, label = city_name, color = visited),
                      size = 2.5, box.padding = 0.3, point.padding = 0.2,
                      segment.color = NA, max.overlaps = 10,
                      family = "myfont") +  # явно задаём шрифт для ggrepel
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray50")) +
      coord_sf() +
      theme_void() +
      theme(plot.title = element_text(family = "myfont", hjust = 0.5, face = "bold", size = 12),
            legend.position = "none",
            text = element_text(family = "myfont")) +
      labs(title = region_name)
    
    plots[[i]] <- p
    if (i %% 10 == 0) cat("   Обработано", i, "из", nrow(combined), "\n")
  }
  cat("✅ Страницы регионов готовы.\n")
  return(plots)
}

# ---- 5. Основная функция ----
generate_full_report_from_json <- function(json_file) {
  cat("🚀 Начало генерации PDF-отчёта из JSON\n")
  
  json_data <- fromJSON(json_file, simplifyVector = FALSE)
  
  required <- c("client_id", "first_name", "last_name", "districts")
  for (f in required) {
    if (!f %in% names(json_data)) {
      stop(sprintf("❌ В JSON отсутствует поле '%s'", f))
    }
  }
  if (length(json_data$districts) == 0) {
    stop("❌ Поле 'districts' пустое.")
  }
  cat("✅ JSON загружен. Клиент:", json_data$client_id, "\n")
  cat("   Пользователь:", json_data$first_name, json_data$last_name, "\n")
  
  data_env <- load_data()
  combined <- data_env$combined
  combined <- combined[!duplicated(combined$name_en), ]
  cat("✅ Загружено полигонов регионов:", nrow(combined), "\n")
  
  p_main <- generate_main_map(json_data)
  p_cities <- generate_cities_map(json_data)
  region_plots <- generate_region_pages(json_data, combined)
  
  output_dir <- "output"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_file <- file.path(output_dir, paste0("report_user_", json_data$client_id, ".pdf"))
  cat("💾 Сохранение PDF:", pdf_file, "\n")
  
  pdf(pdf_file, width = 12, height = 10, family = "sans")  # используем стандартное семейство, showtext его перехватит
  
  print(p_main)
  print(p_cities)
  for (p in region_plots) {
    print(p)
  }
  
  dev.off()
  cat("✅ PDF успешно сохранён в", pdf_file, "\n")
  cat("📊 Всего страниц:", 2 + length(region_plots), "\n")
  cat("🎉 Генерация отчёта завершена!\n")
}

# ---- 6. Запуск из командной строки ----
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Укажите путь к JSON-файлу:\n")
    cat("  Rscript scripts/generate_full_report_from_json.R <json_file>\n")
    quit(status = 1)
  }
  json_file <- args[1]
  generate_full_report_from_json(json_file)
}
