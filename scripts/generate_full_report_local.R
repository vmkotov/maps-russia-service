#!/usr/bin/env Rscript

# ============================================================
# ЛОКАЛЬНАЯ ГЕНЕРАЦИЯ ПОЛНОГО PDF-ОТЧЁТА ПО JSON
# ============================================================
# Использование:
#   Rscript scripts/generate_full_report_local.R <json_file>
# Пример:
#   Rscript scripts/generate_full_report_local.R data/input/user_2_full.json
# ============================================================

Sys.setlocale("LC_ALL", "C.UTF-8")

library(sf)
library(ggplot2)
library(jsonlite)
library(showtext)
library(sysfonts)

# ---- Шрифт ----
font_paths <- c(
  "/System/Library/Fonts/Helvetica.ttc",
  "/System/Library/Fonts/Arial.ttf",
  "/Library/Fonts/Arial.ttf",
  "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
  "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
)
found <- FALSE
for (f in font_paths) {
  if (file.exists(f)) {
    font_add("liberation", regular = f)
    cat("✅ Шрифт найден:", f, "\n")
    found <- TRUE
    break
  }
}
if (!found) {
  stop("❌ Не найден ни один шрифт с поддержкой кириллицы.")
}
showtext_auto()
cat("🔤 showtext активирован.\n")

# ---- Загрузка данных ----
load_data <- function() {
  cat("load_data(): начало\n")
  required_files <- c(
    "data/rds/combined_split.rds",
    "data/rds/russian_rivers.rds",
    "data/rds/selected_lakes.rds",
    "data/rds/azov_sea.rds"
  )
  for (f in required_files) {
    if (!file.exists(f)) {
      stop("Не найден файл: ", f)
    }
  }
  cat("Все файлы найдены. Загружаем...\n")
  data <- list(
    combined = readRDS("data/rds/combined_split.rds"),
    rivers = readRDS("data/rds/russian_rivers.rds"),
    selected_lakes = readRDS("data/rds/selected_lakes.rds"),
    azov = readRDS("data/rds/azov_sea.rds")
  )
  cat("Данные загружены успешно\n")
  return(data)
}

# ---- Функции ----
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

  dist_dict <- setNames(regions_df$district_name, regions_df$region_name_en)
  combined$district_visited <- NA_character_
  for (i in 1:nrow(combined)) {
    name <- combined$name_en[i]
    if (name %in% names(dist_dict)) {
      combined$district_visited[i] <- dist_dict[name]
    }
  }

  all_districts <- c(
    "Дальневосточный", "Приволжский", "Северо-Западный",
    "Северо-Кавказский", "Сибирский", "Уральский",
    "Центральный", "Южный"
  )

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

  combined_visited <- combined
  combined_visited$district_visited <- factor(combined_visited$district_visited, levels = all_districts)

  p <- ggplot() +
    geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = "#E8E8E8") +
    geom_sf(data = combined_visited[!is.na(combined_visited$district_visited), ],
            color = NA, size = 0, aes(fill = district_visited)) +
    scale_fill_manual(
      values = district_colors,
      name = "Федеральный округ",
      drop = FALSE,
      na.translate = FALSE
    ) +
    geom_sf(data = rivers, color = "#00BFFF", size = 0.5, fill = NA) +
    geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = NA) +
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

generate_main_map <- function(json_data) {
  cat("generate_main_map(): начало\n")
  regions_df <- data.frame(
    region_name_en = character(),
    district_name = character(),
    stringsAsFactors = FALSE
  )
  for (dist in json_data$districts) {
    dist_name <- dist$district_name
    for (reg in dist$regions) {
      region_visited <- if (!is.null(reg$region_visited)) reg$region_visited else FALSE
      if (region_visited) {
        regions_df <- rbind(regions_df, data.frame(
          region_name_en = reg$region_name_en,
          district_name = dist_name,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  if (nrow(regions_df) == 0) {
    data_env <- load_data()
    combined <- data_env$combined
    combined <- combined[!duplicated(combined$name_en), ]
    p <- ggplot() +
      geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = "#E8E8E8") +
      coord_sf() +
      theme_void() +
      labs(title = "Нет посещённых регионов")
    return(p)
  }
  temp_json <- list(
    client_id = json_data$client_id,
    first_name = json_data$first_name,
    last_name = json_data$last_name,
    regions = regions_df
  )
  data_env <- load_data()
  p <- generate_map_from_regions(data_env, temp_json, output_file = NULL)
  return(p)
}

generate_cities_map_pdf <- function(json_data) {
  data_env <- load_data()
  combined <- data_env$combined
  rivers <- data_env$rivers
  lakes <- data_env$selected_lakes
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
    p <- ggplot() +
      geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = "#E8E8E8") +
      coord_sf() +
      theme_void() +
      labs(title = "Посещённые города (нет данных)")
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
    theme(plot.background = element_rect(fill = "white", color = NA)) +
    labs(title = "Посещённые города")
  return(p)
}

generate_region_pages_pdf <- function(json_data, combined) {
  cat("generate_region_pages_pdf(): начало\n")
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
          visited = isTRUE(city$visited),
          population = if (!is.null(city$population)) as.numeric(city$population) else 0
        )
      }
      cities_dict[[reg_en]] <- cities_list
    }
  }

  plots <- list()
  for (i in 1:nrow(combined)) {
    if (i %% 10 == 0) cat(sprintf("   Генерация страницы региона %d из %d\n", i, nrow(combined)))
    region_poly <- combined[i, ]
    region_name <- region_poly$name
    region_name_en <- region_poly$name_en

    region_cities_raw <- cities_dict[[region_name_en]]
    if (is.null(region_cities_raw) || length(region_cities_raw) == 0) {
      p <- ggplot() +
        geom_sf(data = region_poly, fill = "#E8E8E8", color = "#2E4053", size = 0.5) +
        coord_sf() +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "liberation")) +
        labs(title = region_name)
      plots[[i]] <- p
      next
    }

    region_cities <- data.frame(
      city_name = sapply(region_cities_raw, function(x) x$city_name),
      lat = sapply(region_cities_raw, function(x) x$lat),
      lon = sapply(region_cities_raw, function(x) x$lon),
      visited = sapply(region_cities_raw, function(x) x$visited),
      population = sapply(region_cities_raw, function(x) x$population),
      stringsAsFactors = FALSE
    )

    region_cities$size_group <- cut(region_cities$population,
                                    breaks = c(-Inf, 50000, 100000, 500000, 1000000, Inf),
                                    labels = c("до 50k", "50-100k", "100-500k", "500k-1m", ">1m"),
                                    include.lowest = TRUE)
    size_map <- c("до 50k" = 1, "50-100k" = 1.5, "100-500k" = 2, "500k-1m" = 2.5, ">1m" = 3)
    region_cities$size <- size_map[as.character(region_cities$size_group)]

    p <- ggplot() +
      geom_sf(data = region_poly, fill = "#E8E8E8", color = "#2E4053", size = 0.5) +
      geom_point(data = region_cities[region_cities$visited, ],
                 aes(x = lon, y = lat, size = size),
                 color = "red", fill = "red", shape = 16) +
      geom_point(data = region_cities[!region_cities$visited, ],
                 aes(x = lon, y = lat, size = size),
                 color = "gray50", fill = NA, shape = 1) +
      geom_text(data = region_cities,
                aes(x = lon, y = lat, label = city_name, color = visited),
                size = 2.5, hjust = 0, vjust = 1, check_overlap = TRUE, family = "liberation") +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray50")) +
      scale_size_identity() +
      coord_sf() +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "liberation"),
            legend.position = "none") +
      labs(title = region_name)

    plots[[i]] <- p
  }
  cat("Страницы регионов готовы.\n")
  return(plots)
}

# ---- Основная функция ----
generate_full_report <- function(json_file) {
  cat("🚀 Генерация PDF-отчёта из JSON:", json_file, "\n")
  json_data <- fromJSON(json_file, simplifyVector = FALSE)

  required <- c("client_id", "first_name", "last_name", "districts")
  for (f in required) {
    if (!f %in% names(json_data)) {
      stop(sprintf("❌ В JSON отсутствует поле '%s'", f))
    }
  }

  data_env <- load_data()
  combined <- data_env$combined
  combined <- combined[!duplicated(combined$name_en), ]

  p_main <- generate_main_map(json_data)
  p_cities <- generate_cities_map_pdf(json_data)
  region_plots <- generate_region_pages_pdf(json_data, combined)

  output_file <- "output/report_local.pdf"
  dir.create("output", recursive = TRUE, showWarnings = FALSE)
  pdf(output_file, width = 12, height = 10)

  print(p_main)
  print(p_cities)
  for (p in region_plots) {
    print(p)
  }
  dev.off()

  cat("✅ PDF сохранён в", output_file, "\n")
}

# ---- Запуск ----
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Укажите путь к JSON-файлу:\n")
    cat("  Rscript scripts/generate_full_report_local.R <json_file>\n")
    quit(status = 1)
  }
  json_file <- args[1]
  generate_full_report(json_file)
}
