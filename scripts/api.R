# scripts/api.R
# Plumber API с поддержкой PNG и PDF (с автоопределением шрифта для кириллицы)

Sys.setlocale("LC_ALL", "C.UTF-8")

library(plumber)
library(sf)
library(ggplot2)
library(jsonlite)
library(showtext)
library(sysfonts)

setwd("/app")
cat("Working directory set to:", getwd(), "\n")
cat("Files in /app/data/rds/:", list.files("/app/data/rds/"), "\n")

# ---- Автоопределение шрифта для кириллицы ----
find_cyrillic_font <- function() {
  # Список возможных путей к шрифтам с поддержкой кириллицы
  candidates <- c(
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
    "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf",
    "/usr/share/fonts/truetype/ubuntu/Ubuntu-R.ttf"
  )
  for (path in candidates) {
    if (file.exists(path)) {
      cat("✅ Найден шрифт:", path, "\n")
      return(path)
    }
  }
  # Если ничего не найдено, пробуем системный sans
  cat("⚠️ Шрифт не найден, используем системный 'sans'\n")
  return(NULL)
}

font_path <- find_cyrillic_font()
if (!is.null(font_path)) {
  font_add("cyr", regular = font_path)
} else {
  # Если путь не найден, надеемся, что системный sans поддерживает кириллицу
  font_add("cyr", family = "sans")
}
showtext_auto()
cat("🔤 Шрифт для кириллицы зарегистрирован.\n")

# ---- Остальной код (без изменений) ----
load_data <- function() {
  cat("load_data(): начало\n")
  required_files <- c(
    "/app/data/rds/combined_split.rds",
    "/app/data/rds/russian_rivers.rds",
    "/app/data/rds/selected_lakes.rds",
    "/app/data/rds/azov_sea.rds"
  )
  for (f in required_files) {
    cat("Проверка файла:", f, " - ", file.exists(f), "\n")
    if (!file.exists(f)) {
      stop("Не найден файл: ", f)
    }
  }
  cat("Все файлы найдены. Загружаем...\n")
  data <- list(
    combined = readRDS("/app/data/rds/combined_split.rds"),
    rivers = readRDS("/app/data/rds/russian_rivers.rds"),
    selected_lakes = readRDS("/app/data/rds/selected_lakes.rds"),
    azov = readRDS("/app/data/rds/azov_sea.rds")
  )
  cat("Данные загружены успешно\n")
  return(data)
}

generate_map_from_regions <- function(data_env, json_data, output_file = NULL) {
  cat("generate_map_from_regions(): начало\n")
  combined <- data_env$combined
  rivers <- data_env$rivers
  selected_lakes <- data_env$selected_lakes
  azov <- data_env$azov
  
  regions_df <- json_data$regions
  if (is.null(regions_df) || nrow(regions_df) == 0) {
    stop("В JSON нет поля 'regions' или оно пустое.")
  }
  cat("Количество регионов:", nrow(regions_df), "\n")
  
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
  combined$district_visited <- factor(combined$district_visited, levels = all_districts)
  
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
  cat("Заголовок:", title, "\n")
  
  p <- ggplot() +
    geom_sf(data = combined, color = NA, size = 0, aes(fill = district_visited)) +
    scale_fill_manual(
      values = district_colors,
      na.value = "#E8E8E8",
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
  t_start <- Sys.time()
  cat("generate_main_map(): начало\n")
  regions_df <- data.frame(
    region_name_en = character(),
    district_name = character(),
    stringsAsFactors = FALSE
  )
  for (dist in json_data$districts) {
    dist_name <- dist$district_name
    for (reg in dist$regions) {
      has_visited <- any(sapply(reg$cities, function(city) isTRUE(city$visited)))
      if (has_visited) {
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
    cat("generate_main_map: завершено за", round(difftime(Sys.time(), t_start, units = "secs"), 2), "сек\n")
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
  cat("generate_main_map: завершено за", round(difftime(Sys.time(), t_start, units = "secs"), 2), "сек\n")
  return(p)
}

generate_cities_map_pdf <- function(json_data) {
  t_start <- Sys.time()
  cat("generate_cities_map_pdf(): начало\n")
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
    cat("generate_cities_map_pdf: завершено за", round(difftime(Sys.time(), t_start, units = "secs"), 2), "сек\n")
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
  cat("generate_cities_map_pdf: завершено за", round(difftime(Sys.time(), t_start, units = "secs"), 2), "сек\n")
  return(p)
}

generate_region_pages_pdf <- function(json_data, combined) {
  t_start <- Sys.time()
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
          visited = isTRUE(city$visited)
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
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "cyr")) +
        labs(title = region_name)
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
      geom_text(data = region_cities, check_overlap = TRUE,
                aes(x = lon, y = lat, label = city_name, color = visited),
                size = 2.5, hjust = 0, vjust = 1, family = "cyr") +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray50")) +
      coord_sf() +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12, family = "cyr"),
            legend.position = "none") +
      labs(title = region_name)
    
    plots[[i]] <- p
  }
  cat("generate_region_pages_pdf: завершено за", round(difftime(Sys.time(), t_start, units = "secs"), 2), "сек\n")
  return(plots)
}

# ---- Эндпоинт /report (PDF) ----
#* @post /report
#* @raw
function(req, res) {
  total_start <- Sys.time()
  cat("=== Запрос на /report ===\n")
  
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
    error = function(e) {
      res$status <- 400
      return(list(error = "Invalid JSON"))
    }
  )
  
  required <- c("client_id", "first_name", "last_name", "districts")
  missing <- setdiff(required, names(body))
  if (length(missing) > 0) {
    res$status <- 400
    return(list(error = paste("Missing fields:", paste(missing, collapse=", "))))
  }
  if (length(body$districts) == 0) {
    res$status <- 400
    return(list(error = "districts must be a non-empty array"))
  }
  
  cat("Клиент:", body$client_id, "\n")
  
  data_env <- load_data()
  combined <- data_env$combined
  combined <- combined[!duplicated(combined$name_en), ]
  
  cat("Генерация страницы 1 (главная карта)...\n")
  p_main <- generate_main_map(body)
  cat("Генерация страницы 2 (карта городов)...\n")
  p_cities <- generate_cities_map_pdf(body)
  cat("Генерация страниц регионов...\n")
  region_plots <- generate_region_pages_pdf(body, combined)
  
  tmp_pdf <- tempfile(fileext = ".pdf")
  cat("Сохранение PDF во временный файл...\n")
  pdf(tmp_pdf, width = 12, height = 10, family = "cyr")
  
  print(p_main)
  print(p_cities)
  for (p in region_plots) {
    print(p)
  }
  
  dev.off()
  
  pdf_raw <- readBin(tmp_pdf, "raw", n = file.info(tmp_pdf)$size)
  res$setHeader("Content-Type", "application/pdf")
  res$setHeader("Content-Disposition", "attachment; filename=report.pdf")
  res$body <- pdf_raw
  
  total_time <- difftime(Sys.time(), total_start, units = "secs")
  cat("=== Отчёт сгенерирован за", round(total_time, 2), "сек ===\n")
  return(res)
}

# ---- Эндпоинт /map (PNG) ----
#* @post /map
#* @raw
function(req, res) {
  cat("=== Запрос на /map ===\n")
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody),
    error = function(e) {
      res$status <- 400
      return(list(error = "Invalid JSON"))
    }
  )
  if (is.null(body) || !"regions" %in% names(body)) {
    res$status <- 400
    return(list(error = "Missing 'regions' field"))
  }
  required <- c("client_id", "first_name", "last_name", "regions")
  missing <- setdiff(required, names(body))
  if (length(missing) > 0) {
    res$status <- 400
    return(list(error = paste("Missing fields:", paste(missing, collapse=", "))))
  }
  if (!is.data.frame(body$regions) || nrow(body$regions) == 0) {
    res$status <- 400
    return(list(error = "regions must be a non-empty array of objects"))
  }
  
  data_env <- load_data()
  p <- generate_map_from_regions(data_env, body, output_file = NULL)
  
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, p, width = 12, height = 10, dpi = 300)
  result <- readBin(tmp, "raw", n = file.info(tmp)$size)
  
  res$setHeader("Content-Type", "image/png")
  res$body <- result
  return(res)
}

cat("=== API загружено успешно ===\n")
