# scripts/api.R
# Plumber API с ручной отправкой PNG через res$body

library(plumber)
library(sf)
library(ggplot2)
library(jsonlite)

setwd("/app")
cat("Working directory set to:", getwd(), "\n")
cat("Files in /app/data/rds/:", list.files("/app/data/rds/"), "\n")

# ---- Встроенная функция load_data ----
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

# ---- Встроенная функция generate_map_from_regions ----
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
      name = "Федеральный округ"
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
    cat("Возвращаем ggplot\n")
    return(p)
  }
  
  cat("Сохраняем в файл:", output_file, "\n")
  ggsave(output_file, p, width = 12, height = 10, dpi = 300)
  cat("Файл сохранён\n")
  return(output_file)
}

# ---- Эндпоинт /map ----
#* @post /map
#* @raw
function(req, res) {
  cat("=== Запрос на /map ===\n")
  cat("Request method:", req$REQUEST_METHOD, "\n")
  
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody),
    error = function(e) {
      cat("Ошибка парсинга JSON:", e$message, "\n")
      res$status <- 400
      return(list(error = "Invalid JSON"))
    }
  )
  
  if (is.null(body) || !"regions" %in% names(body)) {
    cat("Отсутствует поле regions\n")
    res$status <- 400
    return(list(error = "Missing 'regions' field"))
  }
  
  required <- c("client_id", "first_name", "last_name", "regions")
  missing <- setdiff(required, names(body))
  if (length(missing) > 0) {
    cat("Отсутствуют поля:", paste(missing, collapse=", "), "\n")
    res$status <- 400
    return(list(error = paste("Missing fields:", paste(missing, collapse=", "))))
  }
  
  if (!is.data.frame(body$regions) || nrow(body$regions) == 0) {
    cat("regions должен быть непустым массивом\n")
    res$status <- 400
    return(list(error = "regions must be a non-empty array of objects"))
  }
  
  cat("Загружаем данные...\n")
  data_env <- load_data()
  cat("Строим карту...\n")
  p <- generate_map_from_regions(data_env, body, output_file = NULL)
  
  cat("Сохраняем во временный PNG...\n")
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, p, width = 12, height = 10, dpi = 300)
  cat("Временный файл создан:", tmp, "\n")
  cat("Размер файла:", file.info(tmp)$size, "bytes\n")
  
  result <- readBin(tmp, "raw", n = file.info(tmp)$size)
  cat("Возвращаем PNG, длина:", length(result), "\n")
  
  res$setHeader("Content-Type", "image/png")
  res$body <- result
  return(res)
}

cat("=== API загружено успешно ===\n")