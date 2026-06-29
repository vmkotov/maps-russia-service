# scripts/api.R
# Plumber API для генерации карты по JSON
# Все функции встроены, без source

library(plumber)
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

  districts_present <- unique(regions_df$district_name)
  colors_used <- district_colors[names(district_colors) %in% districts_present]

  default_color <- "#A9A9A9"
  missing_districts <- setdiff(districts_present, names(colors_used))
  if (length(missing_districts) > 0) {
    extra_colors <- setNames(rep(default_color, length(missing_districts)), missing_districts)
    colors_used <- c(colors_used, extra_colors)
  }

  full_name <- paste(json_data$first_name, json_data$last_name)
  title <- paste("Посещённые регионы –", full_name)

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

  if (is.null(output_file)) {
    return(p)
  }

  ggsave(output_file, p, width = 12, height = 10, dpi = 300)
  cat("Карта сохранена в", output_file, "\n")
  return(output_file)
}

# ---- Plumber API ----
#* @post /map
#* @serializer png
function(req, res) {
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

  required <- c("request_id", "client_id", "first_name", "last_name", "regions")
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
  print(p)
}
