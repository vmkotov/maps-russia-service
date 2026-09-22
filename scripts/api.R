# scripts/api.R
# Plumber API с поддержкой PNG и PDF (с LiberationSans через showtext)

Sys.setlocale("LC_ALL", "C.UTF-8")

library(plumber)
library(sf)
library(ggplot2)
library(jsonlite)
library(showtext)
library(sysfonts)
library(patchwork)

setwd("/app")
cat("Working directory set to:", getwd(), "\n")
cat("Files in /app/data/rds/:", list.files("/app/data/rds/"), "\n")

# ---- Регистрируем шрифт Liberation Sans ----
font_paths <- c(
  "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
  "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
  "/System/Library/Fonts/Helvetica.ttc",
  "/System/Library/Fonts/Supplemental/Arial.ttf",
  "/Library/Fonts/Arial.ttf"
)
font_found <- FALSE
for (fp in font_paths) {
  if (file.exists(fp)) {
    font_add("liberation", regular = fp)
    cat("✅ Шрифт найден:", fp, "\n")
    font_found <- TRUE
    break
  }
}
if (!font_found) {
  stop("❌ Не найден ни один шрифт с поддержкой кириллицы.")
}
showtext_auto()
cat("🔤 Шрифты Arial активированы.\n")

# ---- Загрузка данных ----
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

# ---- Функция для PNG (старый эндпоинт) ----
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

  # Преобразуем в фактор с уровнями всех округов (для легенды)
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
      na.translate = FALSE,
      breaks = all_districts
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

# ---- Функции для PDF-отчёта (исправленные) ----
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
    geom_point(data = cities_df, aes(x = lon, y = lat), color = "red", size = 0.3, alpha = 1) +
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
  pdf(tmp_pdf, width = 12, height = 10)

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

# ---- Комбинированная PNG (9:16) со стильным блоком статистики ----
generate_combined_map <- function(json_data) {
  p_main <- generate_main_map(json_data)
  p_cities <- generate_cities_map_pdf(json_data)

  # ---- Статистика ----
  regions_visited <- sum(sapply(json_data$districts, function(d) {
    sum(sapply(d$regions, function(r) isTRUE(r$region_visited)))
  }))
  cities_visited <- sum(sapply(json_data$districts, function(d) {
    sum(sapply(d$regions, function(r) {
      sum(sapply(r$cities, function(c) isTRUE(c$visited)))
    }))
  }))
  total_regions <- length(unique(unlist(lapply(json_data$districts, function(d) {
    sapply(d$regions, function(r) r$region_name_en)
  }))))
  total_cities <- sum(sapply(json_data$districts, function(d) {
    sum(sapply(d$regions, function(r) length(r$cities)))
  }))

  # ---- Сбор всех регионов ----
  clean_name <- function(x) {
    # Особые случаи
    x <- gsub("^Донецкая Народная Республика$", "ДНР", x)
    x <- gsub("^Луганская Народная Республика$", "ЛНР", x)
    x <- gsub("^Еврейская автономная область$", "Еврейская", x)
    x <- gsub("^Республика Северная Осетия\\s*–\\s*Алания$", "Северная Осетия", x)
    x <- gsub("^Чувашская Республика$", "Чувашия", x)
    x <- gsub("^Чеченская Республика$", "Чечня", x)
    x <- gsub("^Удмуртская Республика$", "Удмуртия", x)
    x <- gsub("^Кабардино-Балкарская Республика$", "Кабардино-Балкария", x)
    x <- gsub("^Кабардино-Балкарская Республика$", "КБР", x)
    x <- gsub("^Карачаево-Черкесская Республика$", "КЧР", x)
    x <- gsub("^Ханты-Мансийский автономный округ\\s*–\\s*Югра$", "ХМАО", x)
    # Общие правила
    x <- gsub("^Республика\\s+", "", x)
    x <- gsub("\\s+Республика$", "", x)
    x <- gsub("\\s+область$", "", x)
    x <- gsub("\\s+край$", "", x)
    x <- gsub("\\s+автономный округ$", "", x)
    trimws(x)
  }
  reg_df <- data.frame()
  for (dist in json_data$districts) {
    for (reg in dist$regions) {
      reg_df <- rbind(reg_df, data.frame(
        name = clean_name(reg$region_name_ru),
        visited = isTRUE(reg$region_visited),
        stringsAsFactors = FALSE
      ))
    }
  }
  reg_df <- reg_df[order(reg_df$name), ]
  n <- nrow(reg_df)
  ncols <- 6
  nrows <- ceiling(n / ncols)
  reg_df$col <- rep(1:ncols, length.out = n)
  reg_df$row <- rep(1:nrows, each = ncols, length.out = n)
  reg_df$x <- (reg_df$col - 0.5) / ncols
  reg_df$y <- 1 - (reg_df$row - 0.5) / nrows

  p_regions <- ggplot(reg_df, aes(x = x, y = y, label = name, color = visited)) +
    geom_text(size = 2.0, family = "liberation") +
    scale_color_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#c0392b")) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          legend.position = "none",
          plot.margin = margin(-20, 0, 0, 0))

  # ---- Верхний блок: статистика ----
  stat_df <- data.frame(
    x     = c(0.3, 0.7, 0.3, 0.7),
    y     = c(1.3, 1.3, -0.2, -0.2),
    label = c(as.character(regions_visited), as.character(cities_visited),
              paste0("из ", total_regions, " регионов"),
              paste0("из ", total_cities, " городов")),
    size  = c(12, 12, 3.0, 3.0),
    color = c("#1a1a1a", "#1a1a1a", "#888888", "#888888")
  )
  p_stat <- ggplot(stat_df, aes(x = x, y = y, label = label)) +
    geom_text(aes(size = size, color = color), family = "liberation", vjust = 0.5) +
    scale_size_identity() + scale_color_identity() +
    xlim(0, 1) + ylim(-0.5, 2.2) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(35, 10, 5, 10))

  # ---- Подпись над второй картой ----
  label_df <- data.frame(x = 0.5, y = 0.5, label = "ПОСЕЩЁННЫЕ ГОРОДА")
  p_label <- ggplot(label_df, aes(x = x, y = y, label = label)) +
    geom_text(size = 3.5, color = "#888888", family = "liberation") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(5, 5, 0, 5))

  # ---- Нижний блок ----
  bottom_df <- data.frame(x = 0.5, y = 0.5, label = "t.me/vkotov_russian_city_bot")
  p_bottom <- ggplot(bottom_df, aes(x = x, y = y, label = label)) +
    geom_text(size = 4.2, color = "#5b8def", family = "liberation") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(5, 5, 15, 5))

  # ---- Карты ----
  p_main <- p_main +
    labs(title = NULL) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 7, family = "liberation"),
          legend.key.size = unit(0.3, "cm"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          plot.margin = margin(0, 0, 0, 0)) +
    coord_sf(expand = FALSE)

  p_cities <- p_cities +
    labs(title = NULL) +
    theme(plot.margin = margin(0, 0, 0, 0)) +
    coord_sf(expand = FALSE)

  combined <- patchwork::wrap_plots(p_stat, p_main, p_regions, p_label, p_cities, p_bottom,
                                    ncol = 1,
                                    heights = c(0.14, 1, 0.38, 0.035, 1, 0.07))

  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, combined, width = 6, height = 10.67, dpi = 180, bg = "white")
  return(tmp)
}
#* @post /map
#* @raw
function(req, res) {
  cat("=== Запрос на /map ===\n")
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

  tmp_file <- generate_combined_map(body)
  result <- readBin(tmp_file, "raw", n = file.info(tmp_file)$size)

  res$setHeader("Content-Type", "image/png")
  res$body <- result
  return(res)
}

cat("=== API загружено успешно ===\n")
