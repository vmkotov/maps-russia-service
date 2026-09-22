# =====================================================================
# Скрипт: Генерация карт для всех регионов России с городами
# =====================================================================
# Требует:
#   - data/rds/combined.rds или combined_full.rds
#   - city_coordinates_full.csv (или другой файл с координатами)
#   - Подключение к БД для получения соответствия регионов
# Результат: PNG-файлы для каждого региона в папке output/region_maps/
# =====================================================================

library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(dplyr)   # для удобства фильтрации

# ---- 1. Установка рабочей директории ----
setwd("~/github/R_PROGRAMING_1/MAPS_RUSSIA_1.0")
cat("Рабочая директория:", getwd(), "\n")

# ---- 2. Подключение к БД и получение списка регионов ----
con <- dbConnect(
  RPostgres::Postgres(),
  host     = "aws-1-eu-north-1.pooler.supabase.com",
  port     = 5432,
  dbname   = "postgres",
  user     = "postgres.twmbrcbnofrkntblmqkm",
  password = "9OBLWWCCiiIP0m8d",
  sslmode  = "require"
)

regions_db <- dbGetQuery(con, "
  SELECT id, name AS name_ru, name_en
  FROM vkotov_russian_city_bot.dim_region
  WHERE name_en IS NOT NULL
  ORDER BY name_ru;
")
dbDisconnect(con)

cat("Найдено регионов с name_en:", nrow(regions_db), "\n")

# ---- 3. Загрузка полигонов регионов ----
if (file.exists("data/rds/combined_full.rds")) {
  combined <- readRDS("data/rds/combined_full.rds")
  cat("Загружен combined_full.rds\n")
} else if (file.exists("data/rds/combined.rds")) {
  combined <- readRDS("data/rds/combined.rds")
  cat("Загружен combined.rds\n")
} else {
  stop("Нет combined.rds или combined_full.rds")
}

# ---- 4. Загрузка координат городов ----
if (file.exists("city_coordinates_full.csv")) {
  cities <- read.csv("city_coordinates_full.csv", stringsAsFactors = FALSE)
} else if (file.exists("city_coordinates_locationiq.csv")) {
  cities <- read.csv("city_coordinates_locationiq.csv", stringsAsFactors = FALSE)
} else {
  stop("Файл с координатами городов не найден")
}

cat("Загружено городов:", nrow(cities), "\n")

# ---- 5. Создание папки для выходных карт ----
output_dir <- "output/region_maps"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 6. Цикл по регионам ----
success_count <- 0
skip_count <- 0

for (i in 1:nrow(regions_db)) {
  region_id <- regions_db$id[i]
  name_ru <- regions_db$name_ru[i]
  name_en <- regions_db$name_en[i]
  
  cat(sprintf("\n[%d/%d] Обработка: %s (%s)", i, nrow(regions_db), name_ru, name_en))
  
  # ---- 6a. Находим полигон региона ----
  region_poly <- combined[combined$name_en == name_en, ]
  if (nrow(region_poly) == 0) {
    cat(" -> полигон не найден, пропускаем\n")
    skip_count <- skip_count + 1
    next
  }
  
  # ---- 6b. Фильтруем города по русскому названию региона ----
  region_cities <- cities[cities$region_name == name_ru, ]
  region_cities <- region_cities[!is.na(region_cities$lat) & !is.na(region_cities$lon), ]
  
  if (nrow(region_cities) == 0) {
    cat(" -> городов не найдено, пропускаем\n")
    skip_count <- skip_count + 1
    next
  }
  
  # ---- 6c. Построение карты ----
  p <- ggplot() +
    geom_sf(data = region_poly, fill = "#E8E8E8", color = "#2E4053", size = 0.5) +
    geom_point(data = region_cities, aes(x = lon, y = lat), 
               color = "#E63946", size = 1.5, alpha = 0.8) +
    geom_text(data = region_cities, aes(x = lon, y = lat, label = city_name), 
              size = 2, hjust = 0, vjust = 1, check_overlap = TRUE) +
    coord_sf() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
    ) +
    labs(title = name_ru)
  
  # ---- 6d. Сохранение ----
  output_file <- file.path(output_dir, sprintf("region_%d_%s.png", region_id, name_en))
  ggsave(output_file, p, width = 8, height = 8, dpi = 150)
  cat(sprintf(" -> сохранено: %s", basename(output_file)))
  success_count <- success_count + 1
}

# ---- 7. Итоговая статистика ----
cat("\n\n===== СТАТИСТИКА =====\n")
cat(sprintf("Всего регионов в БД с name_en: %d\n", nrow(regions_db)))
cat(sprintf("Успешно сгенерировано карт: %d\n", success_count))
cat(sprintf("Пропущено (нет полигона или городов): %d\n", skip_count))
cat(sprintf("Карты сохранены в папке: %s\n", output_dir))