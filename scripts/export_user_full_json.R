#!/usr/bin/env Rscript

# ============================================================
# ЭКСПОРТ ПОЛНЫХ ДАННЫХ ПОЛЬЗОВАТЕЛЯ В JSON
# ============================================================
# Формат вывода:
#   округ → регион → город (название, координаты, население, флаг посещения)
# Использование:
#   Rscript scripts/export_user_full_json.R <user_id>
# Пример:
#   Rscript scripts/export_user_full_json.R 1
#
# Результат:
#   - data/input/user_<id>_full.json
# ============================================================

library(DBI)
library(RPostgres)
library(jsonlite)

# ---- 1. Конфигурация подключения к БД ----
DB_CONFIG <- list(
  host     = "aws-1-eu-north-1.pooler.supabase.com",
  port     = 5432,
  dbname   = "postgres",
  user     = "postgres.twmbrcbnofrkntblmqkm",
  password = "9OBLWWCCiiIP0m8d",
  sslmode  = "require"
)

# ---- 2. Основная функция экспорта ----
export_user_full <- function(user_id, output_dir = "data/input") {
  cat(sprintf("🚀 Начинаем экспорт для пользователя ID: %d\n", user_id))
  
  # ---- 2.1 Подключение к БД ----
  cat("📡 Подключение к базе данных...\n")
  con <- dbConnect(
    RPostgres::Postgres(),
    host     = DB_CONFIG$host,
    port     = DB_CONFIG$port,
    dbname   = DB_CONFIG$dbname,
    user     = DB_CONFIG$user,
    password = DB_CONFIG$password,
    sslmode  = DB_CONFIG$sslmode
  )
  on.exit({
    dbDisconnect(con)
    cat("🔌 Соединение с БД закрыто.\n")
  })
  cat("✅ Подключение установлено.\n")
  
  # ---- 2.2 Получаем имя и фамилию пользователя ----
  cat("👤 Получение данных пользователя...\n")
  user_info <- dbGetQuery(con, 
                          "SELECT first_name, last_name FROM vkotov_russian_city_bot.dim_user WHERE id = $1", 
                          params = list(user_id)
  )
  if (nrow(user_info) == 0) {
    stop(sprintf("❌ Пользователь с ID %d не найден.", user_id))
  }
  cat(sprintf("✅ Пользователь: %s %s\n", user_info$first_name[1], user_info$last_name[1]))
  
  # ---- 2.3 Загружаем данные: округ → регион → город ----
  cat("📊 Загрузка данных о городах, регионах и округах...\n")
  query <- "
    SELECT 
      d.name AS district_name,
      r.name AS region_name_ru,
      r.name_en AS region_name_en,
      c.id AS city_id,
      c.name AS city_name,
      c.lat,
      c.lon,
      c.population,
      (fuc.city_id IS NOT NULL) AS visited
    FROM vkotov_russian_city_bot.dim_district d
    JOIN vkotov_russian_city_bot.dim_region r ON d.id = r.district_id
    JOIN vkotov_russian_city_bot.dim_city c ON r.id = c.region_id
    LEFT JOIN vkotov_russian_city_bot.fact_user_city fuc 
      ON c.id = fuc.city_id AND fuc.user_id = $1 AND fuc.dt_end IS NULL
    WHERE c.lat IS NOT NULL AND c.lon IS NOT NULL
      AND c.name IS NOT NULL AND c.name != ''
    ORDER BY d.id, r.id, c.id;
  "
  data <- dbGetQuery(con, query, params = list(user_id))
  cat(sprintf("✅ Загружено %d городов с координатами.\n", nrow(data)))
  
  if (nrow(data) == 0) {
    cat("⚠️ Нет городов с координатами для этого пользователя. Экспорт отменён.\n")
    return(invisible(NULL))
  }
  
  # ---- 2.4 Группировка данных в иерархическую структуру ----
  cat("🔄 Группировка данных по округам и регионам...\n")
  districts_list <- list()
  districts <- unique(data$district_name)
  cat(sprintf("   Найдено округов: %d\n", length(districts)))
  
  for (dist_idx in seq_along(districts)) {
    dist_name <- districts[dist_idx]
    cat(sprintf("   Обработка округа [%d/%d]: %s\n", dist_idx, length(districts), dist_name))
    
    dist_data <- data[data$district_name == dist_name, ]
    regions_list <- list()
    regions <- unique(dist_data$region_name_en)
    
    for (reg_idx in seq_along(regions)) {
      reg_en <- regions[reg_idx]
      reg_data <- dist_data[dist_data$region_name_en == reg_en, ]
      region_ru <- unique(reg_data$region_name_ru)[1]
      
      cities_list <- list()
      for (i in 1:nrow(reg_data)) {
        # Обрабатываем population (может быть NULL)
        pop <- reg_data$population[i]
        if (is.na(pop) || is.null(pop)) pop <- 0
        
        cities_list[[i]] <- list(
          city_name = reg_data$city_name[i],
          lat = as.numeric(reg_data$lat[i]),
          lon = as.numeric(reg_data$lon[i]),
          population = as.numeric(pop),
          visited = as.logical(reg_data$visited[i])
        )
      }
      
      regions_list[[reg_en]] <- list(
        region_name_ru = region_ru,
        region_name_en = reg_en,
        cities = cities_list
      )
    }
    
    districts_list[[dist_name]] <- list(
      district_name = dist_name,
      regions = regions_list
    )
  }
  
  # ---- 2.5 Формирование итогового JSON ----
  cat("📝 Формирование итогового JSON...\n")
  json_data <- list(
    client_id = as.character(user_id),
    first_name = user_info$first_name[1],
    last_name = user_info$last_name[1],
    districts = districts_list
  )
  
  # ---- 2.6 Сохранение JSON в файл ----
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, paste0("user_", user_id, "_full.json"))
  write_json(json_data, output_file, pretty = TRUE, auto_unbox = TRUE)
  cat(sprintf("✅ JSON сохранён в: %s\n", output_file))
  
  # ---- 2.7 Статистика ----
  total_regions <- sum(sapply(districts_list, function(d) length(d$regions)))
  total_cities <- nrow(data)
  visited_cities <- sum(data$visited)
  
  cat("\n📊 СТАТИСТИКА ЭКСПОРТА:\n")
  cat(sprintf("   Округов:           %d\n", length(districts_list)))
  cat(sprintf("   Регионов:          %d\n", total_regions))
  cat(sprintf("   Городов (всего):   %d\n", total_cities))
  cat(sprintf("   Городов (посещено):%d\n", visited_cities))
  cat(sprintf("   Городов (не посещено):%d\n", total_cities - visited_cities))
  cat("🎉 Экспорт успешно завершён!\n")
  
  return(output_file)
}

# ---- 3. Запуск из командной строки ----
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Укажите ID пользователя:\n")
    cat("  Rscript scripts/export_user_full_json.R <user_id>\n")
    quit(status = 1)
  }
  user_id <- as.integer(args[1])
  if (is.na(user_id)) {
    cat("ID пользователя должен быть числом.\n")
    quit(status = 1)
  }
  export_user_full(user_id)
}