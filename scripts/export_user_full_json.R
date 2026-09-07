#!/usr/bin/env Rscript

# ============================================================
# ЭКСПОРТ ПОЛНЫХ ДАННЫХ ПОЛЬЗОВАТЕЛЯ В JSON (с region_visited)
# ============================================================
# Использование:
#   Rscript scripts/export_user_full_json.R <user_id>
# Пример:
#   Rscript scripts/export_user_full_json.R 2
#
# Результат:
#   - data/input/user_<id>_full.json
# ============================================================

library(DBI)
library(RPostgres)
library(jsonlite)

DB_CONFIG <- list(
  host     = "aws-1-eu-north-1.pooler.supabase.com",
  port     = 5432,
  dbname   = "postgres",
  user     = "postgres.twmbrcbnofrkntblmqkm",
  password = "9OBLWWCCiiIP0m8d",
  sslmode  = "require"
)

export_user_full <- function(user_id, output_dir = "data/input") {
  cat(sprintf("🚀 Начинаем экспорт для пользователя ID: %d\n", user_id))
  
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
  
  # 1. Имя пользователя
  user_info <- dbGetQuery(con, "SELECT first_name, last_name FROM vkotov_russian_city_bot.dim_user WHERE id = $1", params = list(user_id))
  if (nrow(user_info) == 0) {
    stop(sprintf("Пользователь с ID %d не найден.", user_id))
  }
  cat(sprintf("👤 Пользователь: %s %s\n", user_info$first_name[1], user_info$last_name[1]))
  
  # 2. Получаем список посещённых регионов
  visited_regions <- dbGetQuery(con, "
    SELECT DISTINCT region_id
    FROM vkotov_russian_city_bot.fact_user_region
    WHERE user_id = $1 AND dt_end IS NULL
  ", params = list(user_id))
  visited_ids <- visited_regions$region_id
  cat(sprintf("📌 Посещённых регионов: %d\n", length(visited_ids)))
  
  # 3. Загружаем города с координатами, населением и флагом посещения города
  query <- "
    SELECT 
      d.name AS district_name,
      r.id AS region_id,
      r.name AS region_name_ru,
      r.name_en AS region_name_en,
      c.id AS city_id,
      c.name AS city_name,
      c.lat,
      c.lon,
      c.population,
      (fuc.city_id IS NOT NULL) AS city_visited
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
  cat(sprintf("📊 Загружено городов с координатами: %d\n", nrow(data)))
  
  if (nrow(data) == 0) {
    cat("⚠️ Нет городов с координатами для этого пользователя. Экспорт отменён.\n")
    return(invisible(NULL))
  }
  
  # 4. Группировка
  districts_list <- list()
  districts <- unique(data$district_name)
  cat(sprintf("🔄 Обработка округов: %d\n", length(districts)))
  
  for (dist in districts) {
    dist_data <- data[data$district_name == dist, ]
    regions_list <- list()
    regions <- unique(dist_data$region_name_en)
    for (reg_en in regions) {
      reg_data <- dist_data[dist_data$region_name_en == reg_en, ]
      region_ru <- unique(reg_data$region_name_ru)[1]
      region_id <- unique(reg_data$region_id)[1]
      region_visited <- region_id %in% visited_ids
      
      cities_list <- list()
      for (i in 1:nrow(reg_data)) {
        pop <- reg_data$population[i]
        if (is.na(pop) || is.null(pop)) pop <- 0
        cities_list[[i]] <- list(
          city_name = reg_data$city_name[i],
          lat = as.numeric(reg_data$lat[i]),
          lon = as.numeric(reg_data$lon[i]),
          population = as.numeric(pop),
          visited = as.logical(reg_data$city_visited[i])
        )
      }
      
      regions_list[[reg_en]] <- list(
        region_name_ru = region_ru,
        region_name_en = reg_en,
        region_visited = region_visited,
        cities = cities_list
      )
    }
    districts_list[[dist]] <- list(
      district_name = dist,
      regions = regions_list
    )
  }
  
  # 5. Итоговый JSON
  json_data <- list(
    client_id = as.character(user_id),
    first_name = user_info$first_name[1],
    last_name = user_info$last_name[1],
    districts = districts_list
  )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, paste0("user_", user_id, "_full.json"))
  write_json(json_data, output_file, pretty = TRUE, auto_unbox = TRUE)
  
  cat(sprintf("✅ JSON сохранён в: %s\n", output_file))
  cat("\n📊 СТАТИСТИКА ЭКСПОРТА:\n")
  cat(sprintf("   Округов:           %d\n", length(districts_list)))
  cat(sprintf("   Регионов:          %d\n", sum(sapply(districts_list, function(d) length(d$regions)))))
  cat(sprintf("   Городов (всего):   %d\n", nrow(data)))
  cat(sprintf("   Городов (посещено):%d\n", sum(data$city_visited)))
  cat("🎉 Экспорт успешно завершён!\n")
  
  return(output_file)
}

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
