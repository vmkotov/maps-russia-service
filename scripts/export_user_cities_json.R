#!/usr/bin/env Rscript

# ============================================================
# ЭКСПОРТ ГОРОДОВ ПОЛЬЗОВАТЕЛЯ В JSON
# ============================================================
# Использование:
#   Rscript scripts/export_user_cities_json.R <user_id>
# Пример:
#   Rscript scripts/export_user_cities_json.R 1
#
# Результат:
#   - data/input/user_<id>_cities.json
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

export_user_cities <- function(user_id, output_dir = "data/input") {
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
  
  cities <- dbGetQuery(con, "
    SELECT 
      c.id AS city_id,
      c.name AS city_name,
      c.lat,
      c.lon
    FROM vkotov_russian_city_bot.fact_user_city fuc
    JOIN vkotov_russian_city_bot.dim_city c ON fuc.city_id = c.id
    WHERE fuc.user_id = $1 AND fuc.dt_end IS NULL
      AND c.lat IS NOT NULL AND c.lon IS NOT NULL
      AND c.name IS NOT NULL AND c.name != ''
  ", params = list(user_id))
  
  if (nrow(cities) == 0) {
    cat("У пользователя нет посещённых городов с координатами.\n")
    return(invisible(NULL))
  }
  
  json_data <- list(
    cities = cities[, c("city_name", "lat", "lon")]
  )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, paste0("user_", user_id, "_cities.json"))
  write_json(json_data, output_file, pretty = TRUE, auto_unbox = TRUE)
  
  cat("JSON с городами сохранён в", output_file, "\n")
  cat("Количество городов:", nrow(cities), "\n")
  return(output_file)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Укажите ID пользователя:\n")
    cat("  Rscript scripts/export_user_cities_json.R <user_id>\n")
    quit(status = 1)
  }
  user_id <- as.integer(args[1])
  if (is.na(user_id)) {
    cat("ID пользователя должен быть числом.\n")
    quit(status = 1)
  }
  export_user_cities(user_id)
}
