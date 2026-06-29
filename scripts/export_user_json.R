#!/usr/bin/env Rscript

# ============================================================
# ЭКСПОРТ ДАННЫХ ПОЛЬЗОВАТЕЛЯ В JSON (с request_id)
# ============================================================
# Использование:
#   Rscript scripts/export_user_json.R <user_id> [request_id]
# Пример:
#   Rscript scripts/export_user_json.R 5 1001
#
# Результат:
#   - data/input/user_<id>_<request_id>.json
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

export_user_json <- function(user_id, request_id = NULL, output_dir = "data/input") {
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
  
  query <- "
    SELECT 
      first_name,
      last_name,
      region_id,
      region_name_ru,
      region_name_en,
      district_name
    FROM vkotov_russian_city_bot.v_user_visited_regions_full
    WHERE user_id = $1
    ORDER BY district_name, region_name_en;
  "
  df <- dbGetQuery(con, query, params = list(user_id))
  
  if (nrow(df) == 0) {
    cat("Пользователь с ID", user_id, "не найден или не имеет посещённых регионов.\n")
    return(invisible(NULL))
  }
  
  first_name <- df$first_name[1]
  last_name <- df$last_name[1]
  regions <- df[, c("region_id", "region_name_ru", "region_name_en", "district_name")]
  
  # Если request_id не передан, генерируем на основе user_id
  if (is.null(request_id)) {
    request_id <- as.integer(paste0(user_id, Sys.time()))  # не очень, но для теста
    # Просто возьмём user_id * 1000 + 1
    request_id <- user_id * 1000 + 1
  }
  
  json_data <- list(
    request_id = request_id,
    client_id = paste0("user_", user_id),
    first_name = first_name,
    last_name = last_name,
    regions = regions
  )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, paste0("user_", user_id, "_", request_id, ".json"))
  write_json(json_data, output_file, pretty = TRUE, auto_unbox = TRUE)
  
  cat("JSON сохранён в", output_file, "\n")
  cat("Количество регионов:", nrow(regions), "\n")
  return(output_file)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Укажите ID пользователя и (опционально) request_id:\n")
    cat("  Rscript scripts/export_user_json.R <user_id> [request_id]\n")
    quit(status = 1)
  }
  user_id <- as.integer(args[1])
  if (is.na(user_id)) {
    cat("ID пользователя должен быть числом.\n")
    quit(status = 1)
  }
  request_id <- if (length(args) >= 2) as.integer(args[2]) else NULL
  export_user_json(user_id, request_id)
}