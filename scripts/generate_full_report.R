#!/usr/bin/env Rscript

# ============================================================
# ГЕНЕРАЦИЯ ПОЛНОГО PDF-ОТЧЁТА ДЛЯ ПОЛЬЗОВАТЕЛЯ
# ============================================================
# Использование:
#   Rscript scripts/generate_full_report.R <user_id>
# Пример:
#   Rscript scripts/generate_full_report.R 1
#
# Результат:
#   - output/report_user_<id>.pdf
# ============================================================

library(sf)
library(ggplot2)
library(DBI)
library(RPostgres)
library(jsonlite)
library(ggrepel)

DB_CONFIG <- list(
  host     = "aws-1-eu-north-1.pooler.supabase.com",
  port     = 5432,
  dbname   = "postgres",
  user     = "postgres.twmbrcbnofrkntblmqkm",
  password = "9OBLWWCCiiIP0m8d",
  sslmode  = "require"
)

load_data <- function() {
  list(
    combined = readRDS("data/rds/combined_split.rds"),
    rivers = readRDS("data/rds/russian_rivers.rds"),
    lakes = readRDS("data/rds/selected_lakes.rds"),
    azov = readRDS("data/rds/azov_sea.rds")
  )
}

generate_main_map <- function(json_data) {
  source("scripts/generate_from_json.R", local = TRUE)
  data_env <- load_data()
  p <- generate_map_from_regions(data_env, json_data, output_file = NULL)
  return(p)
}

generate_cities_map <- function(cities_df) {
  data_env <- load_data()
  combined <- data_env$combined
  rivers <- data_env$rivers
  lakes <- data_env$lakes
  azov <- data_env$azov
  combined <- combined[!duplicated(combined$name_en), ]
  
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

generate_region_pages <- function(combined, cities, visited_city_ids) {
  con <- dbConnect(
    RPostgres::Postgres(),
    host     = DB_CONFIG$host,
    port     = DB_CONFIG$port,
    dbname   = DB_CONFIG$dbname,
    user     = DB_CONFIG$user,
    password = DB_CONFIG$password,
    sslmode  = DB_CONFIG$sslmode
  )
  region_order <- dbGetQuery(con, "SELECT id, name_en FROM vkotov_russian_city_bot.dim_region WHERE name_en IS NOT NULL ORDER BY id")
  dbDisconnect(con)
  
  combined$order <- match(combined$name_en, region_order$name_en)
  combined <- combined[!is.na(combined$order), ]
  combined <- combined[order(combined$order), ]
  
  plots <- list()
  for (i in 1:nrow(combined)) {
    region_poly <- combined[i, ]
    region_id <- combined$order[i]
    region_name <- region_poly$name
    
    region_cities <- cities[cities$region_id == region_id, ]
    if (nrow(region_cities) == 0) {
      p <- ggplot() +
        geom_sf(data = region_poly, fill = "#E8E8E8", color = "#2E4053", size = 0.5) +
        coord_sf() +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        labs(title = region_name)
      plots[[i]] <- p
      next
    }
    
    region_cities$visited <- region_cities$id %in% visited_city_ids
    
    p <- ggplot() +
      geom_sf(data = region_poly, fill = "#E8E8E8", color = "#2E4053", size = 0.5) +
      geom_point(data = region_cities[region_cities$visited, ], 
                 aes(x = lon, y = lat), color = "red", shape = 16, size = 2) +
      geom_point(data = region_cities[!region_cities$visited, ], 
                 aes(x = lon, y = lat), color = "gray50", shape = 1, size = 2) +
      geom_text_repel(data = region_cities, 
                      aes(x = lon, y = lat, label = city_name, color = visited),
                      size = 2.5, box.padding = 0.3, point.padding = 0.2,
                      segment.color = NA, max.overlaps = 10) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray50")) +
      coord_sf() +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
            legend.position = "none") +
      labs(title = region_name)
    
    plots[[i]] <- p
  }
  return(plots)
}

generate_full_report <- function(user_id) {
  con <- dbConnect(
    RPostgres::Postgres(),
    host     = DB_CONFIG$host,
    port     = DB_CONFIG$port,
    dbname   = DB_CONFIG$dbname,
    user     = DB_CONFIG$user,
    password = DB_CONFIG$password,
    sslmode  = DB_CONFIG$sslmode
  )
  
  regions <- dbGetQuery(con, "
    SELECT 
      r.name_en AS region_name_en,
      d.name AS district_name
    FROM vkotov_russian_city_bot.fact_user_region fur
    JOIN vkotov_russian_city_bot.dim_region r ON fur.region_id = r.id
    JOIN vkotov_russian_city_bot.dim_district d ON r.district_id = d.id
    WHERE fur.user_id = $1 AND fur.dt_end IS NULL
  ", params = list(user_id))
  
  cities <- dbGetQuery(con, "
    SELECT 
      c.id AS city_id,
      c.name AS city_name,
      c.region_id,
      c.lat,
      c.lon
    FROM vkotov_russian_city_bot.fact_user_city fuc
    JOIN vkotov_russian_city_bot.dim_city c ON fuc.city_id = c.id
    WHERE fuc.user_id = $1 AND fuc.dt_end IS NULL
      AND c.lat IS NOT NULL AND c.lon IS NOT NULL
      AND c.name IS NOT NULL AND c.name != ''
  ", params = list(user_id))
  
  user_info <- dbGetQuery(con, "SELECT first_name, last_name FROM vkotov_russian_city_bot.dim_user WHERE id = $1", params = list(user_id))
  dbDisconnect(con)
  
  full_name <- paste(user_info$first_name[1], user_info$last_name[1])
  
  json_data <- list(
    client_id = paste0("user_", user_id),
    first_name = user_info$first_name[1],
    last_name = user_info$last_name[1],
    regions = regions
  )
  
  combined <- readRDS("data/rds/combined_split.rds")
  combined <- combined[!duplicated(combined$name_en), ]
  
  all_cities <- dbGetQuery(con, "
    SELECT c.id, c.name AS city_name, c.region_id, c.lat, c.lon
    FROM vkotov_russian_city_bot.dim_city c
    WHERE c.lat IS NOT NULL AND c.lon IS NOT NULL
      AND c.name IS NOT NULL AND c.name != ''
  ")
  
  visited_city_ids <- cities$city_id
  
  p_main <- generate_main_map(json_data)
  p_cities <- generate_cities_map(cities[, c("lat", "lon")])
  region_plots <- generate_region_pages(combined, all_cities, visited_city_ids)
  
  output_dir <- "output"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_file <- file.path(output_dir, paste0("report_user_", user_id, ".pdf"))
  pdf(pdf_file, width = 12, height = 10, family = "Helvetica")
  
  print(p_main)
  print(p_cities)
  for (p in region_plots) {
    print(p)
  }
  
  dev.off()
  cat("✅ PDF сохранён в", pdf_file, "\n")
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Укажите ID пользователя:\n")
    cat("  Rscript scripts/generate_full_report.R <user_id>\n")
    quit(status = 1)
  }
  user_id <- as.integer(args[1])
  if (is.na(user_id)) {
    cat("ID пользователя должен быть числом.\n")
    quit(status = 1)
  }
  generate_full_report(user_id)
}
