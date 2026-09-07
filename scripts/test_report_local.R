# test_report_local.R
# Локальная проверка генерации PDF с новыми правилами

Sys.setlocale("LC_ALL", "C.UTF-8")

library(sf)
library(ggplot2)
library(jsonlite)
library(showtext)
library(sysfonts)

# ---- Шрифт (как в api.R) ----
font_path <- "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
if (file.exists(font_path)) {
  font_add("liberation", regular = font_path)
  cat("✅ Шрифт Liberation Sans найден и зарегистрирован.\n")
} else {
  font_path2 <- "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
  if (file.exists(font_path2)) {
    font_add("liberation", regular = font_path2)
    cat("✅ Шрифт DejaVu Sans найден и зарегистрирован.\n")
  } else {
    font_add("liberation", family = "sans")
    cat("⚠️ Используем системный шрифт sans.\n")
  }
}
showtext_auto()
cat("🔤 showtext активирован.\n")

# ---- Загрузка данных (копия из api.R) ----
load_data <- function() {
  cat("load_data(): начало\n")
  required_files <- c(
    "data/rds/combined_split.rds",
    "data/rds/russian_rivers.rds",
    "data/rds/selected_lakes.rds",
    "data/rds/azov_sea.rds"
  )
  for (f in required_files) {
    cat("Проверка файла:", f, " - ", file.exists(f), "\n")
    if (!file.exists(f)) {
      stop("Не найден файл: ", f)
    }
  }
  cat("Все файлы найдены. Загружаем...\n")
  data <- list(
    combined = readRDS("data/rds/combined_split.rds"),
    rivers = readRDS("data/rds/russian_rivers.rds"),
    selected_lakes = readRDS("data/rds/selected_lakes.rds"),
    azov = readRDS("data/rds/azov_sea.rds")
  )
  cat("Данные загружены успешно\n")
  return(data)
}

# ---- Функции (копия из api.R) ----
generate_map_from_regions <- function(data_env, json_data, output_file = NULL) {
  # ... (полная копия из api.R)
  # Для краткости я сокращу, но в реальном скрипте нужно вставить весь код.
  # Однако мы можем просто source("scripts/api.R") и использовать его функции.
  # Но чтобы не зависеть от файла, я предложу source.
}

# ---- Основной тест ----
source("scripts/api.R")  # загружаем все функции из api.R

json_file <- "data/input/user_2_full.json"
if (!file.exists(json_file)) {
  stop("Файл JSON не найден: ", json_file)
}
json_data <- fromJSON(json_file, simplifyVector = FALSE)

cat("Клиент:", json_data$client_id, "\n")
cat("Пользователь:", json_data$first_name, json_data$last_name, "\n")

data_env <- load_data()
combined <- data_env$combined
combined <- combined[!duplicated(combined$name_en), ]

cat("Генерация страницы 1 (главная карта)...\n")
p_main <- generate_main_map(json_data)
cat("Генерация страницы 2 (карта городов)...\n")
p_cities <- generate_cities_map_pdf(json_data)
cat("Генерация страниц регионов...\n")
region_plots <- generate_region_pages_pdf(json_data, combined)

pdf_file <- "output/test_report_local.pdf"
dir.create("output", recursive = TRUE, showWarnings = FALSE)
pdf(pdf_file, width = 12, height = 10)

print(p_main)
print(p_cities)
for (p in region_plots) {
  print(p)
}
dev.off()

cat("✅ PDF сохранён в", pdf_file, "\n")
