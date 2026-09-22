# test_combined_map.R
# Локальная проверка нового /map

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Укажите путь к JSON-файлу: Rscript scripts/test_combined_map.R data/input/user_12_full.json")
}
json_file <- args[1]

if (!file.exists(json_file)) {
  stop("Файл JSON не найден: ", json_file)
}

# Подгружаем api.R с заменой путей
api_code <- readLines("scripts/api.R")
api_code <- gsub('setwd\\("/app"\\)', 'setwd(".")', api_code)
api_code <- gsub('"/app/data/rds/', '"data/rds/', api_code)
eval(parse(text = api_code), envir = globalenv())

json_data <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)

tmp <- generate_combined_map(json_data)

dir.create("output", showWarnings = FALSE, recursive = TRUE)
out_file <- paste0("output/map_combined_", tools::file_path_sans_ext(basename(json_file)), ".png")
file.copy(tmp, out_file, overwrite = TRUE)
cat("✅ Сохранено:", out_file, "\n")
