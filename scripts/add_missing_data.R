library(osmdata)
library(sf)

# Функция с повторными попытками и уменьшенным bbox
safe_load_osm_v2 <- function(bbox, key, value, name, output_file, retries = 3) {
  cat("Загрузка:", name, "...\n")
  for (i in 1:retries) {
    result <- tryCatch({
      opq(bbox = bbox) %>%
        add_osm_feature(key = key, value = value) %>%
        osmdata_sf()
    }, error = function(e) {
      cat("Попытка", i, "ошибка:", e$message, "\n")
      return(NULL)
    })
    if (!is.null(result)) break
    Sys.sleep(3)
  }
  if (is.null(result)) {
    cat("Не удалось загрузить", name, "после", retries, "попыток\n")
    return(NULL)
  }
  
  obj <- result$osm_multipolygons
  if (nrow(obj) == 0) obj <- result$osm_polygons
  if (nrow(obj) == 0) {
    cat("Нет полигонов для", name, "\n")
    return(NULL)
  }
  
  # Фильтруем по названию (если есть)
  if ("name" %in% names(obj)) {
    obj <- obj[grepl(name, obj$name, ignore.case = TRUE), ]
  }
  if (nrow(obj) == 0) {
    cat("Не найден объект с именем", name, "\n")
    return(NULL)
  }
  
  obj <- st_transform(obj, crs = 4326)
  saveRDS(obj, output_file)
  cat("Сохранено", nrow(obj), "полигонов в", output_file, "\n")
  return(obj)
}

# ---- Загрузка с уменьшенными bbox ----
# Московская область (окрестности Москвы)
bbox_mo <- st_bbox(c(xmin = 35, xmax = 40, ymin = 54, ymax = 57), crs = 4326)
moscow_oblast <- safe_load_osm_v2(bbox_mo, "boundary", "administrative", "Московская область", "data/rds/moscow_oblast_osm.rds")

# Алтайский край (окрестности Барнаула)
bbox_altai <- st_bbox(c(xmin = 78, xmax = 87, ymin = 50, ymax = 54), crs = 4326)
altai_krai <- safe_load_osm_v2(bbox_altai, "boundary", "administrative", "Алтайский край", "data/rds/altai_krai_osm.rds")

# Азовское море (уже узкий)
bbox_azov <- st_bbox(c(xmin = 34, xmax = 39, ymin = 45, ymax = 48), crs = 4326)
azov_sea <- safe_load_osm_v2(bbox_azov, "natural", "water", "Азовское море", "data/rds/azov_sea_osm.rds")

cat("Готово.\n")