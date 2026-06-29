# scripts/api.R
# Plumber API для генерации карты по JSON

library(plumber)
library(sf)
library(ggplot2)
library(jsonlite)

# Источник: используем готовую функцию из generate_from_json.R
source("scripts/generate_from_json.R")

#* @post /map
#* @serializer png
function(req, res) {
  # Читаем тело запроса
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody),
    error = function(e) {
      res$status <- 400
      return(list(error = "Invalid JSON"))
    }
  )
  
  if (is.null(body) || !"regions" %in% names(body)) {
    res$status <- 400
    return(list(error = "Missing 'regions' field"))
  }
  
  # Валидация обязательных полей
  required <- c("request_id", "client_id", "first_name", "last_name", "regions")
  missing <- setdiff(required, names(body))
  if (length(missing) > 0) {
    res$status <- 400
    return(list(error = paste("Missing fields:", paste(missing, collapse=", "))))
  }
  
  # Проверка, что regions — это data.frame (массив объектов)
  if (!is.data.frame(body$regions) || nrow(body$regions) == 0) {
    res$status <- 400
    return(list(error = "regions must be a non-empty array of objects"))
  }
  
  # Строим карту (без сохранения на диск, возвращаем ggplot)
  data_env <- load_data()  # из generate_from_json.R
  p <- generate_map_from_regions(data_env, body, output_file = NULL)  # возвращаем ggplot
  
  # Plumber автоматически сериализует в PNG благодаря @serializer png
  print(p)
}