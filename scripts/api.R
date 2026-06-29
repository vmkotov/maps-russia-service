# scripts/api.R
library(plumber)
library(sf)
library(ggplot2)
library(jsonlite)

source("scripts/generate_from_json.R")

#* @post /map
#* @serializer png
function(req, res) {
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
  
  required <- c("request_id", "client_id", "first_name", "last_name", "regions")
  missing <- setdiff(required, names(body))
  if (length(missing) > 0) {
    res$status <- 400
    return(list(error = paste("Missing fields:", paste(missing, collapse=", "))))
  }
  
  if (!is.data.frame(body$regions) || nrow(body$regions) == 0) {
    res$status <- 400
    return(list(error = "regions must be a non-empty array of objects"))
  }
  
  data_env <- load_data()
  p <- generate_map_from_regions(data_env, body, output_file = NULL)
  
  # Сохраняем во временный файл
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, p, width = 12, height = 10, dpi = 300)
  
  # Читаем и возвращаем бинарный PNG
  readBin(tmp, "raw", n = file.info(tmp)$size)
}