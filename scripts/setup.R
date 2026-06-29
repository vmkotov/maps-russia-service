# scripts/setup.R
library(rnaturalearth)
library(sf)

cat("Создаём combined.rds...\n")
russia <- ne_states(country = "russia", returnclass = "sf")
russia_fixed <- st_shift_longitude(russia)
ukraine <- ne_states(country = "ukraine", returnclass = "sf")
ukraine_fixed <- st_shift_longitude(ukraine)
target <- c("Donetsk", "Luhansk", "Kherson", "Zaporizhzhia")
selected <- ukraine_fixed[ukraine_fixed$name_en %in% target, ]
combined <- rbind(russia_fixed, selected)
saveRDS(combined, "data/rds/combined.rds")
cat("combined.rds готов.\n")

cat("Создаём lakes110.rds...\n")
lakes <- ne_download(scale = 110, type = "lakes", category = "physical", returnclass = "sf")
saveRDS(lakes, "data/rds/lakes110.rds")
cat("lakes110.rds готов.\n")

cat("Извлекаем Енисей из rivers10.rds...\n")
rivers10 <- readRDS("data/rds/rivers10.rds")
enissey <- rivers10[rivers10$name == "Yenisey", ]
saveRDS(enissey, "data/rds/enissey.rds")
cat("enissey.rds готов.\n")

cat("Все файлы созданы!\n")