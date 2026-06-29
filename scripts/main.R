library(rnaturalearth)
library(sf)
library(ggplot2)

setwd("/Users/vyacheslavkotov/github/R_PROGRAMING_1/MAPS_RUSSIA_1.0")

dir.create("data/rds", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

# ---- Регионы ----
russia <- ne_states(country = "russia", returnclass = "sf")
russia_fixed <- st_shift_longitude(russia)

ukraine <- ne_states(country = "ukraine", returnclass = "sf")
ukraine_fixed <- st_shift_longitude(ukraine)
target <- c("Donetsk", "Luhansk", "Kherson", "Zaporizhzhia")
selected <- ukraine_fixed[ukraine_fixed$name_en %in% target, ]
combined <- rbind(russia_fixed, selected)
saveRDS(combined, "data/rds/combined.rds")

# ---- Енисей (если rivers10.rds уже есть, читаем, иначе скачиваем) ----
if (file.exists("data/rds/rivers10.rds")) {
  rivers10 <- readRDS("data/rds/rivers10.rds")
} else {
  rivers10 <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
  saveRDS(rivers10, "data/rds/rivers10.rds")
}
enissey <- rivers10[rivers10$name == "Yenisey", ]
saveRDS(enissey, "data/rds/enissey.rds")

# ---- Волга (из rivers10 или rivers50) ----
volga <- rivers10[grepl("Volga", rivers10$name, ignore.case = TRUE), ]
saveRDS(volga, "data/rds/volga.rds")

# ---- Озёра (Байкал, Ладога, Онега) ----
# Попробуем загрузить масштаб 50, если нет — 110
if (!file.exists("data/rds/lakes50.rds")) {
  tryCatch({
    lakes50 <- ne_download(scale = 50, type = "lakes", category = "physical", returnclass = "sf")
    saveRDS(lakes50, "data/rds/lakes50.rds")
  }, error = function(e) {
    message("Не удалось загрузить озёра масштаба 50, пробуем 110")
    lakes110 <- ne_download(scale = 110, type = "lakes", category = "physical", returnclass = "sf")
    saveRDS(lakes110, "data/rds/lakes110.rds")
  })
} else {
  lakes50 <- readRDS("data/rds/lakes50.rds")
}

# Выбираем нужные озёра по названию
if (exists("lakes50")) {
  lakes <- lakes50
} else if (file.exists("data/rds/lakes110.rds")) {
  lakes <- readRDS("data/rds/lakes110.rds")
} else {
  stop("Не удалось загрузить озёра")
}

selected_lakes <- lakes[grepl("Baikal|Ladoga|Onega", lakes$name, ignore.case = TRUE), ]
saveRDS(selected_lakes, "data/rds/selected_lakes.rds")

# ---- Построение карты ----
p <- ggplot() +
  geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = "gray98") +
  geom_sf(data = enissey, color = "dodgerblue", size = 0.4, fill = NA) +
  geom_sf(data = volga, color = "dodgerblue", size = 0.4, fill = NA) +
  geom_sf(data = selected_lakes, fill = "dodgerblue", color = "dodgerblue", size = 0.2, alpha = 0.7) +
  coord_sf() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Россия, области Украины, Енисей, Волга и озёра")

print(p)
ggsave("output/plots/map_final.png", p, width = 12, height = 10, dpi = 300)

cat("Готово! Карта сохранена в output/plots/map_final.png\n")