library(sf)
library(ggplot2)
library(jsonlite)
library(showtext)
library(sysfonts)

# Шрифт
font_paths <- c(
  "/System/Library/Fonts/Helvetica.ttc",
  "/System/Library/Fonts/Arial.ttf",
  "/Library/Fonts/Arial.ttf"
)
for (f in font_paths) {
  if (file.exists(f)) {
    font_add("liberation", regular = f)
    break
  }
}
showtext_auto()

# Загрузка данных
combined <- readRDS("data/rds/combined_split.rds")
rivers <- readRDS("data/rds/russian_rivers.rds")
lakes <- readRDS("data/rds/selected_lakes.rds")
azov <- readRDS("data/rds/azov_sea.rds")
combined <- combined[!duplicated(combined$name_en), ]

json_data <- fromJSON("data/input/user_2_full.json", simplifyVector = FALSE)

# Создаём regions_df только для посещённых регионов
regions_df <- data.frame(
  region_name_en = character(),
  district_name = character(),
  stringsAsFactors = FALSE
)
for (dist in json_data$districts) {
  dist_name <- dist$district_name
  for (reg in dist$regions) {
    if (!is.null(reg$region_visited) && reg$region_visited) {
      regions_df <- rbind(regions_df, data.frame(
        region_name_en = reg$region_name_en,
        district_name = dist_name,
        stringsAsFactors = FALSE
      ))
    }
  }
}

dist_dict <- setNames(regions_df$district_name, regions_df$region_name_en)
combined$district_visited <- NA_character_
for (i in 1:nrow(combined)) {
  name <- combined$name_en[i]
  if (name %in% names(dist_dict)) {
    combined$district_visited[i] <- dist_dict[name]
  }
}

# Преобразуем в фактор с уровнями всех округов
all_districts <- c(
  "Дальневосточный", "Приволжский", "Северо-Западный",
  "Северо-Кавказский", "Сибирский", "Уральский",
  "Центральный", "Южный"
)
combined$district_visited <- factor(combined$district_visited, levels = all_districts)

district_colors <- c(
  "Дальневосточный"      = "#E63946",
  "Приволжский"          = "#F4A261",
  "Северо-Западный"      = "#2A9D8F",
  "Северо-Кавказский"    = "#9B5DE5",
  "Сибирский"            = "#F9C74F",
  "Уральский"            = "#F4845F",
  "Центральный"          = "#B5838D",
  "Южный"                = "#E5989B"
)

p <- ggplot() +
  geom_sf(data = combined, color = NA, size = 0, aes(fill = district_visited)) +
  scale_fill_manual(
    values = district_colors,
    na.value = "#E8E8E8",
    name = "Федеральный округ",
    drop = FALSE,
    na.translate = FALSE,
    limits = all_districts  # <-- добавлено
  ) +
  geom_sf(data = rivers, color = "#00BFFF", size = 0.5, fill = NA) +
  geom_sf(data = combined, color = "#2E4053", size = 0.3, fill = NA) +
  geom_sf(data = lakes, fill = "#00BFFF", color = "#00BFFF", size = 0.2, alpha = 1) +
  geom_sf(data = azov, fill = "#00BFFF", color = "#00BFFF", size = 0.2, alpha = 0.7) +
  coord_sf() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

pdf("output/debug_map.pdf", width = 12, height = 10)
print(p)
dev.off()
cat("✅ debug_map.pdf обновлён\n")
