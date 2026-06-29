FROM rocker/geospatial:latest

# Устанавливаем plumber и jsonlite
RUN install2.r plumber jsonlite

# Копируем проект
WORKDIR /app
COPY . .

# Проверяем наличие RDS-файлов (для диагностики)
RUN ls -la data/rds/

# Открываем порт
EXPOSE 8000

# Запускаем API
CMD ["Rscript", "-e", "plumber::plumb('scripts/api.R')$run(host='0.0.0.0', port=8000)"]
