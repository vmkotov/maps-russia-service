FROM rocker/geospatial:latest

RUN install2.r plumber jsonlite ggrepel showtext sysfonts showtextdb

WORKDIR /app
COPY scripts/ scripts/
COPY data/ data/
COPY . .

RUN ls -la scripts/

EXPOSE 8000
CMD ["Rscript", "-e", "plumber::plumb('scripts/api.R')$run(host='0.0.0.0', port=8000)"]
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Устанавливаем шрифт Liberation Sans для кириллицы
RUN apt-get update && apt-get install -y fonts-liberation && rm -rf /var/lib/apt/lists/*

# Устанавливаем шрифт DejaVu Sans для кириллицы
RUN apt-get update && apt-get install -y fonts-dejavu-core && rm -rf /var/lib/apt/lists/*
