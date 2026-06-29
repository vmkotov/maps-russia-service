FROM rocker/geospatial:latest

RUN install2.r plumber jsonlite

WORKDIR /app
COPY scripts/ scripts/
COPY data/ data/
COPY . .

RUN ls -la scripts/

EXPOSE 8000
CMD ["Rscript", "-e", "plumber::plumb('scripts/api.R')$run(host='0.0.0.0', port=8000)"]
