library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(),
  host     = "aws-1-eu-north-1.pooler.supabase.com",
  port     = 5432,
  dbname   = "postgres",
  user     = "postgres.twmbrcbnofrkntblmqkm",
  password = "9OBLWWCCiiIP0m8d",
  sslmode  = "require"
)

query <- "
SELECT 
  c.id,
  c.name AS city_name,
  r.name AS region_name,
  c.lat,
  c.lon
FROM vkotov_russian_city_bot.dim_city c
JOIN vkotov_russian_city_bot.dim_region r ON c.region_id = r.id
WHERE c.name LIKE '%Сунжа%'
"

result <- dbGetQuery(con, query)
dbDisconnect(con)

print(result)