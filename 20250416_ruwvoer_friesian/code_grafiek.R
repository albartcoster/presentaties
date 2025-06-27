library(RODBC)
library(RPostgres)
library(tidyr)
library(dplyr)
library(dotenv)
library(stringr)
library(openxlsx)
library(openxlsx2)
library(echarts4r)
library(lubridate)
path <- "../../../punt_envs"

load_dot_env(file.path(path,".env_postgres"))
FARM <- 'Milchhof Friesian'

for(VAR in c(
  'PG_HOST',
  'PG_DB',
  'PG_USER',
  'PG_PWD')){
  assign(VAR, Sys.getenv(VAR))
  if(get(VAR) == '') stop(paste0('Missing ', VAR))
}

if(!exists('pgdb')){
  pgdb <- dbConnect(Postgres(), host = PG_HOST, dbname = PG_DB, user = PG_USER, pass = PG_PWD)
  quer_pg <- function(...) dbGetQuery(pgdb, str_glue(paste0(...)))
  pgex <- function(...) dbExecute(pgdb, paste0(...))
}

farms_id <- as.integer(quer_pg("select id from farms where lower(name) = '{tolower(FARM)}'")[1,1])
if(is.na(farms_id)){
  stop(str_glue('Insert failed. Create farm first.'))
}

tab <- quer_pg('select farms_id,date_time,reg_id,lactation,dim,production from view_productions where farms_id = {farms_id}')

tab <- tab |> 
  mutate(
    lactation = as.numeric(lactation),
    dim = as.numeric(dim),
    production = as.numeric(production),
    week = floor_date(date_time,unit = "day",week_start = 1))|> 
  mutate(
    nlactcat = factor(ifelse(lactation>3,3,lactation),
                      levels = 1:3,
                      labels = c("1 Lakt","2 Lakts","3+ Lakts")),    ,
    dilcat = cut(dim,breaks = c(-1,30,60,120,300,2000),
                 labels = c("0-30 DIM","31-60 DIM",'60-120 DIM',"120-300 DIM","300+ DIM"))) |> 
  filter(production>0)



t1 <- tab |>
  filter(dim<1000&dim>=0) |>
  group_by(dilcat,week) |>
  summarise(production = round(mean(production,na.rm = T),2))



ts <- t1 |> 
  ungroup() |> 
  group_by(dilcat) |> 
  e_charts(x = week) |> 
  e_datazoom(
    type = "slider",
    toolbox = FALSE,
    bottom = -5) |> 
  e_tooltip() |> 
  e_title("Leistung") |> 
  e_x_axis(week, axisPointer = list(show = TRUE)) |> 
  e_scatter(production)

save(ts,file = "timeseries")
