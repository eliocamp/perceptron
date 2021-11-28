library(magrittr)

download_ersst <- function(file) {
  base_url <- "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/ersst.v5."
  
  years <- 1979:2020
  months <- formatC(1:12, width = 2, flag = "0")
  dates <- data.table::CJ(years, months)[, paste0(years, months)]
  
  urls <- paste0(base_url, dates, ".nc")
  dir.create(file.path("datos", "raw"), showWarnings = FALSE)
  files <- vapply(seq_along(urls), function(u) {
    file <- file.path("datos", "raw", paste0(dates[u], ".nc"))
    
    if (file.exists(file)) {
      return(file)
    }
    
    message(paste0("Descargando ", file))
    download.file(urls[u], file, quiet = TRUE)
    file
  }, character(1))
  
  data <- lapply(files, function(file) metR::ReadNetCDF(file, vars = c(t = "sst")))
  data <- data.table::rbindlist(data)
  data[, time := as.Date(lubridate::floor_date(time, "month"))]
  saveRDS(data, file)
  file
}

dir.create("datos", showWarnings = FALSE)
file <- file.path("datos", "ersst.mon.Rds")


if (!file.exists(file)) {
  download_ersst(file)
}

datos_sst <- function() {
  readRDS(file.path("datos", "ersst.mon.Rds"))
}


cmap_url <- "https://downloads.psl.noaa.gov/Datasets/cmap/std/precip.mon.mean.nc"
cmap_file <- file.path("datos", "raw", "precip.mon.mean.nc")

if (!file.exists(cmap_file)) {
  download.file(cmap_url, cmap_file, mode = "wb")  
}


cmap <- metR::ReadNetCDF(cmap_file, subset = list(lon = c(-60.74, -48)+360,
                                          lat = c(-34.6, -24))) %>% 
  .[, .(precip = weighted.mean(precip, cos(lat*pi/180))), by = .(time = as.Date(time))] %>% 
  .[time %between% as.Date(c("1979-01-01", "2020-12-01"))]

saveRDS(cmap, file.path("datos", "cmap.mon.Rds"))

datos_precip <- function() {
  readRDS(file.path("datos", "cmap.mon.Rds"))
}



