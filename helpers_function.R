# function to download a file with a time interval
slow_download <- function(url, destfile, sleep_time){
  download.file(url = url,
                destfile = destfile,
                method = "wininet",
                mode = "wb")
  Sys.sleep(sleep_time)
}
