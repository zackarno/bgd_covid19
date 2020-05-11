
# common ------------------------------------------------------------------

write_csv_output <- c("yes","no")[1]
day_to_run <- Sys.Date()
round_text<- "round 2"

# adolescent --------------------------------------------------------------

rmarkdown::render("Market_Covid_Daily_Monitoring_Report.Rmd")

file_location <- "Market_Covid_Daily_Monitoring_Report.html"
copy_to_drop <- "C:\\Users\\MEHEDI\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\COVID-19\\Market Monitoring Initiative (camps)\\Daily monitoring\\02_daily_reports/"
file.copy(file_location,paste0(copy_to_drop,str_replace_all(day_to_run,"-","_"),"_Market_Monitoring_Daily_Report.html"),overwrite = T)

