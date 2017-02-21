.get.servers <- function() {
  servers <- getOption("SWIRL_TRACKING_SERVER_IP", c("http://api.datascienceandr.org", "http://api2.datascienceandr.org"))
  servers <- unlist(strsplit(servers, ","))
  servers <- sample(servers, length(servers), FALSE)
  servers
}