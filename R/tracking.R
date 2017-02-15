.tracking_env <- new.env(parent=emptyenv())

#'@importFrom httr POST stop_for_status content
tracking <- function(ip, courseName, lessonName, userName, version, type, log = list()) {
  body <- list(
    user_id = userName,
    course = sprintf("%s:%s", courseName, lessonName),
    type = type,
    version = version,
    log = log
  )
  tryCatch({
    # get the last vailable ip or pass
    ips <- strsplit(ip, ",")[[1]]
    ips <- c(ips, paste(ips, "3000", sep = ":"))
    last_ip <- unique(append(.tracking_env[[ip]], sample(ips, length(ips))))
    is_tracked <- FALSE
    .tracking_env$error_msg <- c()
    for(current_ip in last_ip) {
      tryCatch({
        res <- POST(url = sprintf("http://%s/api/status", current_ip), body = body, encode = "json")
        stop_for_status(res)
        is_tracked <- TRUE
        break
      }, error = function(e) {
        .tracking_env$error_msg <- append(.tracking_env$error_msg, sprintf("%s : %s", current_ip, conditionMessage(e)))
      })
    }
    if (is_tracked) {
      message(sprintf("Your status has beed updated to tracking server"))
      .tracking_env[[ip]] <- current_ip
    } else {
      stop(paste(.tracking_env$error_msg, collapse = "---"))
    }
  }, error = function(e) {
    warning(sprintf("Failed to communicate with tracking server. The error message is: (%s)", conditionMessage(e)))
  })
  NULL
}

enter_lesson <- function(ip, courseName, lessonName, userName, version) tracking(ip, courseName, lessonName, userName, version, 0)

pass_lesson <- function(ip, courseName, lessonName, userName, version, log) tracking(ip, courseName, lessonName, userName, version, 1, log)

#'@export
query_user_id <- function(user_id) {
  serverIP <- getOption("SWIRL_TRACKING_SERVER_IP", NULL)
  if (!is.null(serverIP)) {
    tryCatch({
      ips <- strsplit(serverIP, ",")[[1]]
      urls <- sprintf("http://%s/api/getRecordsByUserId", ips)
      body <- list(user_id = user_id)
      records <- lapply(urls, function(url) {
        res <- POST(url = url, body = body, encode = "json")
        stop_for_status(res)
        content(res)
      })
      tmp <- Filter(function(x) "type" %in% names(x), unlist(records, recursive = FALSE))
      tmp <- Filter(function(x) x$type == 1, tmp)
      sapply(tmp, "[[", "course")
    }, error = function(e) {
      stop(sprintf("The connection to the tracking server is down. Please report the following message to the chatroom: %s", conditionMessage(e)))
    })
  } else stop("The ip of the tracking server is empty. You need to enter the course first.")
}
