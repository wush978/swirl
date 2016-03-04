.tracking_env <- environment()

#'@importFrom httr POST stop_for_status
tracking <- function(ip, courseName, lessonName, userName, version, type) {
  body <- list(
    user_id = userName,
    course = sprintf("%s:%s", courseName, lessonName),
    type = type,
    version = version
  )
  tryCatch({
    # get the last vailable ip or pass
    last_ip <- unique(append(.tracking_env[[ip]], strsplit(ip, ",")[[1]]))
    is_tracked <- FALSE
    .tracking_env$error_msg <- c()
    for(current_ip in last_ip) {
      tryCatch({
        res <- POST(url = sprintf("http://%s:3000/api/status", current_ip), body = body, encode = "json")
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

pass_lesson <- function(ip, courseName, lessonName, userName, version) tracking(ip, courseName, lessonName, userName, version, 1)