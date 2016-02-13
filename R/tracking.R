#'@importFrom httr POST stop_for_status
tracking <- function(ip, courseName, lessonName, userName, version, type) {
  body <- list(
    user_id = userName,
    course = sprintf("%s:%s", courseName, lessonName),
    type = type,
    version = version
  )
  tryCatch({
    res <- POST(url = sprintf("http://%s:3000/api/status", ip), body = body, encode = "json")
    stop_for_status(res)
  }, error = function(e) {
    warning(sprintf("Failed to communicate with tracking server. The error message is: (%s)", conditionMessage(e)))
  })
  NULL
}

enter_lesson <- function(ip, courseName, lessonName, userName, version) tracking(ip, courseName, lessonName, userName, version, 0)

pass_lesson <- function(ip, courseName, lessonName, userName, version) tracking(ip, courseName, lessonName, userName, version, 1)