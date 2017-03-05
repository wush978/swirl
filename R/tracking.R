.tracking_env <- new.env(parent=emptyenv())

#'@importFrom httr POST stop_for_status content
tracking <- function(ip, courseName, lessonName, userName, version, type, userinfo = list(), log = list()) {
  body <- list(
    user_id = userName,
    course = sprintf("%s:%s", courseName, lessonName),
    type = type,
    version = version,
    userinfo = userinfo,
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

enter_lesson <- function(ip, courseName, lessonName, userName, version, userinfo) {
  tracking(ip, courseName, lessonName, userinfo$tracked_usr, version, 0, userinfo)
}

pass_lesson <- function(ip, courseName, lessonName, userName, version, userinfo, log) {
  tracking(ip, courseName, lessonName, userinfo$tracked_usr, version, 1, userinfo, log)
}

.get_tracked_usr <- function() {
  cbs <- .swirl.task.manager$callbacks()
  if (!.SWIRL_CALLBACK_NAME %in% names(cbs)) {
    stop(s()%N%"This function only works under the swirl environment")
  }
  e <- get("e", envir = environment(cbs[[.SWIRL_CALLBACK_NAME]][["f"]]))
  e$userinfo$tracked_usr
}

#'@export
my_progress <- function() {
  servers <- .get.servers()
  user_id <- .get_tracked_usr()
  for(server in servers) {
    tryCatch({
      urls <- sprintf("http://%s/api/getRecordsByUserId", server)
      body <- list(user_id = user_id)
      records <- lapply(urls, function(url) {
        res <- POST(url = url, body = body, encode = "json")
        stop_for_status(res)
        content(res)
      })
      tmp <- Filter(function(x) "type" %in% names(x), unlist(records, recursive = FALSE))
      tmp <- Filter(function(x) x$type == 1, tmp)
      return(sapply(tmp, "[[", "course"))
    }, error = function(e) {
      stop(sprintf("The connection to the tracking server is down. Please report the following message to the chatroom: %s", conditionMessage(e)))
    })
  }
}
