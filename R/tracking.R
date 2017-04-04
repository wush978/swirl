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
    # ips <- c(ips, paste(ips, "3000", sep = ":"))
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

.get_e <- function() {
  cbs <- .swirl.task.manager$callbacks()
  if (!.SWIRL_CALLBACK_NAME %in% names(cbs)) {
    stop(s()%N%"This function only works under the swirl environment")
  }
  get("e", envir = environment(cbs[[.SWIRL_CALLBACK_NAME]][["f"]]))
}

.get_tracked_usr <- function() {
  e <- .get_e()
  e$userinfo$tracked_usr
}

.get_usr <- function() {
  e <- .get_e()
  e$usr
}

#'@export
my_progress <- function() {
  if (!identical(parent.frame(), globalenv())) return(invisible(NULL))
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

#'@export
upload_my_progress <- function() {
  if (!identical(parent.frame(), globalenv())) return(invisible(NULL))
  servers <- .get.servers()
  e <- .get_e()
  user_id <- .get_tracked_usr()
  if(strsplit(user_id, split = ":", fixed = TRUE)[[1]][1] != "classroom") {
    stop(s()%N%"This function only serves cooperated students")
  }
  # get login info
  account <- gsub("classroom:", "", user_id, fixed = TRUE)
  password <- .get_classroom_password(account)
  # compress object
  usr <- .get_usr()
  plist <- dir(proot <- file.path(progressDir(e), e$usr))
  retval <- lapply(plist, function(path) {
    path <- file.path(proot, path)
    readBin(path, "raw", file.size(path))
  })
  names(retval) <- plist
  saveRDS(retval, file = upload.path <- tempfile(fileext = ".Rds"))
  object <- sprintf("%d-%s", as.integer(Sys.time()), paste(sample(letters, 4, TRUE), collapse = ""))
  servers <- .get.servers()
  for(server in servers) {
    tryCatch({
      cat(sprintf("Uploading... \n"))
      body <- list(account = account, object = object, hmac = digest::hmac(password, object, algo = "sha256"), progress = httr::upload_file(upload.path)) # 
      .r <- httr::POST(sprintf("%s/api/uploadProgress", server), body = body)
      if (httr::status_code(.r) >= 300) {
        .remove_classroom_password(account)
        stop("Invalid account or password")
      } else {
        return(TRUE)
      }
    }, error = function(e) {
      Sys.sleep(1)
      if (conditionMessage(e) == "Invalid account or password") stop(conditionMessage(e))
      warnings(conditionMessage(e))
    })
  }
  .remove_classroom_password(account)
  stop("Failed to upload")
}

.download_my_progress <- function() {
  servers <- .get.servers()
  e <- .get_e()
  user_id <- .get_tracked_usr()
  if(strsplit(user_id, split = ":", fixed = TRUE)[[1]][1] != "classroom") {
    stop(s()%N%"This function only serves cooperated students")
  }
  # get login info
  account <- gsub("classroom:", "", user_id, fixed = TRUE)
  password <- .get_classroom_password(account)
  object <- sprintf("%d-%s", as.integer(Sys.time()), paste(sample(letters, 4, TRUE), collapse = ""))
  servers <- .get.servers()
  for(server in servers) {
    tryCatch({
      cat(sprintf("Downloading... \n"))
      body <- list(account = account, object = object, hmac = digest::hmac(password, object, algo = "sha256")) # 
      .r <- httr::POST(sprintf("%s/api/downloadProgress", server), body = body)
      if (httr::status_code(.r) >= 300) {
        .remove_classroom_password(account)
        stop("Invalid account or password")
      } else {
        return(httr::content(.r, as = "raw"))
      }
    }, error = function(e) {
      Sys.sleep(1)
      if (conditionMessage(e) == "Invalid account or password") stop(conditionMessage(e))
      warnings(conditionMessage(e))
    })
  }
  .remove_classroom_password(account)
  stop("Failed to download")
}

#'@export
download_my_progress <- function() {
  if (!identical(parent.frame(), globalenv())) return(invisible(NULL))
  .raw <- .download_my_progress()
  if (length(.raw) > 0) {
    cat("Updating your local progress...\n")
    retval <- readRDS(gzcon(rawConnection(.raw)))
    e <- .get_e()
    proot <- file.path(progressDir(e), e$usr)
    for(name in names(retval)) {
      # if (name == basename(e$progress)) next
      writeBin(retval[[name]], file.path(proot, name))
    }
  }
  invisible(TRUE)
}