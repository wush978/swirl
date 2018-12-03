test_package_version <- function(pkg_name, pkg_version) {
  e <- get("e", parent.frame())
  tryCatch(
    packageVersion(pkg_name) >= package_version(pkg_version),
    error = function(e) FALSE)
}

test_search_path <- function(pkg_name) {
tryCatch(
  length(grep(sprintf("/%s$", pkg_name), searchpaths())) > 0,
  error = function(e) FALSE)
}

check_val <- function(name, value) {
  e <- get("e", parent.frame())
  tryCatch({
    result <- all.equal(get(name, e), value)
    if (!isTRUE(result)) {
      message(result)
      FALSE
    } else TRUE
  }, error = function(e) {
    message(conditionMessage(e))
    FALSE
  })
}

val_is <- function(value) {
  tryCatch({
    e <- get("e", parent.frame())
    result <- all.equal(e$val, value)
    if (!isTRUE(result)) {
      message(result)
      FALSE
    } else TRUE
  }, error = function(e) {
    message(conditionMessage(e))
    FALSE
  })
}

stop_if_not <- function(f) {
  tryCatch({
    e <- get("e", parent.frame())
    result <- f(e$val)
    isTRUE(result)
  }, error = function(e) {
    message(conditionMessage(e))
    FALSE
  })
}

test_all <- function(...) {
  tryCatch({
    e <- get("e", parent.frame())
    calls <- tail(as.list(match.call()), -1)
    for(i in seq_along(calls)) {
      if (!eval(calls[[i]])) stop("")
    }
    TRUE
  }, error = function(e) {
    FALSE
  })
}

check_then_install <- function(pkg_name, pkg_version) {
  if (!suppressWarnings(suppressMessages(require(pkg_name, character.only = TRUE)))) pvm::install.packages.via.graph(pkg_name) else {
    if (packageVersion(pkg_name) < package_version(pkg_version)) pvm::install.packages.via.graph(pkg_name)
  }
}
  
check_then_install_github <- function(pkg_name, pkg_version, ...) {
  if (!require(pkg_name, character.only = TRUE)) devtools::install_github(...) else {
    if (packageVersion(pkg_name) < package_version(pkg_version)) devtools::install_github(..., dependencies = FALSE)
  }
}
  
  
