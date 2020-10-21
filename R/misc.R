#' Save objects
#'
#' As devtools::use_data but choosing dest_dir 
#'  
#' @export
mysave <- function (..., dir = ".", overwrite = FALSE,
    compress = "bzip2") {

  objs <- eval(substitute(alist(...)))
  objs <- vapply(objs, as.character, character(1))
  paths <- file.path(dir, paste0(objs, ".rda"))

  if (any(file.exists(paths)) & !overwrite) {
    existing_file <- objs[file.exists(paths)]
    stop(paste0(existing_file, " already exists.\n", "Use overwrite = TRUE."))
  }
  message("Saving ", paste(unlist(objs), collapse = ", "),
    " as ", paste(basename(paths), collapse = ", "), " to ",
    dir)
  envir <- parent.frame()
  mapply(save, list = objs, file = paths, MoreArgs = list(envir = envir,
      compress = compress))
  invisible()
}
myload <- function (..., dir = ".", envir = environment()) {

  objs <- eval(substitute(alist(...)))
  objs <- vapply(objs, as.character, character(1))
  paths <- file.path(dir, paste0(objs, ".rda"))

  if (any(!file.exists(paths))) {
    stop(paste0(existing_file, " does not exist\n"))
  }
  lapply(paths, load, envir)
  invisible()
}

get_mypath <- function (...) {
  rprojroot::find_package_root_file(...)
}

source_dir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "*.R")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

get_replace_tab <- function() {
  c(
    "(Intercept)" = "Intercept",
    "longitude" = "Longitude",
    "latitude" = "Latitude",
    "avAltA" = "Altitude",
    "upDist" = "Spring distance",
    "substratcalc" = "Acid",
    "substratmxt" = "Mixed",
    "latitude:longitude" = "Latitude x Longitude",
    "Value" = "Estimate",
    "Std.Error" = "SE",
    "DF" = "DF",
    "t-value" = "t",
    "p-value" = "P",
    "pvals" = "P"
  )

} 

standardize_P <- function (x) {
  if (x > .05) {
    x <- "NS"
  } else if (.01 < x & x <= .05) {
    x <- "< 0.05"
  } else if (.001 < x & x <= 0.01) {
    x <- "< 0.01"
  } else if (.0001 < x & x <= 0.001) {
    x <- "< 0.001"
  } else {
    x <- "< 0.0001"
  }
  return(x)
}
