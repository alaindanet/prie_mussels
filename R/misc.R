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

plot_nmds_envfit <- function (
  nmds = NULL,
  envfit = NULL,
  arrow_scale = 3,
  vec_label_dist = .05,
  col_vec_label = "black",
  col_fac_label = "black",
  ...
  ) {

  # Extract site position 
  NMDS = data.frame(MDS1 = nmds$points[,1], MDS2 = nmds$points[,2])
  #vec.sp<-envfit(nmds$points, NMDS.log, perm=1000)

  # Extract vectors and scale eigen values by correlation with ordination: 
  envfit.df<-as.data.frame(envfit$vectors$arrows*sqrt(envfit$vectors$r)* arrow_scale)

  # Replace vector label 
  vectors_replacement <- c("Lat", "Long", "SpringDist", "Alt")
  names(vectors_replacement) <- rownames(envfit.df)
  envfit.df$species<- str_replace_all(rownames(envfit.df), vectors_replacement)

  # Extract factors
  envfit.fac <- as.data.frame(compo_env$factors$centroids)
  # Replace factor label 
  factors_replacement <- c("Acid", "Calc", "Mixed")
  names(factors_replacement) <- rownames(envfit.fac)
  envfit.fac$substrate <- str_replace_all(rownames(envfit.fac), factors_replacement)

  g <- ggplot(data = NMDS, aes(MDS1, MDS2)) +
    geom_point()+
    geom_segment(data = envfit.df,
      aes(x = 0, xend = NMDS1 ,y = 0, yend = NMDS2),
      arrow = arrow(length = unit(0.5, "cm")), colour = "blue", size = 1, inherit.aes = FALSE) + 
    geom_text(data = envfit.fac,
      aes(x = NMDS1, y = NMDS2, label = substrate),
      size = 5, color = col_fac_label) +
    geom_text(data = envfit.df, aes(
	x = NMDS1,
	y = ifelse(NMDS2< 0, NMDS2 - vec_label_dist, NMDS2 + vec_label_dist),
	label = species),
      size = 5,
      color = col_vec_label)+
    coord_fixed() +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_bw()
  return(g)

}
