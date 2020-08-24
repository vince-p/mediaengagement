
trim <- function(x, variables, cutoff = 3.5, replace = "NA", id = ""){
  col.order <- colnames(x)
  if ("all" %in% variables){
    variables <- colnames(x)[which(colnames(x)!=id)]
  }
  
  x <- center(x, variables = variables, standardize = TRUE)
  
  if (replace=="NA") {
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      x <- dplyr::mutate(x,
                         placeholder = ifelse(get(zscored) > cutoff, NA,
                                              ifelse(get(zscored) < (cutoff*-1), NA, get(i))))
      x <- dplyr::select(x, -(i))
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }
  
  if (replace=="cutoff") {
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      x <- dplyr::mutate(x,
                         placeholder = get(zscored),
                         placeholder.mean = mean(get(i), na.rm = TRUE),
                         placeholder.sd = sd(get(i), na.rm = TRUE),
                         placeholder = ifelse(placeholder > cutoff, placeholder.mean + (placeholder.sd*cutoff),
                                              ifelse(placeholder < (cutoff*-1), placeholder.mean - (placeholder.sd*cutoff), get(i))))
      x <- dplyr::select(x, -(i))
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }
  
  if (replace=="mean"){
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      x <- dplyr::mutate(x,
                         placeholder = get(zscored),
                         placeholder.mean = mean(get(i), na.rm = TRUE),
                         placeholder = ifelse(placeholder > cutoff, placeholder.mean,
                                              ifelse(placeholder < (cutoff*-1), placeholder.mean, get(i))))
      x <- dplyr::select(x, -(i))
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }
  
  if (replace=="median"){
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      x <- dplyr::mutate(x,
                         placeholder = get(zscored),
                         placeholder.median = median(get(i), na.rm = TRUE),
                         placeholder = ifelse(placeholder > cutoff, placeholder.median,
                                              ifelse(placeholder < (cutoff*-1), placeholder.median, get(i))))
      x <- dplyr::select(x, -(i))
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }
  x <- x[col.order]
  return(x)
}

center <- function(x, variables = c(colnames(x)), standardize = FALSE,
                   drop = FALSE, suffix = NULL){
  # Perform this function for each variable specified
  for (variable in colnames(x[variables])){
    # Calculate centered scores using the scale() function
    x <- dplyr::mutate(x,
                       hold = scale(get(variable), center = TRUE,
                                    scale = standardize))
    x$hold <- as.vector(x$hold)
    if (drop == TRUE) {
      x <- x[, -which(colnames(x) == variable)]
    }
    if (standardize == FALSE){
      if (is.null(suffix)) {
        names(x)[which(names(x) == "hold")] <- paste(variable, "_c", sep = "")
      } else {
        names(x)[which(names(x) == "hold")] <- paste(variable, suffix, sep = "")
      }
      
    } else if (standardize == TRUE){
      if (is.null(suffix)) {
        names(x)[which(names(x) == "hold")] <- paste(variable, "_z", sep = "")
      } else {
        names(x)[which(names(x) == "hold")] <- paste(variable, suffix, sep = "")
      }
      
    }
  }
  
  return(x)
}