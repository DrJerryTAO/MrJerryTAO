# Functions to accommodate different axis scales in facets, adapted from
# https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/

scale_override <- function(which, scale) {
  if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) { 
    stop("which must be an integer of length 1") }
  if(is.null(scale$aesthetics) || !any(
    c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale") }
  structure(list(which = which, scale = scale), class = "scale_override")
}

facet_wrap_MrJerryTAO <- function(..., scale_overrides = NULL) {
  
  facet_super <- facet_wrap(...) # Keep other parameters as facet_wrap
  
  # Override scales
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides) } 
  else if(!is.list(scale_overrides) || !all(vapply(
    scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop(paste(
      "scale_overrides must be a scale_override object", 
      " or a list of scale_override objects")) }
  facet_super$params$scale_overrides <- scale_overrides
  
  CustomFacetWrap <- ggproto(
    "CustomFacetWrap", FacetWrap,
    init_scales = function
    (self, layout, x_scale = NULL, y_scale = NULL, params) {
      # Create initial x, y scales
      scales <- ggproto_parent(
        FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)
      if(is.null(params$scale_overrides)) return(scales)
      max_scale_x <- length(scales$x)
      max_scale_y <- length(scales$y)
      
      # Modify scales$x and scales$y 
      for(scale_override in params$scale_overrides) {
        which <- scale_override$which
        scale <- scale_override$scale
        
        if("x" %in% scale$aesthetics) {
          if(!is.null(scales$x)) {
            if(which < 0 || which > max_scale_x) 
              stop("Invalid index of x scale: ", which)
            scales$x[[which]] <- scale$clone() }} 
        else if("y" %in% scale$aesthetics) {
          if(!is.null(scales$y)) {
            if(which < 0 || which > max_scale_y) 
              stop("Invalid index of y scale: ", which)
            scales$y[[which]] <- scale$clone() } } 
        else {stop("Invalid scale")} }
      scales # return scales
    }
  )
  # Output as a ggproto object
  ggproto(
    NULL, CustomFacetWrap, 
    shrink = facet_super$shrink, params = facet_super$params)
}



facet_grid_MrJerryTAO <- function(..., scale_overrides = NULL) {
  
  facet_super <- facet_grid(...)  # Keep other parameters as facet_wrap
  
  # Override scales
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides) } 
  else if(
    !is.list(scale_overrides) ||
    !all(vapply(
      scale_overrides, inherits, 
      "scale_override", FUN.VALUE = logical(1))) ) {
    stop(paste(
      "scale_overrides must be a scale_override object", 
      "or a list of scale_override objects")) }
  facet_super$params$scale_overrides <- scale_overrides
  
  CustomFacetGrid <- ggproto(
    "CustomFacetGrid", FacetGrid,
    init_scales = function
    (self, layout, x_scale = NULL, y_scale = NULL, params) {
      # Create initial x, y scales
      scales <- ggproto_parent(FacetGrid, self)$init_scales(
        layout, x_scale, y_scale, params)
      if(is.null(params$scale_overrides)) return(scales)
      max_scale_x <- length(scales$x)
      max_scale_y <- length(scales$y)
      
      # Modify scales$x and scales$y based on params$scale_overrides
      for(scale_override in params$scale_overrides) {
        which <- scale_override$which
        scale <- scale_override$scale
        
        if("x" %in% scale$aesthetics) {
          if(!is.null(scales$x)) {
            if(which < 0 || which > max_scale_x) 
              stop("Invalid index of x scale: ", which)
            scales$x[[which]] <- scale$clone() } } 
        else if("y" %in% scale$aesthetics) {
          if(!is.null(scales$y)) {
            if(which < 0 || which > max_scale_y) 
              stop("Invalid index of y scale: ", which)
            scales$y[[which]] <- scale$clone() } } 
        else {stop("Invalid scale")}
      }
      scales # return scales
    } # add scales
  ) # generate a new ggproto object
  # Output as a ggproto object
  ggproto(
    NULL, CustomFacetGrid, 
    shrink = facet_super$shrink, params = facet_super$params)
}