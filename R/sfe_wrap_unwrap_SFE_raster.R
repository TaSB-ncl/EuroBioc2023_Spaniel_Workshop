###############################################################################
#' Extract each image separately
#'
#' @param sample_id 
#' @param sfe 
#'
#' @return
#' @export
#'
#' @examples
singleImg <- function(sample_id, sfe){
  sr <- SpatialExperiment::getImg(sfe, sample_id)@image
  return(sr)
}

###############################################################################
#' Create SpatRasterImage from SpatRaster
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples

newSpatRasterImage <- function(x){
  new("SpatRasterImage",image = x)}


###############################################################################
#' Wrap SpatRaster objects so that they can be reloaded
#'
#' @param sfe 
#'
#' @return
#' @export
#'
#' @examples
wrapSFE <- function(sfe){
  if(length(imgData(sfe)$sample_id > 1)){
  SRI_list <- lapply(imgData(sfe)$sample_id, singleImg, sfe)
  wrapped_SRI_list <- SRI_list %>% lapply(terra::wrap) #%>% lapply(PackedSpatRasterImage)
  sfe@int_metadata$imgData@listData$data <- wrapped_SRI_list
  }
  if(length(imgData(sfe)$sample == 1)){
    sr <- SpatialExperiment::getImg(sfe)@image
    wrapped_SRI <- terra::wrap(sr) #%>% new("PackedSpatRasterImage", image = .)
    sfe@int_metadata$imgData@listData$data <- list(wrapped_SRI)
   }
return(sfe)
}


###############################################################################
#' Unwrap SFE
#'
#' @param sfe 
#'
#' @return
#' @export
#'
#' @examples
unwrapSFE <- function(sfe){
sfe@int_metadata$imgData@listData$data <- 
  sfe@int_metadata$imgData@listData$data %>% 
  lapply(terra::unwrap) %>% lapply(newSpatRasterImage)
return(sfe)
}


