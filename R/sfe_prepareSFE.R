#' Add additional metadata to sfe
#' This function adds additional sample information to the colData slot of an 
#' exisiting SFE object. The function requires a SFE object and a 
#' sample information dataframe which must include a column named "sample_id" 
#' with ids that have been added to the sfe object
#'
#' @param sfe 
#' @param sampleInfo 
#'
#' @return
#' @export
#'
#' @examples

additionalMD <- function(sfe, sampleInfo){
  colData(sfe) <- colData(sfe) %>% 
  as.data.frame() %>% 
  dplyr::left_join(sampleInfo) %>% DataFrame()
  return(sfe)
}


#' Add spot positions to sfe object
#'
#' @param sampleName 
#' @param sfe 
#'
#' @return
#' @export
#'
#' @examples
#' spotToSFE("sample03", sfe)
#' 
spotToSFE <- function(sfe, sampleName){
  ## get spot positions
  spots_include <- sfe$sample_id == sampleName
  spots <- sfe[,spots_include] %>% 
    spatialCoords() %>% 
    data.frame() 
  colnames(spots) <- c("Image_X", "Image_Y")
  
  ## use scale factors to determine pixel position
  scaleFactor <- SpatialFeatureExperiment::imgData(sfe)[imgData(sfe)$sample_id 
                                                        == sampleName, 
                                                        "scaleFactor"]
  spots$pixel_x <- spots$Image_X * scaleFactor
  spots$pixel_y <- spots$Image_Y * scaleFactor
  spot_sp <- spots  %>%
    dplyr::select(pixel_x, pixel_y) %>%
    as.matrix() %>%  
    sp::SpatialPoints()
  
  
  ## add to sfe
  annotGeometry(sfe, 
                sample_id = sampleName, type = "spots") <- SPToSF(spot_sp, 
                                                                  sampleName)
  
  
  return(sfe)
}

#' Add spots for all all sections
#'
#' @param sfe 
#' @param sampleInfo 
#'
#' @return
#' @export
#'
#' @examples
allSpots <- function(sfe, sampleInfo){
  
  for (sample_id in sampleInfo$sample_id){
    sfe <- spotToSFE(sfe, sample_id)
  }
  return(sfe)
}






#' Add additional metadata and spot positions to data
#' This is a wrapper function for the additionalMD function and 
#' s
#'
#' @param sfe 
#' @param sampleInfo 
#'
#' @return
#' @export
#'
#' @examples


prepareSFE <- function(sfe, sampleInfo){
  ##add additional MD
  sfe <- additionalMD(sfe, sampleInfo)
  
  ## add spots
  sfe <- allSpots(sfe, sampleInfo)
  return(sfe)
}




#' Add annotation information to SFE object
#'
#' @param sfe 
#' @param annoInfo 
#' @param annoDir 
#'
#' @return
#' @export
#'
#' @examples
addAnnotations <- function(sfe, annoInfo, annotationDir){
  
  for (i in 1:nrow(annoInfo)){
  
  sample_id <- annoInfo$sample_id[i]
  domain_name <- annoInfo$domain[i]
  image_name <- annoInfo$jpg[i]
  img_file <- file.path(annotationDir, image_name)
  
  sfe <- domainToSFE(img_file, 
                     domain_name, 
                     sample_id, 
                     sfe, 
                     cln = 3,
                     fll = 12)
  
  }
  
  return(sfe)
}
  
