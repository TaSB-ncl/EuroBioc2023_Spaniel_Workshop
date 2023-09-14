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
  
