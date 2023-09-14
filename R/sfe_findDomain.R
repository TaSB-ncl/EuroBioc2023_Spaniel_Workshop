#' Convert SP to SF object
#'
#' @param sp 
#'
#' @return
#' @export
#'
#' @examples
SPToSF <- function(sp, sampleName){
  ##convert sp to sf
  sf <- sp::geometry(sp) %>% as("sf")
  sf$sample_id <- sampleName
  return(sf)
 }

#' Convert sf object to sp object
#'
#' @param sf 
#'
#' @return
#' @export
#'
#' @examples
SFToSP <- function(sf){
  sp <- sf %>% as("Spatial") %>% sp::geometry()
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



################################################################################
#' Convert image to spatial polygon
#'
#' @param imgFile 
#'
#' @return
#' @export
#'
#' @examples
imageToCoords <- function(imgFile, cln, fll){
  ## domain coordinates
  im <- imager::load.image(imgFile)
  im <- imager::grayscale(im)
  p0 <- plot(im)
  
  im <- imager::clean(im,cln) %>% imager::fill(fll)
  plot(im)
  hl <- imager::highlight(im)
  
  
  coords <- lapply(hl, function(coord){data.frame(coord$x, coord$y)})
  p1s <- lapply(coords, sp::Polygon)  %>% sp::Polygons(3)
  domain_sp <- sp::SpatialPolygons(list(p1s))
  return(domain_sp)
  
}

################################################################################
#' Assign spots to histological domain
#'
#' @param domain_sp 
#' @param spot_sp 
#' @param sfe 
#'
#' @return
#' @export
#'
#' @examples
domainToColData <- function(domain_sp, spot_sp, sfe, domain_name, sample_id){
  domain_spots <- sp::over(spot_sp, domain_sp) %>% 
    is.na() %>% 
    ifelse(yes = "", no = domain_name)
  
  
  ## add a new column to metadata, only if doesn't already exist
  if(!domain_name %in% colnames(colData(sfe))){
    colData(sfe)[, domain_name] <- ""
  }
  
  colData(sfe)[sfe$sample_id == sample_id, domain_name] <-  domain_spots
  
  return(sfe)
  
}



#' Title
#'
#' @param sfe 
#' @param domain_names 
#'
#' @return
#' @export
#'
#' @examples
#domains <- annoInfo$domain[annoInfo$sample_id == "C01"]
combineDomains <- function(sfe, domain_names){
  domain_names <- domain_names %>% gsub("-", "\\.", .)
  domains <- colData(sfe) %>% 
    data.frame() %>% 
    dplyr::select(all_of(domain_names)) %>% 
    apply(1 , paste, collapse = "")
  
  ## remove overlapping domains
  domain_names <- domain_names %>% gsub("\\.", "-", .)
  domains[!domains %in% domain_names] <- NA
  
  
  sfe$domain <- domains
  return(sfe)
  
}




################################################################################
#' FindDomain
#' 
#' This function converts painted histological domains created in Image 
#' processing software (eg Amira, or Photoshops) and converts them into 
#' spatial polygons.
#'  
#'
#' @param imgFile 
#' @param cln 
#' @param fll 
#' @param domainName 
#' @param sampleName 
#' @param sfe 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' j <- 1
#' domain_name <- annotations_df$domain[j]
#' img_file <- file.path(annotationDir,annotations_df$jpg[j])
#' sfe <- domainToSFE(img_file, domain_name, "C01", sfe)


domainToSFE <-  function(imgFile, 
                         domainName, 
                         sampleName, 
                         sfe, cln = 3, fll = 12){
 # convert spot coordinates back to sp
  spot_sf <- annotGeometry(sfe, type = "spots", sample_id = sampleName)
  spot_sp <- SFToSP(spot_sf)
  
  domain_sp <- imageToCoords(imgFile, cln, fll)
  
  
  ## add to sfe geometries
  annotGeometry(sfe,
               sample_id = sampleName,
               type = domainName) <- SPToSF(domain_sp, sampleName)
  
  ## add to coldata
  sfe <- domainToColData(domain_sp, spot_sp, sfe, domainName, sampleName)
  
  
  return(sfe)
}

################################################################################
#' Find all domains
#' 
#' This function finds mulitple histological domains. It takes an annotation 
#' data frame and sfe as input. 
#'
#' @param annoInfo a data frame with three columns. Thei first column must 
#' contain a sample_id which matches with sample_ids in the sfe object, 
#' the second column must contain a histological domain name name, 
#' the third column must contain the image annotation file name. 
#' 
#' @param sfe 
#' @param cln 
#' @param fll 
#'
#' @return
#' @export
#'
#' @examples
findAllDomains <- function(sfe, annoInfo, annotationDir, cln = 3, fll = 12){
  
  for (i in 1:nrow(annoInfo)){
    
    
    sample_id <- annoInfo[i, 1]
    domain <- annoInfo[i, 2]
    image_name <- annoInfo[i, 3]
    
    img_file <- file.path(annotationDir, image_name)
    
    sfe <- domainToSFE(imgFile = img_file, 
                       domainName = domain, 
                       sampleName = sample_id, 
                       sfe, 
                       cln = 3,
                       fll = 12)
  }
  return(sfe)
  
}




