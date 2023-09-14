#' @import SpatialExperiment
#' @import SpatialFeatureExperiment
#' @import tidyterra
#' @import ggplot2
#' @import cols4all
#' 
NULL

################################################################################

#' getCoordinatesSFE
#'
#' @param sfe 
#' @param sample_id 
#' @param sr_rotated 
#'
#' @return
#' @export
#'
#' @examples
getCoordinatesSFE <- function(sfe, sample_id, sr_rotated){
  ## pixel coordinates from sfe
  coords_sample <- spatialCoords(sfe[, sfe$sample_id == sample_id]) %>% 
    data.frame() 
  
  ## get x and y coordinates
  coords_sample$X <- coords_sample[,1]
  coords_sample$Y <- coords_sample[,2]
  
  ## rev y coordinates for plotting
  extent_y <- terra::ext(sr_rotated)$ymin + terra::ext(sr_rotated)$ymax
  coords_sample$Y <-  coords_sample$Y -  extent_y 
  return(coords_sample)
}

################################################################################






################################################################################
#' Format plot
#' 
#' Removes grid lines, background and ticks from plot
#'
#' @param p 
#'
#' @return
#' @export
#'
#' @examples
plotFormatting <- function(p){
  p2 <- p + 
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.ticks.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks.y=ggplot2::element_blank(),
                   panel.border = element_blank(), 
                   panel.background = element_blank(), 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank()) 
    
  return(p2)
}


################################################################################
#' Add Plot Feature
#' 
#' Add plot feature to coordinates
#'
#' @param coords_sample 
#' @param plot_type 
#' @param plot_feat 
#' @param sample_id 
#'
#' @return
#' @export
#'
#' @examples
addPlotFeatureSFE <- function(sfe, coords_sample, plot_type, plot_feat, sample_id){
  if(plot_type == "gene"){
    print("gene")
    ## if gene plot join with counts
    coords_sample[,plot_feat] <- logcounts(sfe)[rowData(sfe)$symbol == plot_feat,
                                                sfe$sample_id == sample_id]  
  } else{
    print("metadata")
    ## else if metdata plot join with col data
    coords_sample[,plot_feat] <- colData(sfe)[sfe$sample_id == sample_id,
                                              plot_feat] 
  }
  return(coords_sample)
}

################################################################################
#' Spaniel Plot SFE
#'
#' @param sfe 
#' @param plot_type 
#' @param plot_feat 
#' @param sample_id 
#'
#' @return
#' @export
#'
#' @examples
spanielPlot_SFE <- function(sfe, 
                            plot_type = "metadata", 
                            plot_feat, 
                            sample_id, 
                            ptSize = 2,
                            ptSizeMin = 0, 
                            ptSizeMax = 5, 
                            colLow = "#ff3300", 
                            colHigh = "#ffff00"
                            ){


### get spatraster
if (class(getImg(sfe, sample_id)@image) == "PackedSpatRaster"){
  sr <- getImg(sfe, sample_id)@image %>% terra::unwrap()
}else{
  sr <- getImg(sfe, sample_id)@image
}

sr_rotated <- terra::flip(sr)

## get sample coordinates
coords_sample <- getCoordinatesSFE(sfe, sample_id, sr_rotated)

print("got coordinates")

## add plot feature
coords_sample <- addPlotFeatureSFE(sfe,
                                   coords_sample, 
                                   plot_type, 
                                   plot_feat, 
                                   sample_id)

print("added plot feature")




## check if data is discrete or discrete
if(class(coords_sample[,plot_feat]) %in% c("logical", "character", "factor")){
  print("discrete")
  ## discrete
  p1 <- ggplot2::ggplot(coords_sample, aes(colour = !! ensym(plot_feat))) +
    geom_spatraster_rgb(data = sr_rotated) + 
    scale_y_reverse() +
    geom_point(aes(X, Y), alpha = 0.6)
} else {
  print("continuous")
  ## continuous
  ## remove 0 values for plotting
  filter <- coords_sample[, plot_feat ] == 0
  coords_sample[filter,plot_feat] <- NA
  
  p1 <- ggplot2::ggplot(coords_sample, aes(colour = !! ensym(plot_feat), 
                                  size = !! ensym(plot_feat))) +
    geom_spatraster_rgb(data = sr_rotated) + 
    scale_y_reverse() +
    geom_point(aes(X, Y), alpha = 0.6, ) +
    ggplot2::scale_colour_gradient(low=colLow, high=colHigh) + 
    ggplot2::scale_size(range = c(ptSizeMin, ptSizeMax)) 
}

## format plot (remove guides and ticks)
p2 <- plotFormatting(p1)


return(p2)
}

# sfe <- readRDS("testData/rObjects/wrapped_sfe.rds") %>% unwrapSFE()
# sample_id = "C01"
# plot_feat = "clust"
# # 
# ## test plots work for different types
# spanielPlot_SFE(sfe,
#                 sample_id = "C01",
#                 plot_feat = "clust")
# 
# spanielPlot_SFE(sfe,
#                 sample_id = "C01",
#                 plot_feat = "subsets_mito_percent", ptSizeMin = 0,
#                 ptSizeMax = 2)
# 
# spanielPlot_SFE(sfe, plot_type = "gene",
#                 sample_id = "C01",
#                 plot_feat = "Myl1")
# # 
# # 

