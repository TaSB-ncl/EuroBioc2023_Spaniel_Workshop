# ncomponents = 50
# proportion=0.1
# autoPC = TRUE
# K = 30
# cluster_function = "leiden"
# dim_red = "PCA"



#' Preprocess the data for clustering
#' 
#' A wrapper function to normalise
#'
#' @param ncomponents the number of principle components to use
#' @param proportion proportion of highly variable genes to use
#' @param autoPC logical vector determines whether to remove principal 
#' components corresponding to technical noise
#' @param sfe 
#'
#' @return
#' @export
#'
#' @examples
preProcess <- function(sfe,
                          ncomponents = 50,
                          proportion=0.1,
                          autoPC = TRUE,
                          dim_red = "PCA"
                          ){
  ## log nomalise data
  sfe <- scuttle::logNormCounts(sfe)
  ## highly variable genes
  dec <- scran::modelGeneVar(sfe)
  hvgs <- getTopHVGs(dec, prop = proportion)
  
  ## run PCA
  sfe <- scater::runPCA(sfe,
                        ncomponents,
                        scale = TRUE, 
                        subset_row = hvgs)
  
  ## automatic calculation of PCA components
  if(autoPC){
  sfe <- scran::denoisePCA(sfe, dec, subset.row=hvgs)
  ncomponents <- ncol(reducedDim(sfe, "PCA"))
  }
  
  ## run UMAP
  sfe <- scater::runUMAP(sfe,dimred=dim_red)
  
  
  return(sfe)
}




#' Cluster helper function
#' 
#' Uses the clusterCells function to add cluster results 
#' directly to sfe object.
#'
#' @param sfe  a SpatialFeatureExperiment object with log normalised counts
#' @param K the number of nearest neighbours to use during graph construction
#' @param cluster_function any igraph community detection algorithm
#' @param dim_red the reduced dimensions to use for clustering
#'
#' @return
#' @export
#'
#' @examples
clustCells <- function(sfe, 
                       K = 30, 
                       cluster_function = "leiden",
                       dim_red = "PCA"
){
  clust <- scran::clusterCells(sfe, 
                               BLUSPARAM=NNGraphParam(
                               cluster.fun=cluster_function, 
                               k = K), 
                               use.dimred=dim_red, 
                               full=TRUE)
  clustCol <- paste0("clust_", cluster_function, "_K_", K)
  
  colData(sfe)[,clustCol] <- clust$clusters
  return(sfe)
}
