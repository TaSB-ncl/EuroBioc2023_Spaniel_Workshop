#' Label SFE for Shiny
#'
#' @param sfe 
#' @param grp_cols 
#' @param qc_cols 
#' @param exp_cond_cols 
#'
#' @return
#' @export
#'
#' @examples
labelSFE <- function(sfe, 
                     grp_cols, 
                     qc_cols,
                     exp_cond_cols){
  
  md_names <- colnames(colData(sfe))
  md_names[md_names %in% grp_cols] <- 
    paste0("grp_", md_names[md_names %in% grp_cols])
  md_names[md_names %in% qc_cols] <- 
    paste0("qc_", md_names[md_names %in% qc_cols])
  md_names[md_names %in% exp_cond_cols] <- 
    paste0("expCond_", md_names[md_names %in% exp_cond_cols])
  names(colData(sfe)) <- md_names
  return(sfe)
  
}


## reload sfe and test
## save sfe
## document, add to git, add to workshop
## change shiny to select for columns
## 
