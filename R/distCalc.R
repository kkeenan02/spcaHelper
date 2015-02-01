#' Generate an inverse distance matrix for points within a landscape
#' 
#' @param raster a raster upon which distance are calculated
#' @param coords a file containing sample coordinates in lat long format
#' @param mat_type an character string, indicating whether the returned matrix should contain spatial "weight" or absolute "distance".
#' @return a square matrix containing the inverse pairwise distance between all
#' points in \code{coords}.

distCalc <- function(raster, coords, mat_type = "weight") {
  # read raster
  #raster <- "RBW.asc"
  #coords <- "RBW_bl_coords.txt"
  rast <- raster::raster(raster)
  rast[rast == -9999] <- NA
  rast[!is.na(rast[])] <- 1
  
  # read sample coords
  # read the sample coordinates
  sampCoords <- read.delim(coords, header = TRUE, stringsAsFactors = FALSE)
  # Seperate coordinates from sample names
  coords <- sampCoords[, 2:3]
  # Extract all unique sites from coords
  idx <- apply(coords, 1, function(x) {
    return(paste(x[1], "\t", x[2], sep = ""))
  })
  idx <- unique(idx)
  # Convert idx to coord matrix
  uniqueCoords <- t(sapply(idx, function(x) {
    return(as.numeric(strsplit(x, split = "\\s+")[[1]]))
  }))
  # Get sample names for each unique coordinate
  coords_pst <- apply(coords, 1, paste, collapse = "\t")
  preSamps <- sapply(idx, function(x) {
    return(as.character(sampCoords[which(coords_pst == x)[1], 1]))
  })
  stp <- max(sapply(sampCoords[,1], nchar), na.rm = TRUE)-2
  samps <- sapply(preSamps, substr, start = 1, stop = stp)
  # calculate the distances
  # Transform the raster to a transition matrix
  rastTran <- gdistance::transition(rast, transitionFunction = mean,
                                    directions = 8)
  # correct the matrix for diagonal distortion
  rastTran <- gdistance::geoCorrection(rastTran, type = "c", 
                                       multpl = FALSE, scl = TRUE)
  # Calculate river distances
  rivDist <- gdistance::costDistance(x = rastTran, 
                                     fromCoords = uniqueCoords)
  rivDist <- as.matrix(rivDist)
  # Create a pairwise matrix index
  pw_idx <- combn(nrow(sampCoords), 2)
  # create a vector of individual's coordinate index
  coord_idx <- sapply(coords_pst, function(x){
    which(names(samps) == x)
  }, USE.NAMES = TRUE)
  # create an empty matrix
  dist_mat <- matrix(nrow = nrow(sampCoords), ncol = nrow(sampCoords))
  resol <- raster::res(rast)[1]
  # Fill the matrix using the pairwise index.  A distance conversion should
  # be carried out to account for raster resolution
  for (i in 1:length(pw_idx[1,])) {
    row <- coord_idx[pw_idx[1,i]]
    col <- coord_idx[pw_idx[2,i]]
    dist_mat[pw_idx[2, i], pw_idx[1, i]] <- rivDist[row, col] * resol/1000
    dist_mat[pw_idx[1, i], pw_idx[2, i]] <- rivDist[row, col] * resol/1000
  }
  colnames(dist_mat) <- sampCoords[,1]
  rownames(dist_mat) <- sampCoords[,1]
  if(mat_type == "weight"){
    return((dist_mat + 0.0000001)^-1) 
  } else if(mat_type == "distance"){
    dist_mat
  }
}