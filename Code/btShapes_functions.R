require(magrittr)
order.curves <- function(pattern, coords) {
  curve.idx <- grep(pattern, rownames(coords))
  curve.idx <- curve.idx[c(1, 3:length(curve.idx), 2)]
  return(curve.idx)
}

plot_internal_morphology <- function(xy, coor, size = 1, col = "black") {
  
  w <- par('pin')[1] / diff(par('usr')[1:2])
  h <- par('pin')[2] / diff(par('usr')[3:4])
  asp <- w / h
  
  coor[, 1] <- coor[, 1] * (1/asp)
  
  coor <- coor*size
  
  coor <- coor - matrix(colMeans(apply(coor, 2, range)), 
                        nrow=nrow(coor), ncol=ncol(coor), byrow=TRUE)
  
  coor <- coor + matrix(xy, nrow(coor), ncol(coor), byrow=TRUE)
  
  curves <- c("ps", "ep", "cb", "bb", "lj", "ebl")
  lnum <- c(15, 5, 10, 10, 10, 5)
  curve.names <- sapply(c(1:length(curves)), function(i) rep(curves[i], lnum[i] - 2)) %>% unlist
  
  rownames(coor)[grep("curve", rownames(coor))] <- curve.names  
  pt.orders <- c(order.curves("ps", coor),
                 order.curves("ebl", coor),
                 order.curves("bb", coor),
                 order.curves("lj", coor))
  
  polygon(coor[pt.orders, ], 
          col = col, 
          border = col)
  
}

plot_external_morphology <- function(xy, coor, size = 1, col = "black") {
  w <- par('pin')[1] / diff(par('usr')[1:2])
  h <- par('pin')[2] / diff(par('usr')[3:4])
  asp <- w / h
  
  coor[, 1] <- coor[, 1] * (1/asp)
  
  coor <- coor*size
  
  coor <- coor - matrix(colMeans(apply(coor, 2, range)), 
                        nrow=nrow(coor), ncol=ncol(coor), byrow=TRUE)
  
  coor <- coor + matrix(xy, nrow(coor), ncol(coor), byrow=TRUE)
  
  lnum <- c(10, 10, 5, 5, 5, 5, 5)
  curves <- c("snout", "operc", "inter", 
              "lojaw", "preop", "am.mar",
              "am.div")
  curve.names <- sapply(c(1:length(curves)), function(i) rep(curves[i], lnum[i] - 2)) %>% unlist
  
  rownames(coor)[grep("curve", rownames(coor))] <- curve.names
  
  pt.orders <- c(order.curves("snout", coor),
                 grep("jaw.uppe", rownames(coor)),
                 grep("mth.corn", rownames(coor)),
                 grep("jaw.lowe", rownames(coor)),
                 order.curves("lojaw", coor) %>% rev,
                 order.curves("inter", coor) %>% rev,
                 order.curves("operc", coor) %>% rev)
  
  polygon(coor[pt.orders, ], 
          col = col, 
          border = col)
}
