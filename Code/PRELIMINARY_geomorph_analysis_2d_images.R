# following gpagen tutorial from https://cran.r-project.org/web/packages/geomorph/vignettes/geomorph.assistance.html
require(geomorph)
require(StereoMorph)

# formatting ####
# filepath setting
shapes.dir <- "Analysis/SM_test_2/"
INT.landmarks <- dir(shapes.dir, pattern = "*INT.txt", full.names = T)
EXT.landmarks <- dir(shapes.dir, pattern = "*EXT.txt", full.names = T)

# make a table of just IDs (first three letters of genus & species names)
id.table <- substr(tools::file_path_sans_ext(basename(INT.landmarks)),
                   1, 7) %>% table

# there are repeats, so number them accordingly
ids <- c()
for (i in 1:length(id.table)) {
    ids <- c(ids, paste(names(id.table)[i], as.character(c(1:id.table[i]))))
}

# read in landmarks and perform procrustes fitting ####
shapes.list <- readShapes(INT.landmarks)
int.landmarks <- readland.shapes(shapes.list,
                                nCurvePts = c(15, 5, 10, 10, 10, 5),
                                scaled = TRUE)
int.landmarks <- estimate.missing(int.landmarks, method = "TPS")
int.fit <- gpagen(int.landmarks)

ext.shapes <- readShapes(EXT.landmarks)
ext.landmarks <- readland.shapes(ext.shapes,
                                 nCurvePts = c(10, 10, 5, 5, 5, 5, 5),
                                 scaled = TRUE)
for (i in 1:length(ext.landmarks$landmarks)) {
  ext.landmarks$landmarks[[i]][,2] <- -ext.landmarks$landmarks[[i]][,2]
}
ext.landmarks <- estimate.missing(ext.landmarks, method = "TPS")
ext.fit <- gpagen(ext.landmarks)

# create additional classifiers ####
ids
feeding <- c(rep("other", 14),
             rep("winnow", 8),
             rep("other", 3),
             rep("winnow", 5))

reproduction <- c(rep("SS", 4),
                  rep("MB", 6),
                  rep("SS", 6),
                  rep("MB", 5),
                  rep("SS", 6),
                  rep("MB", 3))

location <- c(rep("South America", 14),
              rep("Central America", 2),
              rep("South America", 8),
              "Madagascar",
              rep("South America", 4),
              "Africa")
mb.int <- list(coords = int.fit$coords,
               reproduction = reproduction,
               location = location,
               feeding = feeding,
               id = ids)

# perform PCA and color by group ####
gp <- interaction(mb.int$feeding, mb.int$reproduction)
col.gp <- c("goldenrod1", "mediumseagreen", "tomato", "cornflowerblue")
names(col.gp) <- levels(gp)

col.gp <- col.gp[match(gp, names(col.gp))]

pts <- plotTangentSpace(mb.int$coords,
                        groups = col.gp,
                        legend = T,
                        label = mb.int$id)

ext.pts <- plotTangentSpace(ext.fit$coords,
                            groups = col.gp,
                            legend = T,
                            label = mb.int$id)

# plotting PC scores ####
require(ggplot2)
# PC1 and PC2
sf <- 1.7
png("Analysis/SM_test_2/Oral_cavity_PC1_PC2.png",
    width = 1000 * sf, height = 1000 * sf,
    res = 300)
{plot(pts$pc.scores[ , 1:2],
      main = "Oral cavity",
     col = alpha(col.gp, 0.8),
     pch = 19,
     cex = 1.5, asp = 1,
     panel.first = abline(h = 0, v = 0, lty = 2, col = "grey"))

legend(x = -0.28,
       y = -0.10,
       fill = c("goldenrod1", "mediumseagreen", "tomato", "cornflowerblue"),
       legend = c("MB / other", "MB / winnow", "SS / other", "SS / winnow"),
       cex = 0.75)

text(pts$pc.scores[ , 1:2],
     mb.int$id,
     col = col.gp, pos = 3,
     cex = 0.65)}
dev.off()

# PC1 and PC3
{plot(pts$pc.scores[ , c(1, 3)],
      col = alpha(col.gp, 0.8),
      pch = 19,
      cex = 1.5, asp = 1,
      panel.first = abline(h = 0, v = 0, lty = 2, col = "grey"))

  legend(x = -0.28,
         y = -0.09,
         fill = c("goldenrod1", "mediumseagreen", "tomato", "cornflowerblue"),
         legend = c("MB / other", "MB / winnow", "SS / other", "SS / winnow"),
         cex = 0.75)

  text(pts$pc.scores[ , c(1, 3)],
       mb.int$id,
       col = col.gp, pos = 3,
       cex = 0.65)}

# PC2 and PC3
{plot(pts$pc.scores[ , 2:3],
      col = alpha(col.gp, 0.8),
      pch = 19,
      cex = 1.5, asp = 1,
      panel.first = abline(h = 0, v = 0, lty = 2, col = "grey"))

  legend(x = -0.13,
         y = 0.12,
         fill = c("goldenrod1", "mediumseagreen", "tomato", "cornflowerblue"),
         legend = c("MB / other", "MB / winnow", "SS / other", "SS / winnow"),
         cex = 0.75)

  text(pts$pc.scores[ , 2:3],
       mb.int$id,
       col = col.gp, pos = 3,
       cex = 0.65)}


# exterior plotting ####
# PC1 and PC2
sf <- 1.7
png("Analysis/SM_test_2/Exterior_PC1_PC2.png",
    width = 1000 * sf, height = 1000 * sf,
    res = 300)
{plot(ext.pts$pc.scores[ , 1:2],
      main = "External anatomy",
      col = alpha(col.gp, 0.8),
      pch = 19,
      cex = 1.5, asp = 1,
      panel.first = abline(h = 0, v = 0, lty = 2, col = "grey"))

  legend(x = -0.22,
         y = -0.12,
         fill = c("goldenrod1", "mediumseagreen", "tomato", "cornflowerblue"),
         legend = c("MB / other", "MB / winnow", "SS / other", "SS / winnow"),
         cex = 0.75)

  text(ext.pts$pc.scores[ , 1:2],
       mb.int$id,
       col = col.gp, pos = 3,
       cex = 0.65)}
dev.off()
# PC1 and PC3
{plot(ext.pts$pc.scores[ , c(1, 3)],
      col = alpha(col.gp, 0.8),
      pch = 19,
      cex = 1.5, asp = 1,
      panel.first = abline(h = 0, v = 0, lty = 2, col = "grey"))

  legend(x = -0.22,
         y = -0.09,
         fill = c("goldenrod1", "mediumseagreen", "tomato", "cornflowerblue"),
         legend = c("MB / other", "MB / winnow", "SS / other", "SS / winnow"),
         cex = 0.75)

  text(ext.pts$pc.scores[ , c(1, 3)],
       mb.int$id,
       col = col.gp, pos = 3,
       cex = 0.65)}

# PC2 and PC3
{plot(ext.pts$pc.scores[ , 2:3],
      col = alpha(col.gp, 0.8),
      pch = 19,
      cex = 1.5, asp = 1,
      panel.first = abline(h = 0, v = 0, lty = 2, col = "grey"))

  legend(x = 0.04,
         y = -0.06,
         fill = c("goldenrod1", "mediumseagreen", "tomato", "cornflowerblue"),
         legend = c("MB / other", "MB / winnow", "SS / other", "SS / winnow"),
         cex = 0.75)

  text(ext.pts$pc.scores[ , 2:3],
       mb.int$id,
       col = col.gp, pos = 3,
       cex = 0.65)}


# backtransform -- external ####
sf <- 2
png(filename = "Graphics/External_morphology_backtransform.png", res = 300, 
    width = 1000 * sf, height = 1000 * sf)
{gpa_array <- gpagen(ext.landmarks)$coords

gpa_mat <- t(apply(gpa_array, 3, function(i) matrix(t(i), 1)))

resEig <- eigen(cov(gpa_mat))

scores <- gpa_mat %*% resEig$vectors

per_var <- (resEig$values / sum(resEig$values)) * 100; per_var

coor <- ext.landmarks$landmarks[[30]]

source("Code/btShapes_functions.R")

pcs <- 1:2

plot(scores[, pcs], type = 'n', main = "External morphology",
     xlab=paste0('PC', pcs[1], ' (', round(per_var[pcs[1]]), '%)'),
     ylab=paste0('PC', pcs[2], ' (', round(per_var[pcs[2]]), '%)'),
     asp = 1)

btShapes(scores=scores, vectors=resEig$vectors, fcn=plot_external_morphology, 
         pcs=pcs, n=c(5,5), m=2, row.names=rownames(coor), 
         pc.margin=c(0.05, -0.05), size=0.15, col=gray(0.7))

points(scores[,pcs], 
       col = alpha(col.gp, 0.8),
       pch = 19,
       cex = 1.5, asp = 1,
       panel.first = abline(h = 0, v = 0, lty = 2, col = "grey"))
text(scores[, pcs],
     labels = substr(ids, 1, 7), cex = 0.8, pos = 1, offset = 0.5,
     col = col.gp)

legend(x = 0.15,
       y = -0.08,
       fill = c("goldenrod1", "mediumseagreen", "tomato", "cornflowerblue"),
       legend = c("MB / other", "MB / winnow", "SS / other", "SS / winnow"),
       cex = 0.75)}
dev.off()

# create backtransform morphospace  -- internal####
sf <- 2
png(filename = "Graphics/Internal_morphology_backtransform.png", res = 300, 
    width = 1000 * sf, height = 1000 * sf)

{gpa_array <- gpagen(int.landmarks)$coords

gpa_mat <- t(apply(gpa_array, 3, function(i) matrix(t(i), 1)))

resEig <- eigen(cov(gpa_mat))

scores <- gpa_mat %*% resEig$vectors

per_var <- (resEig$values / sum(resEig$values)) * 100; per_var

coor <- int.landmarks$landmarks[[30]]

# order for plotting:
# parasphenoid, ebl, ceratobranchial, lower jaw


source("Code/btShapes_functions.R")

pcs <- 1:2

plot(scores[, pcs], type = 'n', main = "Oral cavity",
     xlab=paste0('PC', pcs[1], ' (', round(per_var[pcs[1]]), '%)'),
     ylab=paste0('PC', pcs[2], ' (', round(per_var[pcs[2]]), '%)'),
     asp = 1)

btShapes(scores=scores, vectors=resEig$vectors, fcn=plot_internal_morphology, 
         pcs=pcs, n=c(5,5), m=2, row.names=rownames(coor), 
         pc.margin=c(0.05,-0.05), size=0.2, col=gray(0.7))

points(scores[,pcs], 
       col = alpha(col.gp, 0.8),
       pch = 19,
       cex = 1.5, asp = 1,
       panel.first = abline(h = 0, v = 0, lty = 2, col = "grey"))
text(scores[, pcs],
     labels = substr(ids, 1, 7), cex = 0.8, pos = 1, offset = 0.5,
     col = col.gp)

legend(x = 0.2,
       y = 0.13,
       fill = c("goldenrod1", "mediumseagreen", "tomato", "cornflowerblue"),
       legend = c("MB / other", "MB / winnow", "SS / other", "SS / winnow"),
       cex = 0.75)
}
dev.off()

