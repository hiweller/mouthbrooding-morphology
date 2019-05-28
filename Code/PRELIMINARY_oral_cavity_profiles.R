# README: PRELIMINARY_analysis_notes (in "Notes" folder)
image.dir <- dir("Data/Dissection_photographs/Test_set/")
require(ggplot2)
# df <- data.frame(Image.name = image.dir,
#                  Genus = rep(NA, length(image.dir)),
#                  Species = rep(NA, length(image.dir)),
#                  Oral.cavity.area = rep(NA, length(image.dir)),
#                  Head.area = rep(NA, length(image.dir)),
#                  Mouthbrooder = rep(NA, length(image.dir)),
#                  Feeding = rep(NA, length(image.dir)))
# 
# write.csv(df, "Spreadsheets/PRELIMINARY_oral_cavity_profiles.csv")


df <- read.csv("Spreadsheets/PRELIMINARY_oral_cavity_profiles.csv")


df$pct.mouth <- df$Oral.cavity.area..mm.2. / df$Head.area..mm.2.

grp.idx <- names(which(table(df$Group) == 1))

df <- df[-grep(paste(grp.idx, collapse = "|"), df$Group), ]

sf <- 2
p <- ggplot(data = df, 
            aes(Group, 
                pct.mouth)) + 
  geom_boxplot(aes(fill = Mouthbrooder)) + 
  scale_fill_manual(values = c("Tomato", "Cornflowerblue")) +
  xlab("Tribe") + theme_bw() + 
  ylab("Oral cavity (proportion of head)"); p


### stereomorph?
require(StereoMorph)

output.dir <- "Analysis/SM_test_2/"

{external.dir <- "Data/CT_dissection_overlap/External/"
internal.dir <- "Data/CT_dissection_overlap/Internal/"

interior.files <- paste0(output.dir, sub("SL.*", "", dir(internal.dir)), "_INT.txt")
exterior.files <- paste0(output.dir, sub("SL.*", "", dir(external.dir, pattern = "*.jpg")), "_EXT.txt")}

digitizeImages(paste0(internal.dir, dir(internal.dir)), interior.files,
               landmarks.ref = paste0(output.dir, "oral_cavity_landmarks_ref.txt"),
               curves.ref = paste0(output.dir, "oral_cavity_curves_ref.txt"))

digitizeImages(paste0(external.dir, dir(external.dir, pattern = "*.jpg")), exterior.files,
               landmarks.ref = paste0(output.dir, "external_morphology_landmarks_ref.txt"),
               curves.ref = paste0(output.dir, "external_morphology_curves_ref.txt"))

readShapes(dir(paste0(output.dir), pattern = "EXT_*", full.names = T))

# what to do with marker sets?

# function for plotting and storing internal markers
# basically a wrapper for read.shapes that plots curves and landmarks with
# labels and an eye

# also returns the shapes file with reference markers removed

# ref.tags are regular expressions for which landmarks to remove from final
# landmarks file
plot.internal.markers <- function(file, main = "default", 
                                  return.shapes = TRUE,
                                  plot = TRUE,
                                  ref.tags = c("pec", "ref", "eye")) {
  
  if (main == "default") {
    
    main <- tools::file_path_sans_ext(basename(file))
    
  }
  
  # read in shapes file
  shapes.file <- StereoMorph::readShapes(file)
  
  # invert all the y coordinates so things aren't upside-down
  #shapes.file$landmarks.scaled[ , 2] <- -shapes.file$landmarks.scaled[ , 2]

  curve.idx <- grep("curves", names(shapes.file))
  
  for (i in curve.idx) {
    for (j in 1:length(shapes.file[[i]])) {
      shapes.file[[i]][[j]][,2] <- -shapes.file[[i]][[j]][,2]
    }
  }
  
  # extract eye landmarks
  eyedx <- grep("eye", rownames(shapes.file$landmarks.scaled))
  eye.pts <- shapes.file$landmarks.scaled[eyedx, ]
  
  landmarks <- shapes.file$landmarks.scaled[-eyedx, ]
  
  # if plotting is on, plot data
  if (plot) {
    # plot landmarks
    plot(landmarks, asp = 1, pch = 19, 
         xlab = "", ylab = "", main = main)
    text(landmarks[ , 1],
         landmarks[ , 2],
         pos = 4,
         rownames(landmarks),
         cex = 0.8)
    
    # plot curves
    lapply(shapes.file$curves.scaled, function(i) points(i, asp=1, type ='l'))
    
    # get center and radius of eye
    center <- apply(eye.pts, 2, mean)
    radius <- mean(sqrt((eye.pts[, 1] - center[1])^2 + (eye.pts[, 2] - center[2])^2))
    
    plotrix::draw.circle(center[1], center[2], radius, col = "white")
    plotrix::draw.circle(center[1], center[2], radius / 3, col = "black")
    
  }
  
  if (return.shapes) {
    # make a modified shapes file
    shapes <- vector("list", 4)
    names(shapes) <- c("landmarks.scaled", "reference.landmarks",
                       "curves.control", "curves.scaled")
    
    # search for landmarks that match reference tags
    ref.idx <- grep(paste(ref.tags, collapse = "|"),
                    rownames(shapes.file$landmarks.pixel))
    
    # fill in list
    shapes$landmarks <- shapes.file$landmarks.scaled[-ref.idx, ]
    shapes$reference.landmarks <- shapes.file$landmarks.scaled[ref.idx, ]
    shapes$curves.control <- shapes.file$curves.control
    shapes$curves.pixel <- shapes.file$curves.pixel
    
    return(shapes)
  }
  
} 

# get landmarks filepaths
OC.landmarks <- dir("Analysis/SM_test_2/", pattern = "*INT.txt", full.names = T)
t <- readShapes(OC.landmarks[1], fields = c("curves.scaled"))

# make a table of just IDs (first three letters of genus & species names)
id.table <- substr(tools::file_path_sans_ext(basename(OC.landmarks)),
              1, 7) %>% table

# there are repeats, so number them accordingly
ids <- c()
for (i in 1:length(id.table)) {
  
  numbers <- as.character(c(1:id.table[i]))
  ids <- c(ids, paste(names(id.table)[i], numbers))
  
}

# make an empty list with ID names
oral.cavity.shapes <- vector("list", length = length(OC.landmarks))
names(oral.cavity.shapes) <- ids

# store oral cavity shapes in list
for (i in 1:length(OC.landmarks)) {
  oral.cavity.shapes[[i]] <- plot.internal.markers(OC.landmarks[i],
                             ref.tags = c("pec", "ref"),
                             plot = F)
  #colordistance:::pause()
}

oral.cavity.array <- array(dim = c(nrow(oral.cavity.shapes[[1]]$landmarks),
              2, length(oral.cavity.shapes)))
for (i in 1:length(oral.cavity.shapes)) {
  
  oral.cavity.array[ , , i] <- oral.cavity.shapes[[i]]$landmarks
  
}

## geomorph doodling ####

# following gpagen tutorial from https://cran.r-project.org/web/packages/geomorph/vignettes/geomorph.assistance.html
require(geomorph)
require(StereoMorph)



# for the record i hate that i did this but it's DOODLING
reproduction <- c("substrate", "substrate", "substrate",
                  "substrate", "mouthbrooding", "mouthbrooding",
                  "mouthbrooding", "mouthbrooding", "mouthbrooding",
                  "mouthbrooding", "substrate", "substrate",
                  "substrate", "substrate", "substrate",
                  "substrate", "mouthbrooding", "mouthbrooding",
                  "mouthbrooding", "mouthbrooding", "mouthbrooding", 
                  "substrate", "substrate", "substrate",
                  "substrate", "substrate", "substrate",
                  "mouthbrooding", "mouthbrooding", "mouthbrooding")
feeding <- c("picker", "picker", "picker",
             "picker", "picker", "picker",
             "picker", "picker", "picker",
             "picker", "picker", "ambush",
             "ambush", "ambush", "winnower", 
             "winnower", "winnower", "winnower",
             "winnower", "winnower", "winnower", 
             "winnower", "picker", "picker", 
             "unknown", "winnower", "winnower",
             "winnower", "winnower", "winnower")
location <- c(rep("South America", 14),
                  rep("Central America", 2),
                  rep("South America", 8),
                  "Madagascar",
                  rep("South America", 4),
                  "Africa")
mouthbrooders <- list(landmarks = oral.cavity.array,
             feeding = feeding,
             location = location,
             reproduction = reproduction,
             id = ids)

test <- gpagen(mouthbrooders$landmarks)

test <- readShapes(OC.landmarks)
test2 <- readland.shapes(test, nCurvePts = c(15, 5, 10, 10, 10, 5), scaled = TRUE)
test3 <- estimate.missing(test2, method = "TPS")
test4 <- gpagen(test3)

gp <- interaction(mouthbrooders$reproduction, mouthbrooders$feeding)
col.gp <- c("cornflowerblue", "tomato", "mediumseagreen", "goldenrod1")
names(col.gp) <- levels(gp)
col.gp <- col.gp[match(gp, names(col.gp))]

test5 <- plotTangentSpace(test4$coords, 
                          label = ids,
                          groups = col.gp, legend = T)
legend(x = -0.2,
       y = -0.1, 
       legend = levels(gp),
       fill = c("cornflowerblue", "tomato", "mediumseagreen", "goldenrod1"))

plot(test5)
mouthbrooders$feeding[mouthbrooders$feeding != "winnower"] <- "other"

gp <- interaction(mouthbrooders$reproduction, mouthbrooders$feeding)

col.gp <- c("cornflowerblue", "tomato", "mediumseagreen", "goldenrod1")
names(col.gp) <- levels(gp)
col.gp <- col.gp[match(gp, names(col.gp))]

plotTangentSpace(test4$coords, groups = col.gp, legend = T)

plotTangentSpace(mouthbrooders$landmarks, 
                 groups = col.gp, legend = T)

gdf <- geomorph.data.frame(test, 
                           reproduction = mouthbrooders$reproduction,
                           feeding = mouthbrooders$feeding)

fit.size <- procD.lm(coords ~ log(Csize), 
                     data = gdf)

fit.reproduction <- procD.lm(coords ~ reproduction * feeding,
                             data = gdf)

fit.reproduction <- procD.lm(coords ~ feeding,
                             data = gdf) %>% summary
