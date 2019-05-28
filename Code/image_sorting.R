img.dir <- "C:/Users/Problematicus/Dropbox/Midwest_2019_Trip/Data/Dissection_photographs/FMNH_HIW_mouthbrooding/"

images <- dir(img.dir,
    pattern = "*.tif",
    full.names = FALSE)

specimens <- unique(substr(images, 1, nchar(images)-7))

for (i in 1:length(specimens)) {
  
  if (!dir.exists(paste0(img.dir, specimens[i]))) {
    dir.create(paste0(img.dir, specimens[i]))
  }
  
  img.idx <- grep(specimens[i], images)
  
  for (j in img.idx) {
    file.rename(from = paste0(img.dir, images[j]),
                to = paste0(img.dir, specimens[i], "/", images[j]))
  }
  
}
