require(phytools)
require(ape)

tree <- read.tree(file = "Spreadsheets/FIGTREE_reduced_South_American_cichlid_reproduction")

#write.csv(data.frame(sciname = tree$tip.label), "Spreadsheets/REDUCED_SA_cichlid_reproduction.csv")

mb.df <- as.matrix(read.csv("Spreadsheets/REDUCED_SA_cichlid_reproduction.csv", 
                  row.names = 1))
rmode <- as.factor(setNames(mb.df[,1], rownames(mb.df)))

dotTree(tree, rmode,
        colors = setNames(c("magenta", "cyan"),
        c("Mouthbrooder", "Substrate_spawner")),
        fsize = 1)

mb.tree <- make.simmap(tree, rmode, nsim = 100)
obj <- densityMap(mb.tree, states = c("Mouthbrooder", "Substrate_spawner", plot = FALSE))

sf <- 2

png(filename = "Notes/PRELIMINARY_reduced_mouthbrooding_phylogeny.png", 
    width = 1000 * sf, height = 1500 * sf, res = 300)
plot(obj, lwd = 4, outline = T, 
     fsize = c(0.7, 0.9), col = c("magenta", "cyan"))
dev.off()
plotTree(obj, type = "fan", fsize = 0.7)
nodelabels(cex = 0.6)



