#' Kudzu grows in all directions and can grow up to 30 cm per day, with up
#' to around 30 vines growing from a single root crown.  Kudzu's traversal
#' growth ability allows it to grow horizontally across vast areas of land
#' finding new areas of soil to develop new roots in.
#' %%%%%%%%%%%%(https://www.ontario.ca/page/kudzu)%%%%%%%%%%%%%%%%
#'
#' We will try to model it's horizontal growth first with a spatial, or
#' lattice graphical, model and then move onto how it grows vertically.
#'
#'
#' Creating the simulation for horizontal growth:
# Matrix starting with zeros
# Poisson distribution (changing parameter)
grow <- function(plant = list(), nbuds = .1){
if(length(plant) == 0) return(list(matrix(0,ncol = 4, nrow = 1)))
  buds <- plant[[length(plant)]]
  newbuds <- matrix(0, ncol = 4, nrow = 0)
  for(bud in 1:nrow(buds)){
    new <- 1 + rpois(1, nbuds)
    for(newbud in 1:new){
      # dir <- rbind(c(-1,0), c(0,-1), c(1,0), c(0,1))[sample(1:4,1),]
      theta <- 2*pi*runif(1)
      dir <- c(cos(theta), sin(theta))
      newbuds <- rbind(newbuds, c(buds[bud,c(3,4)], buds[bud,c(3,4)] + dir))
    }
  }
  c(plant, list(newbuds))
}

plot.plant <- function(plant){
  extreme_x <- max(sapply(plant, function(mat){max(abs(mat[,3]))}))
  extreme_y <- max(sapply(plant, function(mat){max(abs(mat[,4]))}))
  plot(c(-extreme_x,extreme_x), c(-extreme_y,extreme_y), type = "n", asp = 1, 
       xlab = NA ,ylab = NA, main = "Kudzu growth simulation")
  for(i in 1:length(plant)){
    mat <- plant[[i]]
    for(i in 1:nrow(mat)){
      xs <- mat[i,c(1,3)]
      ys <- mat[i,c(2,4)]
      lines(xs,ys, col = i)
    }
  }
}

plant <- grow() # initializes the plot
n = 5 # n can be any number of iterations, it is just five here
for(i in 1:n){
plant <- grow(plant, .3)
}
plot.plant(plant)

