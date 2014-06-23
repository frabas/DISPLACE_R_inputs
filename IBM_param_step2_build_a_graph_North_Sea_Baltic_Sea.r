

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!FUNCTIONS FOR CREATING THE GRAPH!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

create_graph<-function(general){


 load(file=file.path(general$main.path, "ports_to_keep_2010.RData")) # get port.to.keep built with IBM_param_step1.r 
 


 EUports <- ports.to.keep[!is.na(ports.to.keep$lat),]

 EUports$NAVN <- EUports$Description
 EUports$DEC_LONG <- EUports$lon
 EUports$DEC_LAT <- EUports$lat
 
  # remove few inland ports
 EUports <- EUports[!EUports$NAVN=="Aalborg",]
 EUports <-EUports[!EUports$NAVN=="Brande",]
 EUports <-EUports[!EUports$NAVN=="Nykøbing Mors",]
 EUports <-EUports[!EUports$NAVN=="Hochdonn",]
 EUports <-EUports[!EUports$NAVN=="Burgstaaken/Fehmarn",]
 EUports <-EUports[!EUports$NAVN=="Schleswig",]
 
                
                
 harbour.list  <-  EUports

 
 harbour.list$NAVN <- factor(harbour.list$NAVN)
 harbour.list$idx.port <- harbour.list$NAVN
 harbour.list$idx.port <- 1:length(harbour.list$NAVN) # convert in numeric for soft use


 harbours <- harbour.list[,c("DEC_LONG","DEC_LAT","idx.port")] # longlat
 colnames(harbours) <- c("x","y","idx.port") # rename
 rownames(harbours) <- harbours$idx.port
 harbours <- as.matrix(harbours)



 # create the underlying igraph
 # (caution: remenber that length(ext.y)/2 need to be pair)
 
 do.triangular.grid <- function(dx, dy, xdep, xarr, ydep, yarr){
 # noisy triangle grid with dx and dy
 ext.x <- seq(xdep ,xarr ,by=2*dx)
 ext.x2 <- seq(xdep + dx/2, xarr + dx/2, by=2*dx)
 ext.y <- seq(ydep, yarr,by=dy)
 x <-  rep(c(ext.x,ext.x2), length(ext.y)/2)  # caution: length(ext.y)/2 need to be pair
 y  <- rep(c(ext.y), each=length(ext.x))
 x <- x+ rnorm(length(x),0,0.001)
 y <- y+ rnorm(length(y),0,0.001)
 return(cbind(x,y))
 }
 
 do.regular.grid <- function(dx, dy, xdep, xarr, ydep, yarr){
 # noisy regular grid with dx and dy
 ext.x <- seq(xdep ,xarr ,by=dx)
 ext.y <- seq(ydep, yarr, by=dy)
 x <-  rep(c(ext.x), length(ext.y)) 
 y  <- rep(c(ext.y), each=length(ext.x))
 x <- x+ rnorm(length(x),0,0.0001)
 y <- y+ rnorm(length(y),0,0.0001)
 return(cbind(x,y))
 }

 # various grid resolution centered on dk and western baltic
 # (1 degree longitude = 67.6 km at 57 degree North i.e. middle of North Sea, 
 #  while 1 degree latitude =111.2 km also at 57 degree North (but actually almost constant whatever the latitude)
 # caution: avoid bias: apply one resolution grain only per management area becausae of evenly distributed avai.
 grid1.xy <- do.regular.grid(1.183432,   0.7194245, -13.5, 25, 48,   67.0)  # rought grain: 80 by 80 km grid
 grid2.xy <- do.regular.grid(0.147929,   0.089928,   7,  15, 53,   60.1)  # finest grain: 10 by 10 km grid
 grid3.xy <- do.regular.grid(0.3698225,  0.2248201, -9,  24, 48.6, 62.3)  # medium grain: 25 by 25 km grid
 plot(grid1.xy[,1], grid1.xy[,2], pch=".")
 points(grid2.xy[,1], grid2.xy[,2], col=3, pch=".")
 points(grid3.xy[,1], grid3.xy[,2], col=3, pch=".")
  map("worldHires", add=TRUE)


 idx_pts <- point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0  | point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
       pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
            10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
            11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
        pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
            59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, #sd2224
            57.74247, 57.50441, 57.10708, 57.08073)) > 0  | 
             point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2], #sd2224  
             pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |
            point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
             pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,   #sd2224
            56.6)) > 0 |
            point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
              pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,  #kask
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0   # kask
 grid2.xy <- grid2.xy[idx_pts,] # keep only those           
 points(grid2.xy[,1], grid2.xy[,2], col=3, pch=".")
 
 idx_pts <- point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0  | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
       pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
            10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
            11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
        pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
            59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, #sd2224
            57.74247, 57.50441, 57.10708, 57.08073)) > 0  | 
             point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2], #sd2224  
             pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |
            point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
             pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,   #sd2224
            56.6)) > 0 |
            point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
              pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,  #kask
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0   # kask

 grid1.xy <- grid1.xy[!idx_pts,] # clip on grid2

  idx_pts <- point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0  | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
       pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
            10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
            11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
        pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
            59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, #sd2224
            57.74247, 57.50441, 57.10708, 57.08073)) > 0  | 
             point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2], #sd2224  
             pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |
            point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
             pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,   #sd2224
            56.6)) > 0 |
            point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
              pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,  #kask
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0   # kask

 grid3.xy <- grid3.xy[!idx_pts,] # clip on grid2

 
 
  idx_pts <- point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(-4, 7, 8, 7, 7, 7.906163, -4, -4.6, -4.6, -4),
        pol.y = c(62, 62, 61.5, 60, 58, 57.5, 57.5, 57.3, 58.2,     # nsea
            58.4)) > 0 | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(0, 0.5, 7.5, 7.5), pol.y = c(53.5, 51, 51,        # nsea
            53.5)) > 0  | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(7.906163, 8.6488368, 8.5, 10.3, 10.3, 9.2,
            9.2, 7.11, -1, -4, -1.78), pol.y = c(57.5, 57.08073,   #nsea
            57, 57.3, 57, 56.2, 52.7, 53.5, 53.5, 56.1, 57.5)) > 0 | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(14.2, 14.78, 15, 15, 18, 18, 14.2), pol.y = c(55.4,   #2232
            55.3, 55, 53, 53, 56.5, 56.5)) > 0 | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(18, 18, 22, 22), pol.y = c(56.5, 53.5, 53.5,              #2232
            56.5)) > 0 | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(18, 18, 18.32, 18.32, 19, 19, 16, 16), pol.y = c(56.5,
            57, 57, 57.5, 57.925, 59.762, 59.762, 56.5)) > 0 | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(19, 19, 18.45, 18.3, 18, 18, 21.5, 21.72, 21.98,       #2232
            22.17, 22.24, 21.93), pol.y = c(58.5, 57.9, 57.58,
            57, 57, 56.5, 56.5, 57.57, 57.97, 58.04, 58.15,
            58.5)) > 0 | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(21.5, 21.72, 21.98, 22.17, 22.24, 22.24, 23,          #2232
            25, 25), pol.y = c(56.5, 57.57, 57.97, 58.04, 58.15,
            58.35, 58.5, 58.5, 56.5)) > 0 | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(19, 17.975, 21.6, 21.8, 23.325, 23.325, 23.191,       #2232
            23, 23, 23.5, 23.6, 24, 23.692, 22.5, 22.1, 21.92,
            19), pol.y = c(59.762, 60.5, 60.5, 60.7, 60.5, 59.965,
            59.867, 59.827, 59, 59, 59.05, 58.75, 59.5, 59.5,
            58.35, 58.5, 58.5)) > 0  | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(23.325, 23.325, 23.191, 23, 23, 30.5, 30.5),             #2232
        pol.y = c(60.5, 59.965, 59.867, 59.827, 59, 59, 60.5)) > 0  | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2], 
        pol.x = c(2, -2, -2, -1.956, -1, 2), pol.y = c(51, 51,    # VIId 
            50.6, 49.705, 49, 49)) > 0 | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2], 
        pol.x = c(-4, -4, -4.6, -4.6, -4, -6, -8, -12, -12, -5, # 6a
            -5), pol.y = c(60.5, 58.4, 58.2, 57.3, 55, 55, 54.5, 
            54.5, 60, 60, 60.5)) > 0 | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2], 
        pol.x = c(-6.3, -7.8, -2.5, -2.5), pol.y = c(55, 52,  # 7a
            52, 55)) > 0  | point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2], 
        pol.x = c(-2, -2.22, -5.17, -5.24, -7, -7, -5, -5, -1,  #7e
            -1, -1.956), pol.y = c(50.6, 50.88, 50.21, 50, 50, 
            49.5, 49.5, 48, 48, 49.3, 49.705)) > 0

    grid3.xy <- grid3.xy[idx_pts,] # keep only nsea  and sd2532 (and 7d, 6a, 7a, 7e to have nice network around UK)
         
            
    idx_pts <- point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(-4, 7, 8, 7, 7, 7.906163, -4, -4.6, -4.6, -4),
        pol.y = c(62, 62, 61.5, 60, 58, 57.5, 57.5, 57.3, 58.2,        # nsea
            58.4)) > 0 | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(0, 0.5, 7.5, 7.5), pol.y = c(53.5, 51, 51,        # nsea
            53.5)) > 0  | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(7.906163, 8.6488368, 8.5, 10.3, 10.3, 9.2,          # nsea
            9.2, 7.11, -1, -4, -1.78), pol.y = c(57.5, 57.08073,
            57, 57.3, 57, 56.2, 52.7, 53.5, 53.5, 56.1, 57.5)) > 0  | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(14.2, 14.78, 15, 15, 18, 18, 14.2), pol.y = c(55.4,   #2232
            55.3, 55, 53, 53, 56.5, 56.5)) > 0 | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(18, 18, 22, 22), pol.y = c(56.5, 53.5, 53.5,              #2232
            56.5)) > 0 | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(18, 18, 18.32, 18.32, 19, 19, 16, 16), pol.y = c(56.5,
            57, 57, 57.5, 57.925, 59.762, 59.762, 56.5)) > 0 | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(19, 19, 18.45, 18.3, 18, 18, 21.5, 21.72, 21.98,       #2232
            22.17, 22.24, 21.93), pol.y = c(58.5, 57.9, 57.58,
            57, 57, 56.5, 56.5, 57.57, 57.97, 58.04, 58.15,
            58.5)) > 0 | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(21.5, 21.72, 21.98, 22.17, 22.24, 22.24, 23,          #2232
            25, 25), pol.y = c(56.5, 57.57, 57.97, 58.04, 58.15,
            58.35, 58.5, 58.5, 56.5)) > 0 | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(19, 17.975, 21.6, 21.8, 23.325, 23.325, 23.191,       #2232
            23, 23, 23.5, 23.6, 24, 23.692, 22.5, 22.1, 21.92,
            19), pol.y = c(59.762, 60.5, 60.5, 60.7, 60.5, 59.965,
            59.867, 59.827, 59, 59, 59.05, 58.75, 59.5, 59.5,
            58.35, 58.5, 58.5)) > 0  | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(23.325, 23.325, 23.191, 23, 23, 30.5, 30.5),             #2232
        pol.y = c(60.5, 59.965, 59.867, 59.827, 59, 59, 60.5)) > 0 | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2], 
        pol.x = c(2, -2, -2, -1.956, -1, 2), pol.y = c(51, 51,    # VIId 
            50.6, 49.705, 49, 49)) > 0  | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2], 
        pol.x = c(-4, -4, -4.6, -4.6, -4, -6, -8, -12, -12, -5, # 6a
            -5), pol.y = c(60.5, 58.4, 58.2, 57.3, 55, 55, 54.5, 
            54.5, 60, 60, 60.5)) > 0  | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2], 
        pol.x = c(-6.3, -7.8, -2.5, -2.5), pol.y = c(55, 52,  # 7a
            52, 55)) > 0  | point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2], 
         pol.x = c(-2, -2.22, -5.17, -5.24, -7, -7, -5, -5, -1,  #7e
            -1, -1.956), pol.y = c(50.6, 50.88, 50.21, 50, 50, 
            49.5, 49.5, 48, 48, 49.3, 49.705)) > 0
         

    grid1.xy <- grid1.xy[!idx_pts,] # clip on nsea  and sd2532 and 7d
          
            
 
 
 # clip on grid3
 #grid1.xy <- grid1.xy[grid1.xy[,1]<min(grid3.xy[,1]) | grid1.xy[,1]>max(grid3.xy[,1]) | grid1.xy[,2] <min(grid3.xy[,2]) | grid1.xy[,2]>max(grid3.xy[,2]),] 

 # check
 plot(grid1.xy[,1], grid1.xy[,2], pch=".")
 map("worldHires", add=TRUE)
 points(grid1.xy[,1], grid1.xy[,2], col=4, pch=".")
 points(grid2.xy[,1], grid2.xy[,2], col=4, pch=".")
 points(grid3.xy[,1], grid3.xy[,2], col=4, pch=".")

 # approve...
 x <- c(grid1.xy[,1], grid2.xy[,1], grid3.xy[,1])
 y <- c(grid1.xy[,2], grid2.xy[,2], grid3.xy[,2])


 coord <-matrix(0,ncol=3, nrow=length(x))
 coord[,1] <- x
 coord[,2] <- y
 coord[,3] <- 0 # 'at sea'
 colnames(coord) <- c("x","y","idx.port")

 
 # remove points on land...
 library(rgeos)
 sh1 <- readShapePoly(file.path(general$main.path,"shp","francois_EU"))
 dd <-sapply(slot(sh1, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...
 library(sp)
 for(iLand in 1:length(dd)){
     print(iLand)
     if(sh1@data$C_NAME[iLand]=="DENMARK") dilate.with=0.01 else dilate.with=0.03
          # Points on land are 1 or 2 and not is 0
          #res<- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][[1]][,1],dd[[iLand]][[1]][,2])
          #replaced by:
          p1 <- readWKT(paste("POLYGON((",paste(dd[[iLand]][[1]][,1], dd[[iLand]][[1]][,2], collapse=","),"))", sep=''))
          buff <- gBuffer(p1,width=dilate.with)
          coords <- buff@polygons[[1]]@Polygons[[1]]@coords
          res<- point.in.polygon(coord[,1],coord[,2],coords[,1],coords[,2])
          #=> very small dilatation using the gBuffer facility in library(rgeos): to remove the useless points in Fjords
    
       coord <- coord[which(res==0),]
       map("worldHires",  xlim=c(min(grid1.xy[,1]),max(grid1.xy[,1])), ylim=c(min(grid1.xy[,2]),max(grid1.xy[,2])))
       points(coord[,"x"], coord[,"y"])
       }
 # remove artefact UK
 #rect(-2.5, 51.0, -4.5, 52.0, col=2)      
 coord <- coord[!(coord[,1]< -2.5 & coord[,1]> -4.5 & coord[,2]> 51.0 & coord[,2]< 52.0),]
 # remove artefact UK
 #rect( -3.25, 51.6, -5.6, 49.9)      
 coord <- coord[!(coord[,1]< -3.25 & coord[,1]> -5.6 & coord[,2]> 49.9 & coord[,2]< 51.6),]
 # remove artefact SWD
 #rect(14.7, 62.0, 28, 68.0, col=2)     
 coord <- coord[!(coord[,1]< 28 & coord[,1]> 14.7 & coord[,2]> 62 & coord[,2]< 68.0),]
 # remove artefact FRA
 #rect(-0.85, 49.82, -1.95, 47.88)     
 coord <- coord[!(coord[,1]< -0.85 & coord[,1]> -1.95 & coord[,2]> 47.88 & coord[,2]< 49.82),]
 
 # caution: add to coord some nodes to avoid link over land for denmark Esperg
 # coord <-  rbind(coord, cbind(c(8.254036, 8.104059, 8.196352, 9.696125 ,9.592294,  8.265572, 8.277109, 8.323256, 8.565527,  8.41555, 10.03069),
 #  c(55.33099, 55.12831, 54.77024, 55.09453, 55.12155, 53.93249, 54.06086, 54.16895, 54.18247, 53.86493, 54.79727), c(0,0,0,0,0, 0,0,0, 0, 0, 0)  ))
      
 # ...and save temporary object
 save(coord, file="C:\\temp\\coord.Rdata")


 
 # add harbour nodes to coord
 load("C:\\temp\\coord.Rdata")
 coord <-  coord[!is.na(coord[,"x"]),]

 
 coord <- as.matrix(coord)
 coord <- rbind(coord, harbours)
 coord[,"x"] <- as.numeric(coord[,"x"] )
 coord[,"y"] <- as.numeric(coord[,"y"] )

 # reorder to get increasing x i.e. longitude
 idx.for.reorder <-  order(coord[,"x"], decreasing=FALSE)
 coord <- coord [idx.for.reorder,]



 # graph-based Neighbours
 library(spdep)
 nbk <- 8
 map("worldHires", xlim=c(min(grid1.xy[,1]),max(grid1.xy[,1])), ylim=c(min(grid1.xy[,2]),max(grid1.xy[,2])))
 plot(knn2nb(knearneigh(coord[,c("x","y")], k=nbk, longlat = TRUE),
                 row.names=1:nrow(coord)) , coord[,c("x","y")], add=TRUE, cex=0.3,col=grey(0.5))
 graph.neightb <- knearneigh(coord[,c("x","y")], k=nbk, longlat = TRUE)
 # get distance
 dd <- nbdists(knn2nb(graph.neightb) , coord[,c("x","y")], longlat = TRUE)  # return distance in km
 #dd <- nbdists(nb , coord, longlat = TRUE)  # return distance in km

 # build the graph object compatible with igraph
 node      <- rep(1:nrow(graph.neightb$nn), each=nbk)
 neighbour <- matrix(t(graph.neightb$nn), ncol=1)
 dist.km   <- unlist(dd)
 graph     <- cbind(node, neighbour, dist.km)

 # CAUTION: DOES NOT WORK BECAUSE SOME PTS ARE ERASED IN THE PROCESS AND THEN COORD AND GRAPH ARE INCONSISTENT
 # reduce the number of neigbours for harbours
 # caution : when removing link it is possible to get
 # less than 5 neigbours for remaining port when port to port links exist
 idx.harbor.in.coord <-  as.numeric(which(coord[,"idx.port"]!=0))
 for(a.port in idx.harbor.in.coord) {
      # col 1
      connect.for.this.port <- graph[graph[,1]==a.port,] # all neighbours of this port
     if(length(connect.for.this.port)>=2*3){
         connect.for.this.port <- connect.for.this.port[order(connect.for.this.port[,"dist.km"]),][1:2,] # keep the 5 nearest neighbours
         graph <- graph[graph[,1]!=a.port,]  # remove
         graph <- rbind (graph, connect.for.this.port) # add
         }
      # col 2
      connect.for.this.port <- graph[graph[,2]==a.port,] # all neighbours of this port
      if(length(connect.for.this.port)>=2*3){
         connect.for.this.port <- connect.for.this.port[order(connect.for.this.port[,"dist.km"]),][1:2,] # keep the 5 nearest neighbours
         graph <- graph[graph[,2]!=a.port,]  # remove
         graph <- rbind (graph, connect.for.this.port) # add
         }
      }

 # restore the few ports which have been potentially lost when removing links...
 the.disappeared.ports.idx <- c(1: max(unique(graph[,1])))[! c(1: max(unique(graph[,1]))) %in% unique(graph[,1])]
 if(length(the.disappeared.ports.idx)>0){
     init.graph     <- cbind(node, neighbour, dist.km)
     this.unfortunate.port <- init.graph[init.graph[,1] %in% the.disappeared.ports.idx,]
     graph <- rbind (graph, this.unfortunate.port)
     }


   graph <- graph[order(graph[,1]),] # VERY VERY ESSENTIAL (because see ** below)

 # build the igraph object
 library(igraph)
 vertices    <- data.frame(name=as.character(unique(graph[,1])), x=coord[,1], y=coord[,2]) #(**)
 edges        <- data.frame(from=c(graph[,1]),
                        to=c(graph[,2]),
                        dist.km=graph[,3])
 g <- graph.data.frame(edges, directed=FALSE, vertices=vertices)
 print(g, e=TRUE, v=TRUE)

 # plot the graph
 x1 <- coord[graph[,1],1]
 y1 <- coord[graph[,1],2]
 x2 <- coord[graph[,2],1]
 y2 <- coord[graph[,2],2]
 segments(x1,y1,x2,y2,col=3)

 # add harbours
 coord.ports <- coord[coord[,"idx.port"]!=0,]
 points(coord.ports[,1],coord.ports[,2], col=2, pch=16)

return( list(g=g, graph=graph, coord=coord, harbour.list=harbour.list) )
}






    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
a.shortest.path.old.igraph <- function (from = 6, to = 1001, g=g, is.plot=FALSE, a.color=2) {
from <- from - 1; to <- to - 1 # CAUTION! idx from 0 in g (!)
path <- unlist(get.shortest.paths(g, from, to , weights= g[[9]][[4]]$dist.km)) # dijkstra´s algorithm in library(igraph)
x.path <- g[[9]][[3]]$x[path+1]
y.path <- g[[9]][[3]]$y[path+1]
if(is.plot){
  lines(x.path,y.path,col=a.color, lwd=1.6)
  points(x.path,y.path,col=a.color, pch=16, cex=0.6)
  # i.e start point is points(coord[from,1], coord[from,2], col=3,pch=16)
  # i.e stop point is points(coord[to,1], coord[to,2],col=3, pch=16)
  }
return(path+1) # index pts compatible with coord
}

a.shortest.path <- function (from = 6, to = 1001, g=g, is.plot=FALSE, a.color=2) {
path <- unlist(get.shortest.paths(g, from, to , weights= get.edge.attribute(g, "dist.km"))) # dijkstra´s algorithm in library(igraph)
x.path <-get.vertex.attribute(g, "x")[path]
y.path <- get.vertex.attribute(g, "y")[path]
if(is.plot){
  lines(x.path,y.path,col=a.color, lwd=1.6)
  points(x.path,y.path,col=a.color, pch=16, cex=0.6)
  # i.e start point is points(coord[from,1], coord[from,2], col=3,pch=16)
  # i.e stop point is points(coord[to,1], coord[to,2],col=3, pch=16)
  }
return(path) # index pts compatible with coord
}


     #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 destB.f <- function(A, angleAB, dist.km){
   # CAUTION : LONGLAT
      Rearth = 6378.388 # in km
      # remind to convert from the degree to radian after input dividing by p
      # and from radian to degree before output multiplying by p
      p = 180/pi
       lat2 <-  asin(sin(A[,2]/p)*cos(dist.km/Rearth) + cos(A[,2]/p)*sin(dist.km/Rearth)*cos(angleAB/p))
       lon2 <-  A[,1]/p + atan2(sin(angleAB/p)*sin(dist.km/Rearth)*cos(A[,2]/p), cos(dist.km/Rearth)-sin(A[,2]/p)*sin(lat2))
       #lon2 <- ((lon2+pi)%%(2*pi)) -pi
        return(cbind(lon2*p,lat2*p))
        }
        
   #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 distAB.f <- function(A,B, .unit="km"){
 # CAUTION : LONGLAT
 # return the dist in km or nautical miles.
 # formula = cos (gamma) = sin LatA sin LatB + cos LatA cos LatB cos delta
 # if gamma in degree, knowing that 1 degree = 60 minutes = 60 nm, then gamma in degree * 60 = dist in nm
 # we can also use the radius R of the earth: the length of the arc AB is given by R*gamma with gamma in radians
        p = 180/pi;  Rearth = 6378.388 # in km
        nm <- 1852e-3 # 1852 m for 1 nm
        res <- switch(.unit,
           km = Rearth * acos(sin(A[,2]/p)*sin(B[,2]/p) + (cos(A[,2]/p) * cos(B[,2]/p)*cos(A[,1]/p - B[,1]/p))),
           nm = Rearth * acos(sin(A[,2]/p)*sin(B[,2]/p) + (cos(A[,2]/p) * cos(B[,1]/p)*cos(A[,1]/p - B[,1]/p))) / nm
           )
       names(res) <- NULL
        return(res)
        }

    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  bearingAB.f  <- function(A,B, .unit="km"){
    # CAUTION : LONGLAT
      p = 180/pi
         bearing <- atan2(   sin((B[,1]/p)-(A[,1]/p))*cos(B[,2]/p), cos(A[,2]/p)*sin(B[,2]/p)-sin(A[,2]/p)*cos(B[,2]/p)*cos((B[,1]/p)-(A[,1]/p)) )
        return(bearing*p)
        }
              


   #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
dist.paths <- function(from=1, to=c(100,200), g=g, coord=coord){
   short.path.dists <- rep(0, length(to))
   for(i in 1: length(to)){
     a.sh.path <- a.shortest.path  (from = from, to = to[i], g=g, is.plot=FALSE, a.color=4)
     mat <- coord[a.sh.path,]
     if(length(mat)>3){
         short.path.dists[i] <-  sum (distAB.f(A=mat, B=rbind(mat[-1,],mat[nrow(mat),]), .unit="km"  ))
       } else{short.path.dists[i] <- 0} # in that case, dep pt = arr pt!
     }

return(short.path.dists)
}








###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################



if(FALSE){

general <- list()
general$lim.lat <- c(49,70) # default
general$lim.long <-  c(-8.5,25) # default
general$main.path <- file.path("C:","Users","fba","Dropbox","ibm_vessels_param")


res <- create_graph(general)
g=res$g; graph=res$graph; coord=res$coord; harbour.list=res$harbour.list


 # plot the graph
library(maps)
library(mapdata)
windows(15,15)
 map("worldHires", xlim=general$lim.long, ylim=general$lim.lat)
 #zoom: map("worldHires", xlim=c(5,14), ylim=c(53,56))

 x1 <- coord[graph[,1],1]
 y1 <- coord[graph[,1],2]
 x2 <- coord[graph[,2],1]
 y2 <- coord[graph[,2],2]
 segments(x1,y1,x2,y2,col=3)

 # add harbours
 coord.ports <- coord[coord[,"idx.port"]!=0,]
 points(coord.ports[,1],coord.ports[,2], col=2, pch=16)

 # a shortest path
 a.sh.path <- a.shortest.path  (from = 2, to = 3170, g=g, is.plot=TRUE, a.color=4)

# save a graph (do not erase previous ones i.e. increment 'num')
 lst <- list.files(path =  file.path(general$main.path, "igraph"), pattern = "graphibm")
 num <- max(as.numeric(unlist(lapply(strsplit (lst, split="_"), function(x) x[1]))))+1
 save('g','graph','coord', 'harbour.list', file = file.path(general$main.path, "igraph",
         paste(num,"_graphibm.RData",sep="")))



## FILES FOR BUILDING A IGRAPH
load(file.path(general$main.path,"igraph","4_graphibm.RData")) # built from the R code

 # save a plot
 
  png(filename=file.path(general$main.path,"igraph",
       paste("map_graph4_points.png",sep='')),
                                   width = 1600, height = 1600, 
                                   units = "px", pointsize = 12,  res=300)

 #windows(5,5)
 plot(coord[,1], coord[,2], pch=".", xlab="", ylab="", xlim=c(-8,23), ylim=c(52,65))
 library(maptools)
 ices_areas <- readShapeSpatial("C:\\Users\\fba\\Dropbox\\ibm_vessels_param\\ices_areas\\ices_areas")
 library(maptools)
 sh1 <- readShapePoly(file.path("C:","Users", "fba", "Dropbox", "ibm_vessels_param","shp","francois_EU"))
 plot(ices_areas, col="white",  add=TRUE)
 plot(sh1,  col=grey(0.7), add=TRUE)
 points(coord[,1], coord[,2], pch=".")
 box()
 #map("worldHires", add=TRUE)
library(igraph)
 a.shortest.path  (from = 2, to = 3170, g=g, is.plot=TRUE, a.color=5)
a.shortest.path  (from = 725, to = 2341, g=g, is.plot=TRUE, a.color=3)
 text(3,57, "North Sea", cex=0.8)
 text(17.9,55.5, "Baltic Sea", cex=0.8)
 text(11,58.5, "Skagerrak Kattegat", cex=0.8)
 
  mtext(side=1, "Longitude", cex=1, adj=0.5, line=2)
    mtext(side=2, "Latitude", cex=1, adj=0.5, line=2)
    mtext(side=3, "(a)", cex=1.5, adj=0, line=1)


 #savePlot(filename=file.path(general$main.path,"igraph",
 #      paste("map_graph4_points.jpeg",sep='')), type="jpeg")
 dev.off()
 
# using ggmap
coord.ports <- data.frame(coord.ports)
coord.ports$color <- factor(sample(c(1,2), size=nrow(coord.ports),replace=TRUE)) # fake just to test
require(ggmap)
require(mapproj)
map.center <- geocode("copenhaguen")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom=7)
SHmap + geom_point(
  aes(x=x, y=y, colour= color), data=coord.ports) +
  scale_colour_manual(values=c("1"="dark blue","2"="orange"))+
  labs(colour=c("port type"))


# RE-BUILD THE IGRAPH OBJECT IN R TO SHOW WHAT IS NECESSARY AS INPUT
 library(igraph)
 vertices    <- data.frame(name=as.character(unique(graph[,1])), x=coord[,1], y=coord[,2]) #(**)
 edges        <- data.frame(from=c(graph[,1]),
                        to=c(graph[,2]),
                        dist.km=graph[,3])
 g <- graph.data.frame(edges, directed=FALSE, vertices=vertices)
 print(g, e=TRUE, v=TRUE)


# export in graphml format for use with C
#write.graph(g, file=file.path("C:","ibm_vessels_param","1_graphibm"), format="graphml")

# export in txt format for use with C
#write.table(signif(coord, 6), file=file.path(general$main.path,"igraph","coord3.dat"),
#    row.names=FALSE, col.names=FALSE, sep=" ")


# export in txt format for use with C
# THIS WAY IS BETTER BECAUSE C IS ROW ORIENTED
write(signif(coord, 6),file=file.path(general$main.path,"igraph", "coord4.dat"), ncol=1)
nrow(coord)
 ncol(coord)
 # remnber that the idx.port is related to the one in harbour.list

# export in txt format for use with C
# THIS WAY IS BETTER BECAUSE C IS ROW ORIENTED
graph [,1:2] <- graph [,1:2] -1
write(signif(graph, 4),file=file.path(general$main.path,"igraph","graph4.dat"), ncol=1)  # CONNECTION BETWEEN NODES + DISTANCE KM
nrow(graph)
 ncol(graph)

# plot: compare version R vs version cpp
plot(0,0,xlim=c(-10,20),ylim=c(40,70),type="n")
a.shortest.path (from = 20, to = 200, g=g, is.plot=TRUE, a.color=2) # WITH IGRAPH
 [1]  20  19  92 119 150 214 287 366 368 438 445 450 514 536 516 490 581 598 624 557 532 504 535
[24] 457 429 417 437 362 390 327 342 267 258 194 211 239 260 200


a.path <- c(20, 19, 92, 172, 247, 266, 346, 366, 368, 438, 445, 468, 450, 514, 536,
516 ,490, 478 ,449 ,481, 464, 467, 455, 479, 457, 429, 417 ,385,
326 ,296 ,274 ,257 ,198 ,162 ,149 ,138 ,155,217,200)   # WITH THE HOME MADE C++ VERSION
points(coord[a.path, 1], coord[a.path,2], col=1)
for(i in 1:(length(a.path)-1)) segments(coord[a.path[i], 1], coord[a.path[i],2], coord[a.path[i+1], 1], coord[a.path[i+1],2], col=1)

# list of harbour nodes
which(coord[,"idx.port"]!=0)-1


##--------------------------------------------------------------------------##
##-------------ALSO EXPORT THE DEFAULT NODES IN POLYGONS GRAPH NODES -------##
##--------------------------------------------------------------------------##


idx_nodes_cpp_in_polygons <- matrix("", ncol=2)
colnames(idx_nodes_cpp_in_polygons) <- c("polygon", "node")

#=> just generate empty files at this stage for this graph...
# => see IBM_param_step2bis.r

## copy/paste for quarters

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path, "igraph",
             paste("nodes_in_polygons_a_graph4_quarter1",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
  
write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path, "igraph",
             paste("nodes_in_polygons_a_graph4_quarter2",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path, "igraph",
             paste("nodes_in_polygons_a_graph4_quarter3",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path, "igraph",
             paste("nodes_in_polygons_a_graph4_quarter4",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)




} # end False