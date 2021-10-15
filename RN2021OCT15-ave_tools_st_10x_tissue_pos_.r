#Rachel Yuan Nong Mikkelsen 2021-OCT-15
#stare at tissue area including a number of features | R locator

print("place screen pointer at four locations on a image, i.e. on a axis a1, a2, a3, a4 that difference between a1 and a4 > a2 and a3 while a1 and a4 are the minimum and maximum values among them...")

edge_pts <- locator(n = 4)
edge_pts_x_max <- round(max(edge_pts[[1]]))
edge_pts_x_min <- round(min(edge_pts[[1]]))

edge_pts_y_max <- round(max(abs(edge_pts[[2]])))
edge_pts_y_min <- round(min(abs(edge_pts[[2]])))

tissue_by_edge_pts <- tissue_position[tissue_position[,6] > edge_pts_x_min & tissue_position[,6] < edge_pts_x_max & tissue_position[,5] > edge_pts_y_min & tissue_position[,5] < edge_pts_y_max,]

points(tissue_by_edge_pts[,6], -tissue_by_edge_pts[,5], col = "purple", pch = 19)
print("region in tissue_by_edge_pts:")
 
