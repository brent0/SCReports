require(grid)
tab.4.tex = function(d,...){
  gridExtra::grid.table(d, show.rownames = FALSE, show.colnames = TRUE, row.just = "center", show.hlines=T, col.just="center", separator="black", show.box=T, gpar.colfill = grid::gpar(fill = "blue3", col = "black"), gpar.coltext = gpar(col = "white", cex = 1, fontface = "bold"), gpar.rowfill = grid::gpar(fill = "white", col = "black"), gpar.corefill = grid::gpar(fill = "white", col = "black"), ...)
}