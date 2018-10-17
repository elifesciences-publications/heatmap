library("png")

heatmap3 = function(x, outFile = c(NA), rowClust = c(F), colClust = c(T), col = c(heat.colors(100)), annotCol = c(NA), annotRow = c(NA), heatDim = c(3, 3), annotWid = c(0.25), clusterWid = c(0.4), key = c(T), keyWid = c(0.3), flatten = c(T), res = c(300), outMai = c(0.1, 0.1, 0.1, 0.1), norm = c(NA), scale = c(NA), clustMeth = c("average"), colLab = c(NA), rowLab = c(NA), labCex = c(1), labDim = c(0.2, 0.2), annotSp = c(0.025), height = c(6), width = c(6), flattenColClust = c(flatten), flattenRowClust = c(flatten), flattenKey = c(flatten), symm = c(F), colDen.lwd = c(1), rowDen.lwd = c(0.1), tcl = c(-0.2), ...) {

	#Debug
	#x = as.matrix(dmlp); outFile = NA; rowClust = F; colClust = F; col = ramp(1000); annotRow = NA; annotCol = NA; annotRow = NA; colLab = NA; rowLab = NA; heatDim = c(2, 2); annotWid = 0.25; clusterWid = 0.4; key = T; keyWid = 0.3; flatten = T; res = 300; labDim = c(0.1, 0.1); height = 3; width = 3; outMai = c(0.1, 0.1, 0.1, 0.1); norm = NA; scale = NA; clustMeth = "average"; annotSp = 0.025; flattenRowClust = F; flattenColClust = F; labCex = 1; flattenKey = T;
	#x = as.matrix(dmlp); annotCol = sample.colors; annotRow = dml.colors; flatten = T; scale = c(0, 1);

	if (!is.na(outFile)) cairo_pdf(outFile, height = height, width = width) else dev.new(height = height, width = width);

	org = x	

	#If normalization is required normalize
	if (!is.na(norm) & norm == "row") {
		rm <- rowMeans(x, na.rm = T);
		x <- sweep(x, 1, rm);
		sx <- apply(x, 1, sd, na.rm = T);
		x <- sweep(x, 1, sx, "/");
	}

	if (!is.na(norm) & norm == "column") {
		cm <- colMeans(x, na.rm = T);
		x <- sweep(x, 2, cm);
		sx <- apply(x, 2, sd, na.rm = T);
		x <- sweep(x, 2, sx, "/");
	}

	#determine the number of annotation rows (ars) and columns (acs)
	if (all(is.na(annotCol))) acs = 0 else if (grepl("matrix|data.frame", class(annotCol))) acs = dim(annotCol)[2] else if (grepl("character", class(annotCol))) acs = 1
	if (all(is.na(annotRow))) ars = 0 else if (grepl("matrix|data.frame", class(annotRow))) ars = dim(annotRow)[2] else if (grepl("character", class(annotRow))) ars = 1

	#Cluster columns and rows if required
	if (colClust) {
		#set distance to 0 for NA
		if (symm) d = dist(x) else d = dist(t(x))
		d[is.na(d)] = 0

		colCluster = hclust(d, method = clustMeth)
		colDen = as.dendrogram(colCluster)
		if (symm) colDen = reorder(colDen, rowMeans(org, na.rm = T)) else colDen = reorder(colDen, colMeans(org, na.rm = T))
		x = x[, if (symm) rev(order.dendrogram(colDen)) else order.dendrogram(colDen)]; #order matrix by cluster

	}
	if (rowClust) {
		#set distance to 0 for NA
		d = dist(x)
		d[is.na(d)] = 0
		
		rowCluster = hclust(d, method = clustMeth)
		rowDen = as.dendrogram(rowCluster)
		rowDen = reorder(rowDen, rowMeans(org, na.rm = T))
		x = x[order.dendrogram(rowDen), ]; #order matrix by cluster
	}	

	#determine rows and columns for layout
	rows = (if (key & keyWid > 0) 2 else 1) + (if (labDim[1] > 0) 1 else 0) + acs + (if (colClust) 1 else 0);
	hs = c((if(key  & keyWid > 0) keyWid else NULL), if (labDim[1] > 0) labDim[1] else NULL, heatDim[1], rep(annotWid, acs), if (colClust) clusterWid else NULL) 

	cols = (if (labDim[2] > 0) 1 else 0) + 1 + ars + (if(rowClust) 1 else 0);
	ws = c((if (labDim[2] > 0) labDim[2] else NULL), heatDim[2], rep(annotWid, ars), clusterWid)

	#make layout
	l = matrix(c((rows * cols):1), nrow = rows, ncol = cols)
	layout(l, widths = ws[cols:1], heights = hs[rows:1])
	#layout.show(length(l))

	if (labDim[2] > 0) {
		if (key) { par(mai = c(0, 0, 0, 0)); plot.new(); }
		if (labDim[1] > 0) { par(mai = c(0, 0, 0, 0)); plot.new(); }
	
		#print rowLabels
		if (all(is.na(rowLab))) { 
			par(mai = c(0, 0, 0, 0))
			plot.new()
		} else {
			par(mai = c(0, 0, 0, 0))
			plot(NA, xaxs = "i", yaxs = "i", ylab = NA, xaxt = "n", yaxt = "n", xlab = NA, xlim = c(0, 1), ylim = c(0, dim(x)[1]), bty = "n")
			if (rowClust) rowLab = rowLab[order.dendrogram(rowDen)]

			text(rep(0, dim(x)[1]), y = 0:dim(x)[1] + 0.5, labels = as.character(rowLab), pos = 4, cex = labCex, family = "Arial");
		}

		#Skip blank regions in layout 
		if (acs > 0) for (j in 1:acs) { par(mai = c(0, 0, 0, 0)); plot.new(); }
		if (colClust) { par(mai = c(0, 0, 0, 0)); plot.new(); }
	}

	#make key
	if (key) {
		#make key flat
		if (flattenKey) {
			png("temp.png", height = keyWid * res, width = heatDim[2] * res, res = res)
			par(mai = c(0, 0, 0, 0), tcl = tcl)
			plot(NA, xaxs = "i", yaxs = "i", ylab = NA, xaxt = "n", yaxt = "n", xlab = NA, ylim = c(0, 1), xlim = c(0, length(col)))
			rect(1:length(col) - 1, rep(0, length(col)), 1:length(col), rep(1, length(col)), col = col, border = NA)
			dev.off();
			flat <- readPNG("temp.png");
			file.remove("temp.png");
	
			par(mai = c(outMai[1], if (all(is.na(annotRow)) & !rowClust) outMai[2] else 0, 0, if (all(is.na(rowLab))) outMai[4] else 0), tcl = tcl)
			plot(NA, xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA, ylim = c(0, 1), xlim = c(0, length(col)))
			rasterImage(flat, par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4]);
		} else {	
			par(mai = c(outMai[1], if (all(is.na(annotRow)) & !rowClust) outMai[2] else 0, 0, if (all(is.na(rowLab))) outMai[4] else 0), tcl = tcl)
			plot(NA, xaxs = "i", yaxs = "i", ylab = NA, xaxt = "n", yaxt = "n", xlab = NA, ylim = c(0, 1), xlim = c(0, length(col)))
			rect(1:length(col) - 1, rep(0, length(col)), 1:length(col), rep(1, length(col)), col = col, border = NA)
		}
		xat = seq(from = 0, to = length(col), by = length(col) / 5)

		if (all(!is.na(scale))) {
			axis(side = 1, at = xat, labels = c(scale[1], rep(NA, length(xat) - 2), scale[2]), tick = T, font = 2, cex.axis = 1.2, xaxs = "i")
		} else {
			axis(side = 1, at = xat, labels = c(min(x, na.rm = T), rep(NA, length(xat) - 2), max(x, na.rm = T)), tick = T, font = 2, cex.axis = 1.2, xaxs = "i")
		}
	}

	###Column labels
	if (labDim[1] > 0) {
		if (!all(is.na(colLab))) {
			par(mai = c(0, if (all(is.na(annotRow)) & !rowClust) outMai[2] else 0, 0, if (all(is.na(rowLab))) outMai[4] else 0))
			plot(NA, xaxs = "i", yaxs = "i", ylab = NA, xaxt = "n", yaxt = "n", xlab = NA, xlim = c(0, dim(x)[2]), ylim = c(0, 1), bty = "n")
			if (colClust) colLab = colLab[order.dendrogram(colDen)]
			if (symm) colLab = rev(colLab)

			text(1:dim(x)[2] - 0.5, rep(1, dim(x)[2]), labels = colLab, adj = 1, cex = labCex, srt = 90, family = "Arial"); #
		} else {
			par(mai = c(0, 0, 0, 0));	
			plot.new();
		}
	}

	#### Main heatmap
	if (flatten) {
		png("temp.png", height = heatDim[1] * res, width = heatDim[2] * res, res = res)
		par(mai = c(0, 0, 0, 0))
	
		if (all(!is.na(scale))) {		
			xScale = x
			xScale[xScale < scale[1]] = scale[1]
			xScale[xScale > scale[2]] = scale[2]		

			image(as.matrix(t(xScale)), col = col, xaxt = "n", yaxt = "n", breaks = seq(from = scale[1], to = scale[2], by = diff(scale) / length(col)))
		} else {
			image(as.matrix(t(x)), col = col, xaxt = "n", yaxt = "n")
		}

		dev.off();
		flat <- readPNG("temp.png");
		file.remove("temp.png");
	
		par(mai = c(0, if (all(is.na(annotRow)) & !rowClust) outMai[2] else 0, 0, if (all(is.na(rowLab))) outMai[4] else 0))
		plot(NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, main = NA, ylim = c(0, dim(x)[1]), xlim = c(0, dim(x)[2]))
		rasterImage(flat, par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4]);

	} else {
		par(mai = c(0, if (all(is.na(annotRow)) & !rowClust) outMai[2] else 0, 0, if (all(is.na(rowLab))) outMai[4] else 0))
		if (all(!is.na(scale))) {
			xScale = x
			xScale[xScale < scale[1]] = scale[1]
			xScale[xScale > scale[2]] = scale[2]
		
			image(as.matrix(t(xScale)), col = col, xaxt = "n", yaxt = "n", breaks = seq(from = scale[1], to = scale[2], by = diff(scale) / length(col)))
		} else {
			image(as.matrix(t(x)), col = col, xaxt = "n", yaxt = "n")
		}
	}

	#plot annotation col
	if (!all(is.na(annotCol))) {
		if (grepl("character", class(annotCol))) annotCol = as.matrix(annotCol);
		for (i in 1:acs) {

			if (colClust) annotCol[, i] = annotCol[order.dendrogram(colDen), ]		
		
			
			if (flattenColClust) {
				png("temp.png", height = annotWid * res, width = heatDim[2] * res, res = res)
				par(mai = c(0, 0, 0, 0))
				plot(NA, xlim = c(0, dim(x)[2]), ylim = c(0, 1), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA)
				rect((1:dim(x)[2]) - 1, rep(0, dim(x)[2]), 1:dim(x)[2], rep(1, dim(x)[2]), col = annotCol[, i], border = NA)
				dev.off();
				flat <- readPNG("temp.png");
				file.remove("temp.png");
	
				par(mai = c(annotSp, if (all(is.na(annotRow)) & !rowClust) outMai[2] else 0, annotSp, if (all(is.na(rowLab))) outMai[4] else 0), bty = "n")
				plot(NA, xlim = c(0, dim(x)[2]), ylim = c(0, 1), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA)
				rasterImage(flat, par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4]);
			} else {
				
				par(mai = c(annotSp, if (all(is.na(annotRow)) & !rowClust) outMai[2] else 0, annotSp, if (all(is.na(rowLab))) outMai[4] else 0))
				plot(NA, xlim = c(0, dim(x)[2]), ylim = c(0, 1), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA)
				rect((1:dim(x)[2]) - 1, rep(0, dim(x)[2]), 1:dim(x)[2], rep(1, dim(x)[2]), col = annotCol[, i], border = NA)
			}
		}
	}

	#plot cluster column
	if (colClust) {
		if (flattenColClust) {
			png("temp.png", height = clusterWid * res, width = heatDim[2] * res, res = res)
			par(mai = c(0, 0, 0, 0))
			if (symm) plot(rev(colDen), axes = F, ann = F, xaxs = "i", lwd = colDen.lwd) else plot(colDen, axes = F, ann = F, xaxs = "i", lwd = colDen.lwd)
			dev.off();
			flat <- readPNG("temp.png");
			file.remove("temp.png");

			par(mai = c(0, if (all(is.na(annotRow)) & !rowClust) outMai[2] else 0, outMai[3], if (all(is.na(rowLab))) outMai[4] else 0))
			plot(NA, xlim = c(0, 1), ylim = c(0, dim(x)[1]), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA)
			rasterImage(flat, par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4]);
		} else {				
			par(mai = c(0, if (all(is.na(annotRow)) & !rowClust) outMai[2] else 0, outMai[3], if (all(is.na(rowLab))) outMai[4] else 0))
			if (symm) plot(rev(colDen), axes = F, ann = F, xaxs = "i", lwd = colDen.lwd) else plot(colDen, axes = F, ann = F, xaxs = "i", lwd = colDen.lwd)
		}
	}	

	#plot annotation rows
	if (ars > 0) {
		if (grepl("character", class(annotRow))) annotRow = as.matrix(annotRow);
		if (rowClust) annotRow = as.matrix(annotRow[order.dendrogram(rowDen), ]); #order by rowClust

		for (i in 1:ars) {
			par(mai = c(0, 0, 0, 0))
			if (key) { par(mai = c(0, 0, 0, 0)); plot.new(); }
			if (labDim[1] > 0) { par(mai = c(0, 0, 0, 0)); plot.new(); }
		
			#plot annotation row
			if (flattenRowClust) {
				png("temp.png", height = heatDim[1] * res, width = annotWid * res, res = res)
				par(mai = c(0, 0, 0, 0))
				plot(NA, xlim = c(0, 1), ylim = c(0, dim(x)[1]), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA)
				rect(rep(0, dim(x)[1]), (1:dim(x)[1]) - 1,  rep(1, dim(x)[1]), 1:dim(x)[1], col = annotRow[, i], border = NA)
				dev.off();
				flat <- readPNG("temp.png");
				file.remove("temp.png");

				par(mai = c(0, annotSp, 0, annotSp))
				plot(NA, xlim = c(0, 1), ylim = c(0, dim(x)[1]), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA)
				rasterImage(flat, par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4]);
			} else {	
				par(mai = c(0, annotSp, 0, annotSp))
				plot(NA, xlim = c(0, 1), ylim = c(0, dim(x)[1]), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA)
				rect(rep(0, dim(x)[1]), (1:dim(x)[1]) - 1,  rep(1, dim(x)[1]), 1:dim(x)[1], col = annotRow[, i], border = NA)
			}
		
			if (acs > 0) for (j in 1:acs) { par(mai = c(0, 0, 0, 0)); plot.new(); }
			if (colClust) { par(mai = c(0, 0, 0, 0)); plot.new(); }
		}
	}
		
	#plot row cluster
	if (rowClust) {
	
		if (key) { par(mai = c(0, 0, 0, 0)); plot.new() }

		if (labDim[1] > 0) { par(mai = c(0, 0, 0, 0)); plot.new(); }

		if (flattenRowClust) {
			png("temp.png", height = heatDim[1] * res, width = clusterWid * res, res = res)
			par(mai = c(0, 0, 0, 0))
			plot(rowDen, horiz = T, axes = F, ann = F, yaxs = "i", lwd = rowDen.lwd)
			dev.off();
			flat <- readPNG("temp.png");
			file.remove("temp.png");

			par(mai = c(0, outMai[2], 0, 0), bty = "n")
			plot(NA, xlim = c(0, 1), ylim = c(0, dim(x)[1]), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, main = NA)
			rasterImage(flat, par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4]);
		} else {				
			par(mai = c(0, outMai[2], 0, 0))
			plot(rowDen, horiz = T, axes = F, ann = F, yaxs = "i", lwd = rowDen.lwd)
		}

		if (colClust) { par(mai = c(0, 0, 0, 0)); plot.new(); }
		if (acs > 0) for (i in 1:acs) { par(mai = c(0, 0, 0, 0)); plot.new(); }
	}
	if (!is.na(outFile)) dev.off();
}

