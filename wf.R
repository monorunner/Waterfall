print.waterfall <- function(dt, bar.width = 9, 
			    initial.height = 16, gap = 0) {

    nbar <- nrow(dt)

    numbers <- dt[, 2]
    x <- numbers[1] / initial.height

    WF <- NULL
    WF[[1]] <- add.bar(bar.width, initial.height)
    rownames(WF[[1]]) <- 1:16
    
    all.rows <- rownames(WF[[1]])

    for (i in 2:nbar) {
    	WF[[i]] <- add.bar(bar.width, floor(abs(numbers[2]) / x))
	rownames(WF[[i]]) <- seq(as.numeric(rownames(WF[[i-1]])[1]), 
	    		      len = floor(abs(numbers[2] / x)),
	    		      by = -numbers[2]/abs(numbers[2]))
	all.rows <- c(all.rows, rownames(WF[[i]]))
    }

    all.rows <- unique(all.rows)
    all.rows <- all.rows[order(as.numeric(all.rows))]

    CHART <- matrix(" ", nrow = length(all.rows),
		    ncol = (bar.width) * nbar + 1)
    CHART[, bar.width * nbar + 1] <- "\n"
    rownames(CHART) <- all.rows
	
    x0 <- (0 : (nbar-1)) * (bar.width + gap) + 1 
    x1 <- x0 + bar.width - 1

    for (i in 1:nbar) {
	CHART[rownames(WF[[i]]), x0[i] : x1[i]]  <- WF[[i]]
    }

    cat(t(CHART), sep = "")

}

add.hline <- function(width = 9) {
    return(c("+", rep("-", width - 2), "+"))
}

add.vline <- function(height = 16) {
    return(c("+", rep("|", height - 2), "+"))
}


add.bar <- function(width = 9, height = 16) {

    if(width < 2) stop("Width too small.")
    if(height < 2) stop("Height too small.")
    BAR <- matrix(" ", ncol = width, nrow = height)
    # BAR[, width+1] <- "\n"
    BAR[1, 1:width] <- BAR[height, 1:width]<- add.hline(width)
    BAR[, 1] <- BAR[, width] <- add.vline(height)
    return(BAR)
    # cat(t(BAR), sep = "")

}
    

 
