print.waterfall <- function(dt, bar.width = 9, 
			    initial.height = 16, gap = 0) {
    
    if(bar.width < 2) stop("Width too small.")
    
    nbar <- nrow(dt)

    steps <- as.character(dt[, 1])
    numbers <- dt[, 2]
    x <- numbers[1] / initial.height

    WF <- NULL
    WF[[1]] <- add.bar(bar.width, initial.height)
    rownames(WF[[1]]) <- 1:initial.height
    
    all.rows <- rownames(WF[[1]])

    for (i in 2:nbar) {
    	WF[[i]] <- add.bar(bar.width, floor(abs(numbers[i]) / x))
	rownames(WF[[i]]) <- seq(min(as.numeric(rownames(WF[[i-1]]))), 
	    		      len = nrow(WF[[i]]),
	    		      by = -numbers[i]/abs(numbers[i]))
	all.rows <- c(all.rows, rownames(WF[[i]]))
    }

    all.rows <- unique(all.rows)
    all.rows <- all.rows[order(as.numeric(all.rows))]

    CHART <- matrix(" ", nrow = length(all.rows),
		    ncol = (bar.width + gap) * nbar + 1)
    CHART[,  (bar.width + gap) * nbar + 1] <- "\n"
    rownames(CHART) <- all.rows
	
    x0 <- (0 : (nbar-1)) * (bar.width + gap) + 1 
    x1 <- x0 + bar.width - 1

    for (i in 1:nbar) {
	CHART[rownames(WF[[i]]), x0[i] : x1[i]]  <- WF[[i]]
    }

    texts <- strsplit(steps, split = " ")
    texts <- lapply(texts, abbreviate, min = bar.width - 1, dot = TRUE)
    texts <- lapply(texts, format, width = bar.width + gap)

    textrow <- max(unlist(lapply(texts, length)))
    for(i in 1:nbar) {
	while(length(texts[[i]]) < textrow)
	    texts[[i]] <- c(texts[[i]], format("", width = bar.width + gap))
    }

    TEXT <- matrix(unlist(strsplit(do.call(rbind, texts), split = "")), 
		   nrow = textrow, byrow = TRUE)
    TEXT  <- cbind(TEXT, "\n")

    CHART <- rbind(CHART, TEXT)


    cat(t(CHART), sep = "")

}

add.hline <- function(width = 9) {
    return(c("+", rep("-", width - 2), "+"))
}

add.vline <- function(height = 16) {
    return(c("+", rep("|", height - 2), "+"))
}


add.bar <- function(width = 9, height = 16) {

    if(height < 2) BAR <- matrix(add.hline(width), nrow = 1)
    else {
	BAR <- matrix(" ", ncol = width, nrow = height)
	BAR[1, 1:width] <- BAR[height, 1:width]<- add.hline(width)
	BAR[, 1] <- BAR[, width] <- add.vline(height)
	return(BAR)
    }
}
    

dt <- data.frame(step = c("Experian White Space", "OC&C Catchments",
			  "Population Too Low"), 
		 count = c(12, 4, -6)) 
