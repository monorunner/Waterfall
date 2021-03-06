print.waterfall <- function(dt, bar.width = 9, 
			    initial.height = 16, gap = 1,
			    fill = "-") {
    
    if(bar.width < 2) stop("Width too small.")
    if(nchar(as.character(fill)) > 1) fill <- substr(fill, 1, 1)
  
    # ------ HELPERS ------ #
    # <helpers>
    # helper function to draw a horizontal line
    add.hline <- function(width = 9) {
       return(c("+", rep("-", width - 2), "+"))
    }
    
    # helper function to draw a vertical line
    add.vline <- function(height = 16) {
       return(c("+", rep("|", height - 2), "+"))
    }
    
    # helper function to draw a bar
    add.bar <- function(width = 9, height = 16) {
       
       if(height < 2) BAR <- matrix(add.hline(width), nrow = 1)
       else {
          BAR <- matrix(" ", ncol = width, nrow = height)
          BAR[1, 1:width] <- BAR[height, 1:width]<- add.hline(width)
          BAR[, 1] <- BAR[, width] <- add.vline(height)
          return(BAR)
       }
    }
    
    
    # helper function to prepare data input for waterfall
    # decode 'e', add tag for 'e' and cumsum for 'e' bars
    # 'e' bars are just cumsums to this point
    prepare.dt <- function(dt) {
       
       dt[, 2] <- as.character(dt[ ,2])
       dt$tag <- 0
       dt$tag[which(dt[, 2] == "e")] <- 1
       dt[dt$tag == 1, 2] <- 0
       dt[, 2] <- as.numeric(dt[, 2])
       dt$cumsum <- cumsum(dt[, 2])
       dt[, 2][dt$tag == 1]  <- - dt$cumsum[dt$tag == 1]
       return(dt)
       
    }
    
    
    # helper function to wrap texts
    # start from the second word, then cat the one before
    # so that if the second one is cat'd to the first one
    # the next iteration starts from the originally 3rd word
    cat.prev <- function(words, width) {
       
       if (length(words) == 1) return(words)
       
       for (i in 2:length(words)) {
          if (nchar(paste(words[i-1], words[i])) <= width) {
             words[i] <- paste(words[i-1], words[i])
             words <- words[-(i-1)]
          }
          
          # if the last word has been cat'd
          if(is.na(words[i+1])) break
       }
       
       return(words)
    }
    
    # </helpers>
    
    # ------- FUNCTION ------ #

  
    dt  <- prepare.dt(as.data.frame(dt))
    nbar <- nrow(dt)
    if(nbar < 3) stop("Less then 3 bars. Invalid input.")

    # the descriptions
    steps <- as.character(dt[, 1])
    numbers <- dt[, 2]
    # x is ratio of number to bar height
    x <- numbers[1] / initial.height

    if(max(nchar(as.character(numbers))) > bar.width)
	stop("Bar width too small for number width.")

    # set up waterfall
    WF <- NULL

    # initial bar
    WF[[1]] <- add.bar(bar.width, initial.height)
    rownames(WF[[1]]) <- 1:initial.height
    WF[[1]] [WF[[1]] == " "]  <- fill

    # give every bar row names for final binding by rownames
    all.rows <- rownames(WF[[1]])

    for (i in 2:nbar) {
	
	# if previous > 0, start from top, otherwise bottom
	# unless tag 'e', where count's sign is reversed
	if(dt[i-1 ,2] > 0 | dt$tag[i-1] == 1 & dt[i-1, 2] < 0)
	    bar.start <- min(as.numeric(rownames(WF[[i-1]])))
	else
	    bar.start <- max(as.numeric(rownames(WF[[i-1]])))

	if (dt$tag[i] == 1)
	    WF[[i]] <- add.bar(bar.width,
			       max(as.numeric(all.rows)) - 
				   bar.start + 1)
	else
	    WF[[i]] <- add.bar(bar.width, round(abs(numbers[i]) / x))

	# assign rownames as corresponding postion in waterfall
	rownames(WF[[i]]) <- seq(bar.start, 
	    		      len = nrow(WF[[i]]),
	    		      by = -numbers[i]/abs(numbers[i]))
	if (dt$tag[i] == 1) 
	    WF[[i]] [WF[[i]] == " "] <- fill	
	all.rows <- c(all.rows, rownames(WF[[i]]))
    }

    rownames(WF[[nbar]]) <- seq(max(as.numeric(rownames(WF[[nbar-1]]))), 
                                len = nrow(WF[[nbar]]),
                                by = 1)
    WF[[nbar]] [WF[[nbar]] == " "] <- fill

    # collate all rows
    all.rows <- unique(all.rows)
    all.rows <- all.rows[order(as.numeric(all.rows))]
    
    # set up final chart
    CHART <- matrix(" ", nrow = length(all.rows),
		    ncol = (bar.width + gap) * nbar + 1)
    CHART[,  (bar.width + gap) * nbar + 1] <- "\n"
    rownames(CHART) <- all.rows

    # x positions of all bars    
    x0 <- (0 : (nbar-1)) * (bar.width + gap) + 1 
    x1 <- x0 + bar.width - 1

    for (i in 1:nbar) {
	CHART[rownames(WF[[i]]), x0[i] : x1[i]]  <- WF[[i]]
    }

    # make every line of description length of bar width
    # abbreviate words if too long
    # combine words if short
    # then strsplit them to fill one letter into one matrix cell
    texts <- strsplit(steps, split = " ")
    texts <- lapply(texts, abbreviate, min = bar.width, dot = FALSE)
    texts <- lapply(texts, cat.prev, bar.width)
    texts <- lapply(texts, format, width = bar.width + gap)

    # number of rows of description at bottom of waterfall chart
    textrow <- max(unlist(lapply(texts, length)))
    for(i in 1:nbar) {
	while(length(texts[[i]]) < textrow)
	    texts[[i]] <- c(texts[[i]], format("", width = bar.width + gap))
    }

    # set up description matrix
    TEXT <- matrix(unlist(strsplit(do.call(rbind, texts), split = "")), 
		   nrow = textrow, byrow = TRUE)
    TEXT  <- cbind(TEXT, "\n")

    CHART <- rbind(CHART, TEXT)

    # 'e' numbers need sign reverted
    numbers[dt$tag == 1]  <- -numbers[dt$tag == 1]
    # format numbers so that they are on top & centred
    NO <- format(format(as.character(numbers), width = bar.width, 
			justify = "centre"), 
			width = bar.width + gap)
    NO <- matrix(unlist(strsplit(NO, split = "")), nrow = 1)
    NO <- cbind(NO, "\n")

    # bind numbers to chart
    CHART <- rbind(NO, CHART)

    # print waterfall chart (by col)
    cat(t(CHART), sep = "")

}



wf <- data.frame(step = c("Catchment Universe",
			  "Unsuitable Demographics", 
			  "Tested",
			  "Unfavourable Competition",
			  "Cannibalisation Too High", 
			  "Final White Space"),
		 count = c(2889, -1350, "e", -490, -217, "e"))
print.waterfall(wf, bar.width = 8)

wf2 <- data.frame(step = c("2015 Revenue", 
			   "Gain from Cheaper Rent", 
			   "Loss to Competiton", 
			   "2016 Revenue", 
			   "Loss to Competition", 
			   "2017 Revenue"),
		  count = c(2500, 630, -340, "e", -490, "e"))

