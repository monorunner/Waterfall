print.waterfall <- function(dt, bar.width = 9, inital.height = 16) {

    nbar <- nrow(dt)

    numbers <- dt[, 2]
    x <- numbers[1] / initial.height

    WF <- add.bar(bar.width, initial.height)






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
    BAR <- matrix(" ", ncol = width + 1, nrow = height)
    BAR[, width+1] <- "\n"
    BAR[1, 1:width] <- BAR[height, 1:width]<- add.hline(width)
    BAR[, 1] <- BAR[, width] <- add.vline(height)
    cat(t(BAR), sep = "")

}
    

 
