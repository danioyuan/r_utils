useful_colors <- c(
  "#3D5FD1", # blue: 61 95 209, 3D5FD1
  "#54A8FB", # lightblue: 83 168 251
  "#D93A00", # RED: 217 58 0, D93A00
  "#FC9C00", # ORANGE: 252 156 0, FC9C00
  "#149800", # GREEN: 20 152 0, 149800
  "#98009D", # PURPLE: 152 0 157, 98009D
  "#2197C9"  # TEAL: 33 151 201, 2197C9
  )

# Barplot with both stacked and beside, both 45 and -45 degree shades, two legends
# datColors <- c(
#   "#FC9C00", # blue: 61 95 209, 3D5FD1
#   "#149800", # GREEN: 20 152 0, 149800
#   "#54A8FB",  # TEAL: 33 151 201, 2197C9
#   "#D93A00" # PURPLE: 152 0 157, 98009D
# )
# stages <- c("seed", "early", "expansion", "later")
# dat1 <- dat
# dat1[, 1:n * 2] <- NA
# pos <- barplot(dat1, space = c(0.75, 0.25), col = datColors, ylim = c(0, max(colSums(dat))))
# dat2 <- dat
# dat2[, 1:n * 2 - 1] <- NA
# barplot(dat2, space = c(0.75, 0.25), density = 30, col = datColors, add = T)
# barplot(dat2, space = c(0.75, 0.25), density = 30, angle = -45, col = datColors, add = T)
# text((pos[1:n * 2 - 1] + pos[1:n * 2])/2, par("usr")[2], labels = paste("'", substr(years, 3, 4), sep = ""), pos = 1, xpd = TRUE)
# legend("topright", rev(stages), fill = rev(datColors), bty = "n")
# legend("top", c("DJ", "TR"), fill = c("black", NA), bty = "n", horiz = T)
# legend("top", c("DJ", "TR"), fill = "black", density = 30, bty = "n", horiz = T)
# legend("top", c("DJ", "TR"), fill = "black", density = 30, angle = -45, bty = "n", horiz = T)

Table2Barplot <- function(tbl, ...) {
  loc <- barplot(tbl, xaxt = "n", ...)
  labels <- names(tbl)
#  axis(1, at = loc, labels = FALSE)
  text(loc, par("usr")[3] - 0.2, labels = labels, srt = 45, pos = 1, cex = .8, xpd = TRUE)
}

wordWrap<-function(x, split, len) paste(strwrap(gsub(split, " ", x), width = len), collapse = "\n")

TimelineScatterPlot <- function(timestamp, y, amount = 0.2, ...) {
  y <- factor(as.character(y))
  plot(timestamp, jitter(as.numeric(y), amount = amount), pch = ".",
       ylim = c(0.5, length(levels(y)) + 0.5),
       xaxt = "n", yaxt = "n", type = "n", ...)

  axis(2, at = 1:length(levels(y)), labels = FALSE)
  labels <- sapply(levels(y), wordWrap, split = "[/_\\]", len = 10)

  text(par("usr")[1] - 10000, 1:length(levels(y)), labels = labels, pos = 2, xpd = TRUE)
  ycounts <- table(y)
  labels <- sprintf("%.2f%%", ycounts/sum(ycounts) * 100)
  text(par("usr")[1], 1:length(levels(y)), labels = labels, pos = 4, xpd = TRUE)

  tsDate <- as.Date(timestamp)
  dates <- seq(min(tsDate), max(tsDate) + 1, by = 1)
  if (length(dates) > 60) {
    dates <- dates[format(dates, "%d") == "01"]
    locs <- as.POSIXct(as.character(dates))
    axis(1, at = locs, labels = FALSE)
    labels <- format(dates, "%m/%Y")
    abline(v = locs, col = "lightgrey", lty = 2)
  } else {
    locs <- as.POSIXct(as.character(dates))
    axis(1, at = locs, labels = FALSE)
    locs <- locs[1:(length(locs) - 1)]
    dates <- dates[1:(length(dates) - 1)]
    labels <- paste(format(dates, "%m-%d"), substring(weekdays(dates), 1, 1), sep = "")
  }
  srt <- if (length(labels) > 15) 45 else 0
  text(locs + 3600 * 12, par("usr")[3] - 0.02 * length(levels(y)), labels = labels, srt = srt, pos = 1, xpd = TRUE)

  dates <- as.Date(format(timestamp, "%Y-%m-%d"))
  data <- data.frame(Source = y, Date = dates)
  data$Date <- factor(as.character(data$Date), levels = as.character(seq(min(dates), max(dates), by = 1)))
  counts <- table(data)
  dates <- as.POSIXct(colnames(counts))
  counts[counts == 0] <- NA
  for (i in 1:nrow(counts)) {
    # make the NAs around positive values be zero
    idx <- which(!is.na(counts[i, ]))
    idx <- intersect(union(idx + 1, idx - 1), which(is.na(counts[i, ])))
    counts[i, idx] <- 0
    lines(dates + 3600 * 12, counts[i, ]/max(counts[i, ], na.rm = TRUE) * 2 * amount + i + amount, col = "red")
  }
  points(timestamp, jitter(as.numeric(y), amount = amount), pch = ".")
  invisible(counts)
}

CalcFailureRate <- function(t0, t1) {
  allDates <- union(date0, date1)
  allCols <- union(colnames(t0), colnames(t1))
  allRows <- union(rownames(t0), rownames(t1))
  s0 <- matrix(0, nrow = length(allRows), ncol = length(allCols))
  colnames(s0) <- allCols
  rownames(s0) <- allRows
  s1 <- s0
  s0[allRows %in% rownames(t0), allCols %in% colnames(t0)] <- t0
  s1[allRows %in% rownames(t1), allCols %in% colnames(t1)] <- t1
  s0[is.na(s0)] <- 0
  s1[is.na(s1)] <- 0

  return(s0 / (s0 + s1))
}

TimelineScatterPlot2 <- function(timestamp, y, success, amount = 0.2, ...) {
  y <- factor(as.character(y))
  plot(timestamp, jitter(as.numeric(y), amount = amount), pch = ".",
       ylim = c(0.5, length(levels(y)) + 0.5),
       xaxt = "n", yaxt = "n", type = "n", ...)

  axis(2, at = 1:length(levels(y)), labels = FALSE)
  labels <- sapply(levels(y), wordWrap, split = "[/_\\]", len = 10)

  text(par("usr")[1] - 10000, 1:length(levels(y)), labels = labels, pos = 2, xpd = TRUE)
  ycounts <- table(y)
  labels <- sprintf("%.2f%%", ycounts/sum(ycounts) * 100)
  text(par("usr")[1], 1:length(levels(y)), labels = labels, pos = 4, xpd = TRUE)

  tsDate <- as.Date(timestamp, origin = "1970-01-01")
  dates <- seq(min(tsDate), max(tsDate) + 1, by = 1)
  if (length(dates) > 60) {
    dates <- dates[format(dates, "%d") == "01"]
    locs <- as.POSIXct(as.character(dates))
    axis(1, at = locs, labels = FALSE)
    labels <- format(dates, "%m/%Y")
    abline(v = locs, col = "lightgrey", lty = 2)
  } else {
    locs <- as.POSIXct(as.character(dates))
    axis(1, at = locs, labels = FALSE)
    locs <- locs[1:(length(locs) - 1)]
    dates <- dates[1:(length(dates) - 1)]
    labels <- paste(format(dates, "%m-%d"), substring(weekdays(dates), 1, 1), sep = "")
  }
  srt <- if (length(labels) > 15) 45 else 0
  text(locs + 3600 * 12, par("usr")[3] - 0.02 * length(levels(y)), labels = labels, srt = srt, pos = 1, xpd = TRUE)

  dates <- as.Date(format(timestamp, "%Y-%m-%d"))
  data <- data.frame(Source = y, Date = dates)
  data$Date <- factor(as.character(data$Date), levels = as.character(seq(min(dates), max(dates), by = 1)))
  count1 <- table(data[success == 1, ])
  count0 <- table(data[success == 0, ])
  counts <- CalcFailureRate(count0, count1)

  dates <- as.POSIXct(colnames(counts))
  counts[counts == 0] <- NA
  for (i in 1:nrow(counts)) {
    # make the NAs around positive values be zero
    idx <- which(!is.na(counts[i, ]))
    idx <- intersect(union(idx + 1, idx - 1), which(is.na(counts[i, ])))
    counts[i, idx] <- 0
    lines(dates + 3600 * 12, counts[i, ] * 0.5 + i, col = "red")
    abline(h = c(i, 0.5 + i), col = "lightgrey", lty = 2)
  }
  points(timestamp[success == 1], jitter(as.numeric(y[success == 1]) - amount, amount = amount / 2), pch = 20)
  points(timestamp[success == 0], jitter(as.numeric(y[success == 0]) - 3 * amount, amount = amount / 2), pch = 20, col = "red")
  invisible(counts)
}

TimelineTableLines <- function(yTable, amount = 0.2, ...) {
  y <- rownames(yTable)
  y <- factor(as.character(y))
  timestamp <- as.POSIXct(colnames(yTable))
  plot(rep(timestamp, each = length(y)), jitter(rep(as.numeric(y), length(timestamp)), amount = amount), pch = ".",
       ylim = c(0.5, length(levels(y)) + 0.5),
       xaxt = "n", yaxt = "n", type = "n", ...)

  axis(2, at = 1:length(levels(y)), labels = FALSE)
  labels <- sapply(levels(y), wordWrap, split = "[/_\\]", len = 10)
  text(par("usr")[1] - 10000, 1:length(levels(y)), labels = labels, pos = 2, xpd = TRUE)

  tsDate <- as.Date(timestamp)
  dates <- seq(min(tsDate), max(tsDate) + 1, by = 1)
  if (length(dates) > 60) {
    dates <- dates[format(dates, "%d") == "01"]
    locs <- as.POSIXct(as.character(dates))
    axis(1, at = locs, labels = FALSE)
    labels <- format(dates, "%m/%Y")
    abline(v = locs, col = "lightgrey", lty = 2)
  } else {
    locs <- as.POSIXct(as.character(dates))
    axis(1, at = locs, labels = FALSE)
    locs <- locs[1:(length(locs) - 1)]
    dates <- dates[1:(length(dates) - 1)]
    labels <- paste(format(dates, "%m-%d"), substring(weekdays(dates), 1, 1), sep = "")
  }
  srt <- if (length(labels) > 15) 45 else 0
  text(locs + 3600 * 12, par("usr")[3] - 0.02 * length(levels(y)), labels = labels, srt = srt, pos = 1, xpd = TRUE)

  dates <- as.Date(format(timestamp, "%Y-%m-%d"))
  counts <- yTable
  dates <- as.POSIXct(colnames(counts))
  counts[counts == 0] <- NA
  for (i in 1:nrow(counts)) {
    # make the NAs around positive values be zero
    idx <- which(!is.na(counts[i, ]))
    idx <- intersect(union(idx + 1, idx - 1), which(is.na(counts[i, ])))
    counts[i, idx] <- 0
    lines(dates + 3600 * 12, counts[i, ] * 3 * amount + i, col = "red")
    abline(h = c(i, 3 * amount + i), col = "lightgrey", lty = 2)
  }
  invisible()
}

rollmean.new <- function(x, k, wt = NULL) {
  if (is.null(wt)) {
  	wt <- rep(1, length(x))
  }
  x <- c(rep(NA, k / 2), x, rep(NA, k / 2))
  wt <- c(rep(NA, k / 2), wt, rep(NA, k / 2))
  xm <- NULL
  for (i in 1:(length(x) - k + 1)) {
    xm <- c(xm, weighted.mean(x[i:(i + k - 1)], w = wt[i:(i + k - 1)], na.rm = TRUE))
  }
  return(xm)
}

smooth.new <- function(x, y, width = 11, wt = NULL) {
  idx <- order(x)
  x <- x[idx]
  y <- y[idx]
  xm <- rollmean.new(x, k = width)
  ym <- rollmean.new(y, k = width, wt = wt)
#   sm <- list(x = xm, y = ym)
  sm <- smooth.spline(x, ym, df = 6)
  return(list(x = sm$x, y = sm$y))
}

ClassPlot <- function(x, y, wt = NULL, y0 = NULL, FUN = NULL, file = NULL, main = "", jm = 1, width = 800, height = 800, add_density = F, ...) {
  idx <- which(!is.na(x) & !is.na(y))
  x <- x[idx]
  y <- y[idx]
  wt <- wt[idx]
  sm <- smooth.new(x, y, width = 11, wt = wt)
  ymin <- min(y, na.rm = T)
  ymax <- max(y, na.rm = T)
  if (add_density) {
    sm1 <- density(x[y == ymin])
    sm2 <- density(x[y == ymax])
  }
  #   sm2 <- smooth.new(x[y != min(y, na.rm = T)], y[y != min(y, na.rm = T)], width = 11)
  if (is.null(y0)) {
    y <- jitter(y, jm)
  } else {
    y[y == y0] <- jitter(y[y == y0], jm)
  }
  
  if (!is.null(file)) {
    if (substring(file, nchar(file) - 3, nchar(file)) == ".pdf") {
      pdf(file)
    } else {
      png(file, width, height)
    }
  }
  if (is.null(FUN)) {
    plot(x, y, main = main, ...)
    lines(sm$x, sm$y, col = "red")
    if (add_density) {
      lines(sm1$x, (sm1$y - min(sm1$y)) / (max(sm1$y)- min(sm1$y)) + ymin, col = 'grey')
      lines(sm2$x, - (sm2$y - min(sm2$y)) / (max(sm2$y)- min(sm2$y)) + ymax, col = 'grey')
    }
  } else {
    plot(x, FUN(y), main = main, ...)
    lines(sm$x, FUN(sm$y), col = "red")
    if (add_density) {
      lines(sm1$x, FUN((sm1$y - min(sm1$y)) / (max(sm1$y)- min(sm1$y)) + ymin), col = 'grey')
      lines(sm2$x, - FUN((sm2$y - min(sm2$y)) / (max(sm2$y)- min(sm2$y)) + ymax), col = 'grey')
    }
  }
  
  if (!is.null(file)) {
    dev.off()
  }
}

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

PlotPredictorEffect <- function(effect, maxY = NULL) {
  y <- rowMeans(effect$y, na.rm = TRUE)
  if (is.numeric(effect$x)) {
    # line chart
    deciles <- quantile(effect$x, seq(0, 1, .1))
    z <- median(effect$mean.effect, na.rm = TRUE)
    if (is.null(maxY)) {
      plot(effect$x, y, type = "l", main = effect$name, xlab = "", ylab = "importance", ylim = c(0, maxY),
           lwd = 2, col = "darkblue")
    } else {
      plot(effect$x, y, type = "l", main = effect$name, xlab = "", ylab = "importance",
           lwd = 2, col = "darkblue")
    }
    lines(effect$x, effect$y_05, lty = 2, col = "darkblue")
    lines(effect$x, effect$y_95, lty = 2, col = "darkblue")
    points(deciles, rep(0, 11), col = "grey", pch = "|")
  } else {
    # bar chart
    if (is.null(maxY)) {
      mp <- barplot(y, names.arg=effect$x, ylim=c(0, maxY), main = effect$name, xlab="", ylab="importance")
    } else {
      mp <- barplot(y, names.arg=effect$x, main = effect$name, xlab="", ylab="importance")
    }
    error.bar(mp, y, effect$y_95 - y, y - effect$y_05)
  }
}

PlotValByDateStage <- function(udb, valCol, stage, uselog = TRUE, ...) {
  date <- udb$financing_round_date[udb$financing_round_investment_stage == stage]
  val <- udb[udb$financing_round_investment_stage == stage, valCol]
  ylab <- valCol
  if (uselog) {
    val <- log10(1 + val)
    ylab <- paste("log10(1 + ", valCol, ")", sep = "")
  }
  idx <- which(!is.na(date) & !is.na(val) & is.finite(date) & is.finite(val))
  plot(date[idx], val[idx], xlab = "date", ylab = ylab,
       main = paste("Stage =", stage), ...)
  sm <- lowess(date[idx], val[idx])
  lines(sm$x, sm$y, col = "red")
}

MakeCdfPlot <- function(x, line_pos = NULL, line_label = NULL, ...) {
  x.ecdf <- ecdf(x)
  if (is.null(line_pos)) {
    plot(x.ecdf, ...)
  } else {
    plot(x.ecdf, xaxt = "n", ...)
    rng <- par("usr")
    if (par('xlog')) {
      rng[1] <- max(rng[1], 1e-10)
    }
    axis(1, line_pos, line_label)
    line_colors <- rainbow(length(line_pos))
    for (i in 1:length(line_pos)) {
      lines(rep(line_pos[i], 2), c(0, x.ecdf(line_pos[i])), lty = 2, col = line_colors[i])
      lines(c(rng[1], line_pos[i]), rep(x.ecdf(line_pos[i]), 2), lty = 2, col = line_colors[i])
    }
  }
}

ThresholdAccuracyPlot <- function(score, y, border = 10, vline = NULL, ...) {
  # Plot two curves, one for accuracy for those with score above threshold,
  # and one for accuracy for those with score below threshold.
  # y has to be 1 or 0
  
  n <- length(y)
  m <- sum(y)
  idx <- order(score)
  score <- score[idx]
  y <- y[idx]
  idx <- which(diff(score) > 0)
  idx <- idx[which(idx >= border & idx <= n - border)]
  corrects <- cumsum(y)
  val1 <- corrects[idx] / idx
  val2 <- (m - corrects[idx]) / (n - idx)
  cols <- c("#D93A00", "#3D5FD1")
  plot(score[idx], val1, type = 'l', col = cols[1], ylim = range(val1, val2), ...)
  lines(score[idx], val2, col = cols[2])
  xx <- score[idx[1]] + 0.025 * diff(range(score[idx]))
  if (!is.null(vline)) {
    for (x in vline) {
      i <- which(score[idx] > x)[1]
      x <- score[idx[i]]
      y1 <- val1[i]
      y2 <- val2[i]
      lines(c(x, x), c(-1, max(y1, y2)), lty = 2, col = 'grey')
      lines(c(0, x), c(y1, y1), lty = 2, col = cols[1])
      lines(c(0, x), c(y2, y2), lty = 2, col = cols[2])
      text(xx, y1, pos = 1, round(y1, 3), col = cols[1])
      text(xx, y2, pos = 3, round(y2, 3), col = cols[2])
    }
  }
  legend("topleft", c("> threshold", "<= threshold"),
         lty = 1, col = rev(cols), bty = "n")
}

TwoPlotExample <- function()
{
  oldpar <- par(las=1, mar=c(4,4,2,4), oma=c(2.5,0.5,1.5,0.5)) 
  bb <- barplot(tt, xlab = "# shared folders", ylab = "# Trial teams", yaxt = "n")
  lines(bb[, 1], par("yaxp")[1] + (sapply(as.numeric(names(tt)), CalcConversion, x, t1$good) - 0.5) * par("yaxp")[2] * 2,
        col = "red")
  rr <- seq(0.5, 1, by = 0.1)
  axis(2, cex.axis = 0.8)
  axis(4, par("yaxp")[1] + (rr - 0.5) * par("yaxp")[2] * 2,
       paste(rr * 100, '%', sep = ""), cex.axis = 0.8,
       col = "red", col.axis = "red")
  mtext('Conversion rate', side=4, line=3, las=0, cex=1, col = "red") 
  par(oldpar)
}

# Barplot with error bars
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
# barx <- barplot(data)
# error.bar(barx, y, upper)

# legend on top of plot
# par(xpd = T) # so legend can by outside of plotting region
# legend(x, par("usr")[4] + 1, c("invest/raised > 0.5", "invest/raised <= 0.5"), col = c("red", "blue"), pch=20, pt.cex=c(0.3, 0.8), bty="n", horiz=T)

# point size in legend
# pt.cex


# generate color palette green to red
# showpanel <- function(Colors)
# {
#   image(matrix(1:length(Colors), ncol=1), col=Colors, xaxt="n", yaxt="n" )
# }
# 
# oldpar <- par(mfrow=c(4,2))
# 
# # Method 1 (colorRampPalette was new in R 2.1.0) in grDevices
# # Same as function with same name in dichromat package?
# showpanel(colorRampPalette( c("green", "black", "red"), space="rgb")(32))
# showpanel(colorRampPalette( c("green", "black", "red"), space="rgb")(64))
# 
# # Method 2
# library(gplots)
# showpanel(greenred(32))
# showpanel(redgreen(64))
# 
# # Method 3
# library(geneplotter)
# showpanel(greenred.colors(32))
# showpanel(greenred.colors(64))
# 
# # Method 4
# library("marray")
# pal <- maPalette(low="green", high="red",mid="black")
# maColorBar(seq(-2,2, 0.2), col=pal, horizontal=TRUE, k=0)
# maColorBar(seq(-2,2, 0.1), col=pal, horizontal=TRUE, k=0)
# 

# Two Y-axis, text angle
# m <- c(1166, 1534, 1777, 2982, 4226, 2010, 1324, 1012, 1069, 1042, 1197, 1229, 1020, 805, 812, 710, 542, 366, 233, 108, 13)
# n <- c(2138, 3057, 3560, 5842, 8097, 4574, 3438, 3078, 3356, 3594, 3971, 4401, 4406, 3715, 4153, 4424, 4426, 4720, 4773, 4677, 1217)
# bp <- barplot(rbind(m, n-m), col = c("#3D5FD1", "#149800"), xlab = "Financing round year",
#               ylab = "# financing rounds", main = "Train set v.s. Full set by year")
# lines(rep((bp[11] + bp[12])/2, 2), c(0, 6000), lty = 2, col = "#2197C9")
# text((bp[11] + bp[12])/2, 6000, "Test set", col = "#2197C9", pos = 4, cex = .6)
# lines(bp, m/n * max(n), col = "#98009D")
# lines(bp, m/n * max(n), type = "p", pch = 20, col = "#98009D")
# axis(4, c(0:6 / 10) * max(n), labels=F, col = "#98009D")
# text(par("usr")[2] + 0.1, c(0:6 / 10) * max(n), col = "#98009D",
#      labels = paste(seq(0, 60, by = 10), "%", sep = ""), pos = 4, cex = .6, xpd = TRUE) # srt = 90, 
# legend("top", c("Not in train set", "In train set"), fill = c("#149800", "#3D5FD1"), bty = "n", cex = .8)
# legend("topright", "Pct in train set", lty=1, pch=20, col = "#98009D", bty="n", cex = 0.8)
# text(bp, par("usr")[2] - 100, , labels = 1996:2016, pos = 1, cex = .6, xpd = TRUE, srt = 90)
