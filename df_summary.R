# Yuan Yuan <danioyuan@gmail.com>

PlotFactorCol <- function(x, ...) {
  xLevels <- levels(x)
  barplot(table(x), ...)
}

PlotCharacterCol <- function(x, ...) {
  x <- as.factor(x)
  PlotFactorCol(x, ...)
}

PlotNumericCol <- function(x, ...) {
  # Judge skewness of the distribution
  hist(x, 100, ...)
  box()
  rng <- par("usr")
  h <- (rng[4] - rng[3]) / 50
  pos <- quantile(x, 0:10 / 10, na.rm = TRUE)
  for (i in 0:10) {
    lines(rep(pos[i + 1], 2), c(rng[4], rng[4] - h))
  }
}

PlotIntegerCol <- function(x, ...) {
  PlotNumericCol(x, ...)
}

PlotEmpty <- function(h) {
  plot(0:h, 0:h, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
}

DrawSummaryText <- function(summaryText) {
  PlotEmpty(2)
  text(1, 1, summaryText, family = "Courier", cex = 1.5)
}

PlotColumn <- function(x, summaryText, ...) {
  DrawSummaryText(summaryText)
  colClass <- class(x)
  if (colClass == "factor") {
    PlotFactorCol(x, ...)
    PlotEmpty(2)
  } else if (colClass == "character") {
    PlotCharacterCol(x, ...)
    PlotEmpty(2)
  } else if (colClass %in% c("numeric", "integer")) {
    PlotNumericCol(x, xlab = "x", ...)
    if (min(x, na.rm = TRUE) < 0) {
      PlotNumericCol(log10(1 + x - min(x, na.rm = TRUE)), xlab = "log10(1 + x - min(x))", ...)
    } else if (min(x, na.rm = TRUE) == 0){
      PlotNumericCol(log10(1 + x), xlab = "log10(1 + x)", ...)
    } else {
      PlotNumericCol(log10(x), xlab = "log10(x)", ...)
    }
  }
}

DataframeSummaryPlot <- function(df, pdfFile) {
  n <- nrow(df)
  p <- ncol(df)
  colNames <- colnames(df)
  summaryText <- summary(df)
  summaryText[is.na(summaryText)] <- ""
  summaryText <- apply(summaryText, 2, paste, collapse = "\n")
  pdf(file = pdfFile, width = 11, height = 8.5)
  par(mfrow = c(3, 3))
  for (i in 1:p) {
    PlotColumn(df[, i], summaryText = summaryText[i], main = colNames[i])
  }
  dev.off()
}

# DataframeSummaryPlot(df, "summary.pdf")
