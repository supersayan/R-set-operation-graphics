library(venn)
library(QCA)

venn("A~B~C + AB~C + A~BC + ABC") +
  text(300, 300, label = "dog")

data(CVF)
obj <- truthTable(CVF, "SURV", incl.cut = 0.85)

venn(obj)

pCVF <- minimize(obj, include = "?")
venn(pCVF$solution[[1]], zcol = "#ffdd77, #bb2020, #1188cc")
cases <- paste(c("HungariansRom", "CatholicsNIreland", "AlbaniansFYROM",
                 "RussiansEstonia"), collapse = "\n")
coords <- unlist(getCentroid(getZones(pCVF$solution[[1]][2])))
text(coords[1], coords[2], labels = cases, cex = 0.85)

library(ggforce)

ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = pi / 2)) +
  coord_fixed() +
  geom_ellipse(aes(x0 = 10, y0 = 0, a = 10, b = 3, angle = pi / 2)) +
  geom_text(x = 5, y = 0, label = "1") +
  coord_fixed() + theme_void()

df.venn <- data.frame(x = c(-0.5, 0.5),
                      y = c(0, 0),
                      labels = c("A", "B"),
                      stringsAsFactors = FALSE)

yvals <- seq(-sqrt(2), sqrt(2), 0.01)
xvals <- sqrt(2.25 - yvals^2) - 0.5
yvals <- c(yvals, rev(yvals))
xvals <- c(xvals, -xvals)
combo <- data.frame(x = xvals, y = yvals)

ggplot2::ggplot(data = df.venn) +
  ggforce::geom_circle(
    ggplot2::aes_string(x0 = "x", y0 = "y", r = 1.5, fill = "labels"),
    alpha = 0.3,
    size = 1,
    colour = 'lightgray'
  ) +
  ggplot2::geom_polygon(data = combo, aes(x = x, y = y), fill = "darkgrey") +
  ggplot2::coord_fixed() +
  ggplot2::theme_void() +
  ggplot2::scale_fill_manual(values = c("gray80", "gray80")) +
  ggplot2::geom_text(x = -1.5, y = 0, label = "1") +
  ggplot2::geom_text(x = 1.5, y = 0, label = "2")

