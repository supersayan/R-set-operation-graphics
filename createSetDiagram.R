library(ggplot2)
library(ggforce)

library(extrafont)
font_import(paths="./fonts", prompt=F)
loadfonts(device="win")

createSetDiagram <- function(setName, elements) {
  d <- ggplot() +
    theme_void() + coord_fixed() +
    theme(text=element_text(family="Gill Sans")) +
    geom_rrect(aes(xmin=0, xmax=30, ymin=0, ymax=20), fill="blue", color="NA", size=0, alpha=0.4,
               radius=unit(12, "pt")) +
    geom_circle(aes(x0 = 15, y0 = 10, r=6), fill="red", color="NA", alpha=0.4, size=0) +
    geom_text(aes(x=29, y=21, label='Omega', family="Gill Sans"), size=8, parse = TRUE) +
    geom_text(aes(x=19, y=16, label=setName), size=8, family="Gill Sans")
    # geom_text_wordcloud(mapping = aes(x=15, y=10, label=a, family="Gill Sans"), size=6, rstep=.01,
    #                     eccentricity=1, grid_size = 50)
  if (length(elements) == 1) {
    d <- d + geom_text(aes(x=14, y=11, label=elements[1]), size=8, family="Gill Sans") +
            geom_point(aes(x=15, y=10), size=4)
  } else if (length(elements) == 2) {
    d <- d + geom_text(aes(x=13, y=12, label=elements[1]), size=8, family="Gill Sans") +
          geom_text(aes(x=17, y=8, label=elements[2]), size=8, family="Gill Sans") +
          geom_point(aes(x=14, y=11), size=4) +
          geom_point(aes(x=16, y=9), size=4)
  } else if (length(elements) == 3) {
    d <- d + geom_text(aes(x=14, y=12, label=elements[1]), size=8, family="Gill Sans") +
          geom_text(aes(x=13, y=8, label=elements[2]), size=8, family="Gill Sans") +
          geom_text(aes(x=17, y=8, label=elements[3]), size=8, family="Gill Sans") +
          geom_point(aes(x=15, y=11), size=4) +
          geom_point(aes(x=14, y=9), size=4) +
          geom_point(aes(x=16, y=9), size=4)
  } else if (length(elements) == 4) {
    d <- d + geom_text(aes(x=13, y=12, label=elements[1]), size=8, family="Gill Sans") +
          geom_text(aes(x=17, y=12, label=elements[2]), size=8, family="Gill Sans") +
          geom_text(aes(x=13, y=8, label=elements[3]), size=8, family="Gill Sans") +
          geom_text(aes(x=17, y=8, label=elements[4]), size=8, family="Gill Sans") +
          geom_point(aes(x=14, y=11), size=4) +
          geom_point(aes(x=16, y=11), size=4) +
          geom_point(aes(x=14, y=9), size=4) +
          geom_point(aes(x=16, y=9), size=4)
  } else {
    d <- d + geom_text(aes(x=15, y=13.5, label=elements[1]), size=8, family="Gill Sans") +
          geom_text(aes(x=12, y=11, label=elements[2]), size=8, family="Gill Sans") +
          geom_text(aes(x=18, y=11, label=elements[3]), size=8, family="Gill Sans") +
          geom_text(aes(x=13, y=7.5, label=elements[4]), size=8, family="Gill Sans") +
          geom_text(aes(x=17, y=7.5, label=elements[5]), size=8, family="Gill Sans") +
          geom_point(aes(x=15, y=12), size=4) +
          geom_point(aes(x=13, y=10.5), size=4) +
          geom_point(aes(x=17, y=10.5), size=4) +
          geom_point(aes(x=14, y=8.5), size=4) +
          geom_point(aes(x=16, y=8.5), size=4)
  }
  
  d
}

createSetDiagram('A', list('1'))
createSetDiagram('B', list('1', '2'))
createSetDiagram('C', list('1', '2', '3'))
createSetDiagram('D', list('1', '2', '3', '4'))
createSetDiagram('E', list('a', 'b', 'c', '4', '5'))

createSetDiagram('G', list('a'))
