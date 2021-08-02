library(ggplot2)
library(ggforce)
library(gganimate)
library(transformr)

createSetAnimation <- function(set1Name, set1Elements, set2Name, set2Elements) {
  df <- read.table(text = "set x y grp
                          A 10  10 1
                          A 12  10 2
                          A 14  10 3
                          B 20  10 1
                          B 18  10 2
                          B 16  10 3", header = TRUE)
  
  d <- ggplot(data = df, aes(x0=x, y0=y, group=grp)) +
    theme_void() + coord_fixed() +
    theme(text=element_text(family="Gill Sans")) +
    geom_circle(aes(r=6), fill="red", color="NA", alpha=0.4, size=0) +
    geom_rrect(aes(xmin=0, xmax=30, ymin=0, ymax=20), fill="blue", color="NA", size=0, alpha=0.4,
               radius=unit(12, "pt")) +
    geom_text(aes(x=29, y=21, label='Omega', family="Gill Sans"), size=8, parse = TRUE) +
    geom_text(aes(x=19, y=16, label=set1Name), size=8, family="Gill Sans") +
    transition_states(set)
  
  d <- d + geom_text(aes(x=13, y=12, label=set1Elements[1]), size=8, family="Gill Sans") +
      geom_text(aes(x=17, y=8, label=set1Elements[2]), size=8, family="Gill Sans") +
      geom_point(aes(x=14, y=11), size=4) +
      geom_point(aes(x=16, y=9), size=4)
  
  d
}

createSetAnimation('A', list('1', '2'), 'B', list('1', '2'))
