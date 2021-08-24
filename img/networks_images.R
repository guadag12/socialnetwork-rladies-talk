nodes <- data.frame(id = c("A", "B", "C", "D", "E", "F"),
                    color = c("#EB15EB", "#EB15EB", "#562457", "#EB15EB", "#562457", "#562457"), 
                    size = c(70, 70, 70, 70, 70, 70))
edges <- data.frame(source = c("A", "B", "B", "C", "D", "D","E"), 
                    target = c("B", "C", "D", "D", "F", "A", "E"))
g <- graph_from_data_frame(edges, directed=TRUE, vertices=nodes)

set.seed(1000)
#nodos
plot(g, edge.color = "white", vertex.label = NA, 
     margin = 2, asp = 0.999, ylim=c(0,0.002),xlim=c(-4,3), 
     layout = layout_in_circle(g))
#directed + aristas
plot(g,  vertex.label = NA, margin = 2, asp = 1.5, ylim=c(0,0.002),xlim=c(-2,3), 
     edge.arrow.size = 0.5, edge.color = "black",
     layout = layout_in_circle(g))
#undirected
plot(g,  vertex.label = NA, margin = 2, asp = 1.5, ylim=c(0,0.002),xlim=c(-2,3), 
     edge.arrow.size = 0, edge.color = "black", 
     layout = layout_in_circle(g))
plot(g, margin = 1, asp = 0.999, ylim=c(0,1),xlim=c(-2,3))
#degree
plot(g,  vertex.label.color = "white", margin = 2, asp = 1.5, ylim=c(0,0.002),xlim=c(-2,3), 
     edge.arrow.size = 0.5, edge.color = "black", 
     layout = layout_in_circle(g))
#layouts
plot(g,  vertex.label.color = "white", margin = 2, asp = 1.5, ylim=c(0,0.002),xlim=c(-2,3), 
     edge.arrow.size = 0.5, edge.color = "black", 
     layout = layout_randomly(g))
plot(g,  vertex.label.color = "white", margin = 2, asp = 1.5, ylim=c(0,0.002),xlim=c(-2,3), 
     edge.arrow.size = 0.5, edge.color = "black", 
     layout = layout.fruchterman.reingold(g))

plot(g,  vertex.label.color = "white", margin = 2, asp = 1.5, ylim=c(0,0.002),xlim=c(-2,3), 
     edge.arrow.size = 0.5, edge.color = "black", 
     layout = layout_on_grid(g))

plot(g,  vertex.label.color = "white", margin = 2, asp = 1.5, ylim=c(0,0.002),xlim=c(-2,3), 
     edge.arrow.size = 0.5, edge.color = "black", 
     layout = layout_in_circle(g))
