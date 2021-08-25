##### INTRO A SOCIAL NETWORK USANDO POLITICXSENTWITTER

### Workshop enmarcado en [Rladies BA](https://twitter.com/RLadiesBA) 

# 1. Cargamos las librerías ----------------------------------------------
options(scipen = 999)

library(devtools)
install_github("guadag12/politicxsentwitteR")
library(politicxsentwitteR)
library(tidyverse)
library(igraph)
library(visNetwork)
library(viridis)
library(ggpp)

# 2. ¿Cómo funciona politicxsentwitteR? ----------------------------------

# 2.a. Podemos descargar para un funcionario en particular:
data_vizzotti <- get_timeline_data(screen.name = "carlavizzotti")

# 2.b. Podemos descargar la cantidad de seguidores / seguidos que tiene un usuario:

data_fr_fol <- get_friends_followers(screen.name = c("alferdez", "horaciorlarreta"))

data_fr_fol %>% 
  ggplot() + 
  geom_line(aes(x=as.Date(date), y=as.numeric(followers_count), color=screen_name)) + theme_bw() +
  scale_color_manual(values = c("#00459e", "#ba9200")) +
  labs( x = "Fecha", y = "Cantidad", title = "Evolución de followers de @alferdez y @horaciorlarreta") +
  facet_wrap(~screen_name, scale="free_y") +
  geom_vline(xintercept = as.Date("2021-04-14"),
             color = "black") +
  geom_text_npc(data = data.frame(x = c("right"),
                                  y = c("top"),
                                  label = c("Cierre escuelas 🎓")),
                mapping = aes(npcx = x, npcy = y, label = label),
                size = 3)


# 2.c. Descarguemos la data para armar la nube!

## OPCION 1: Podemos descargar las conexiones que tuvieron entre ellos:

network_diputadxs <- get_network_data(category = "deputies", 
                                      start_date = "2021-06-01", 
                                      end_date = "2021-07-31")

## OPCION 2: Tambien podemos levantar la data de github:
network_diputadxs <- read.csv("https://raw.githubusercontent.com/guadag12/socialnetwork-rladies-talk/main/codigo/network_diputadxs.csv") # ACA CARGAR LINK DE GITHUB
data_usuarios <- read.csv("https://github.com/guadag12/socialnetwork-rladies-talk/raw/main/codigo/data_usuarios.csv")

# 3. Social Network -----------------------------------------------------------

# 3.a. Limpieza de datos previa

# 3.a.a. NODOS / VERTICES
network_diputadxs <- network_diputadxs %>% 
  group_by(user_id, retweet_user_id) %>% 
  summarise(retweet_count = sum(value))

nodos <- gather(data = network_diputadxs, key = "tipo", value = "id", c(1,2))
nodos$retweet_count <- as.numeric(nodos$retweet_count)
nodos <- nodos %>% 
  group_by(id) %>% 
  summarise(retweet_count = sum(retweet_count)) %>% 
  left_join(data_usuarios, by = c("id" = "user_id"))

head(nodos)

# 3.a.b. ARISTAS / EDGES

links <- network_diputadxs %>% 
  group_by(user_id, retweet_user_id) %>% 
  filter((user_id %in% nodos$id) & (retweet_user_id %in% nodos$id)) %>%
  rename(from = user_id,
         to = retweet_user_id,
         friendship = retweet_count)

# 3.b. Armado de nube
g <- graph_from_data_frame(links, directed=TRUE, vertices=nodos)

# 3.c. Graficamos la nube!
plot(g, vertex.color = NA)

# 3.d. Exploremos un poco la data

class(g) #chequiemos que esta bien el tipo de grafico

summary(g)

is.directed(g)

plot(g,  vertex.color = NA)

# 4. Customizacion --------------------------------------------------------

# 4.a. Cambiamos el color de los nodos con el parametro "vertex.color" y
# le pedimos que NO nos traiga el nombre de los vertices con "vertex.label = NA":

plot(g, vertex.label = NA, vertex.color = "red")

# 4.b.cambiamos el color de los nodos según la columna "colors" y agreguemosle la leyenda:

plot(g, vertex.label = NA)
legend(x=-1.5, y=-1.1, c("Otrxs",  "Cambiemos", "Frente de Todos", "Libertarios",
                         "Frente de Izquierda","Peronismo"), pch=21,
       col="#777777",pt.bg=unique(data_usuarios$color), pt.cex=2, cex=.8, bty="n", ncol=1)

# 4.c. Cambiar el tamaño de los nodos con "vertex.size":
plot(g, vertex.label = NA, 
     vertex.size = 5)

# 4.d. Cambiar el color de las aristas con "edge.color":
plot(g, vertex.label = NA, 
     vertex.size = 5, 
     edge.color = "#d1d1d1")


# 4.e. Cambiar el tamaño del nodo según la cantidad de seguidores:
plot(g, vertex.label = NA, 
     vertex.size = sqrt(V(g)$followers_count)/22, 
     edge.color = "#d1d1d1")

# 4.f. ¿Quiénes son los que más followers tienen?
label <- ifelse(V(g)$followers_count > quantile(V(g)$followers_count, 0.95), 
                yes = V(g)$name,
                no = NA) 

plot(g, vertex.label = label, 
     vertex.size = sqrt(V(g)$followers_count)/28,
     edge.color = "#d1d1d1")


# 4.g. Realicemos un gráfico comparando los usuarios con más 
# rtweets vs. los que más followers tuvieron

set.seed(102)
label2 <- ifelse(test = V(g)$retweet_count > quantile(V(g)$retweet_count, 0.98),
                 yes = V(g)$name, no = NA)

par(mfrow=c(1,2), mar=c(0,0,0,0))  
plot(g, vertex.label = NA, vertex.size = sqrt(V(g)$followers_count)/28, 
     edge.color = "#d1d1d1", 
     main = "Network basado en Followers")

plot(g, vertex.label = NA, vertex.size = sqrt(V(g)$retweet_count)*2, 
     edge.color = "#d1d1d1", 
     main = "Network basado en RTS")

# 5. Degree ---------------------------------------------------------------

# 5.a. Calcular el out-degree con la función "degree":
g.outd <- degree(g, mode = c("out"))

hist(g.outd, breaks = 30)

# 5.b. Calcular el in-degree (grado de entrada):
g.ind <- degree(g, mode = c("in"))

# 5.c. Cambiemos el tamaño de los nodos según el grado de entrada:
plot(g,  vertex.size = g.ind, vertex.label= NA)

# 6. Layouts --------------------------------------------------------------
# 6.a. Utilicemos el "layout_in_circle"

plot(g, vertex.size = g.ind, vertex.label = label2,
     layout = layout_in_circle(g))

# 6.b. Utilicemos el "layout.fruchterman.reingold"
plot(g, vertex.size = g.ind, vertex.label = label2, 
     layout = layout.fruchterman.reingold(g))

# 7. Comunidades ----------------------------------------------------------

wc <- walktrap.community(g)

# Se determina qué miembro corresponde a qué comunidad
users_wc <- membership(wc)

new.color<-data.frame(t(col2rgb(wc$membership)/255))
new.color<-rgb(new.color, alpha=.6)

plot(g, vertex.label=label2, layout=layout.fruchterman.reingold(g), 
     vertex.size=log(g.ind+1)*2, vertex.label.color="Black",
     vertex.color=V(g)$color, vertex.frame.color=V(g)$color, 
     edge.arrow.size=.2)

# 8. Agreguemos Interactividad! -------------------------------------------
library(visNetwork)

nodos$screen_name <- nodos$name

nodes <- nodos %>% rename(label = screen_name)
visNetwork(nodes, links, main = paste0("Interacción en Twitter de usuarios de la HCDN")) %>%
  visIgraphLayout() %>%
  visNodes(
    shape = "dot",
    shadow = list(enabled = TRUE, size = 10)
  ) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#C4C4C4", highlight = "#C4C4C4"), length = 1000
  ) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1,
                                     hover = T)) %>%
  visPhysics(enabled = F, solver = "repulsion", repulsion = list(nodeDistance = 1000)) %>%
  visInteraction(navigationButtons = TRUE)


