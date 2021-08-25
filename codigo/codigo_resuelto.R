##### INTRO A SOCIAL NETWORK USANDO POLITICXSENTWITTER

### Workshop enmarcado en [Rladies BA](https://twitter.com/RLadiesBA) 

# 1. Cargamos las librerías ----------------------------------------------

### Recordá que para descargar "POLITICXSENTWITTER" debemos primero activar
### devtools y luego iniciar la instalación.
### Una vez iniciada la instalación, si R pregunta: "Which would you like to update?"
### debemos seleccionar la opción "3".

options(scipen = 999)

library(devtools)
install_github("guadag12/politicxsentwitteR", force = TRUE)
library(politicxsentwitteR)
library(tidyverse)
library(igraph)
library(visNetwork)
library(viridis) #sólo se necesita para los gráficos de ggplot del principio, no para las nubes
library(ggpp)    #sólo se necesita para los gráficos de ggplot del principio, no para las nubes

# 2. ¿Cómo funciona politicxsentwitteR? ----------------------------------

# 2.a. Podemos descargar para un funcionario en particular con la función "get_timeline_data()" 
# dandole el screen_name de la persona que queremos descargar:

data_vizzotti <- get_timeline_data(screen.name = "carlavizzotti")

# 2.b. Podemos descargar la cantidad de seguidores / seguidos que tiene un usuario
# con la función "get_friends_followers()" indicandole el nombre de él / los usuarios :

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

# Con la función "get_network_data()" obtenemos data sobre quiénes se retweetearon
# entre si. Podemos elegir entre las siguientes categorias de usuario "'all','deputies',
# 'national executive','others','province servants', 'senators'". Y le tenemos que aclarar
# una fecha de inicio y una de finalización

network_diputadxs <- get_network_data(category = "deputies", 
                                      start_date = "2021-06-01", 
                                      end_date = "2021-07-31")

## OPCION 2: Tambien podemos levantar la data de github con "read.csv()":
network_diputadxs <- read.csv("https://raw.githubusercontent.com/guadag12/socialnetwork-rladies-talk/main/codigo/network_diputadxs.csv") 
data_usuarios <- read.csv("https://github.com/guadag12/socialnetwork-rladies-talk/raw/main/codigo/data_usuarios.csv")

# 3. Social Network -----------------------------------------------------------

# 3.a. LIMPIEZA -----------------------------------------------

## LO IMPORTANTE ES QUE NOS QUEDEN 2 DATASETS.
##  I. CON LOS NODOS (QUE ES UNA COLUMNA DE USUARIOS ÚNICOS CON UN ID Y -SI QUEREMOS- UN LABEL)
##  II. CON LAS ARISTAS (QUE ESTA COMPUESTA POR DOS COLUMNAS: FROM (DESDE) Y TO (HASTA))

# 3.a.a. NODOS / VERTICES

# Generamos todo el tratamiento para que nos quede UNA SOLA columna con el id 
# y alguna información adicional:
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

# Realizamos los filtros correspondientes para asegurarnos que todos los usuarios 
# de las aristas esten enlistados en los nodos y renombramos las columnas.

links <- network_diputadxs %>% 
  group_by(user_id, retweet_user_id) %>% 
  filter((user_id %in% nodos$id) & (retweet_user_id %in% nodos$id)) %>%
  rename(from = user_id,
         to = retweet_user_id,
         friendship = retweet_count)

# 3.b. ARMADO DE LA NUBE  -------------------------------------------------

# 3.b. Armado de nube con la función "graph_from_data_frame()":

g <- graph_from_data_frame(links, directed=TRUE, vertices=nodos)

# 3.c. Graficamos la nube con "plot()" e insertamos adentro la nube 
# formada en el punto anterior:

plot(g)

# 3.d. Exploremos un poco la data:

class(g) #chequiemos que esta bien el tipo de grafico

summary(g) #vemos alguna información adicional sobre la nube

is.directed(g) # le preguntamos si es un grafo dirigido

# 3.e. Accedamos a datos de los nodos / edges: 

V(g) # Podemos acceder a data sobre los vertices

E(g) #podemos acceder a data sobre las edges

V(g)$name # Podemos pedirle info adicional sobre los diferentes atributos de nuestro gráfico

plot(g, vertex.label = NA, vertex.color = NA) # hacemos un plot sin distinguir color y usuario

# 4. Customizacion --------------------------------------------------------

# 4.a. Cambiamos el color de los nodos con el parametro "vertex.color" y
# le pedimos que NO nos traiga el nombre de los vertices con "vertex.label = NA":

plot(g, vertex.label = NA, vertex.color = "red")



# 4.b.Cambiamos el color de los nodos según la columna "colors" y agreguemosle la leyenda:

plot(g, vertex.label = NA)
legend(x=-1.5, y=-1.1, c("Otrxs",  "Cambiemos", "Frente de Todos", "Libertarios",
                         "Frente de Izquierda","Peronismo"), pch=21,
       col="#777777", pt.bg=unique(data_usuarios$color), pt.cex=2, cex=.8, bty="n", ncol=1)

# 4.c. Cambiar el tamaño de los nodos con "vertex.size":

plot(g, vertex.label = NA, vertex.size = 5)

# 4.d. Cambiar el color de las aristas con "edge.color":

plot(g, vertex.label = NA, vertex.size = 5, edge.color = "#ffbc75")


# 4.e. Cambiar el tamaño del nodo según la cantidad de seguidores:

plot(g, vertex.label = NA,  vertex.size = sqrt(V(g)$followers_count)/22, edge.color = "#ffbc75")

# 4.f. ¿Podemos graficar quiénes tienen más followers? 

# Es necesario hacer un ifelse() y asignarlo a una variable, luego se puede graficar:

label <- ifelse(V(g)$followers_count > quantile(V(g)$followers_count, 0.95), 
                yes = V(g)$name, no = NA) 

plot(g, vertex.label = label, 
     vertex.size = sqrt(V(g)$followers_count)/28,
     edge.color = "#ffbc75")


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

par(mfrow=c(1,1), mar=c(0,0,0,0))  

# 5. Degree (grados de centralidad) ----------------------------------

# 5.a. Calcular el out-degree con la función "degree()" aclarandole el mode "out":
g.outd <- degree(g, mode = c("out"))

hist(g.outd, breaks = 30)

# 5.b. Calcular el in-degree (grado de entrada):
g.ind <- degree(g, mode = c("in"))

# 5.c. Cambiemos el tamaño de los nodos según el grado de entrada:
plot(g,  vertex.size = g.ind, vertex.label= NA)

# 6. Layouts --------------------------------------------------------------

# 6.a. Utilicemos el "layout_in_circle":

plot(g, vertex.size = g.ind, vertex.label = label2,
     layout = layout_in_circle(g))

# 6.b. Utilicemos el "layout.fruchterman.reingold"
plot(g, vertex.size = g.ind, vertex.label = label2, 
     layout = layout.fruchterman.reingold(g))

# 7. Comunidades ----------------------------------------------------------

## Generamos un nuevo objeto que detecta comunidades para nuestra nube con
## walktrap.community()
wc <- walktrap.community(g)

# Se determina qué miembro corresponde a qué comunidad
users_wc <- membership(wc)

## Generamos el gráfico

plot(wc, g, vertex.label=NA, layout=layout.fruchterman.reingold(g), 
     vertex.size=log(g.ind+1)*2, vertex.label.color="Black",
     vertex.color=V(g)$color, vertex.frame.color=V(g)$color, 
     edge.arrow.size=.2)

# 8. Agreguemos Interactividad! -------------------------------------------

library(visNetwork)

nodos$screen_name <- nodos$name
nodes <- nodos %>% rename(label = screen_name)

# Utilizamos algunas funciones y las concatenamos entre si para obtener la red
# "visNetwork()" donde le aclaramos el dataset de los nodos, las aritas y el titulo
# "visIgraphLayout()" donde le expresamos que el formato de salida es como en igraph
# "visNodes()" donde le damos caracteristica de los nodos
# "visEdges()" donde le aclaramos caracteristicas de las edges
# "visOptions()" donde le damos opciones para la customización
# "visPhysics()" donde le pedimos que la distancia de los nodos este dada por la cantidad de retweets que comparten
# "visInteraction()" donde le pedimos que sea interactivo

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
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>%
  visPhysics(enabled = F, solver = "repulsion", repulsion = list(nodeDistance = 1000)) %>%
  visInteraction(navigationButtons = TRUE)


