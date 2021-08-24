##### INTRO A SOCIAL NETWORK USANDO POLITICXSENTWITTER

### Workshop enmarcado en [Rladies BA](https://twitter.com/RLadiesBA) 

# 1. Cargamos las librerías ----------------------------------------------

library(devtools)
install_github("guadag12/politicxsentwitteR")
library(politicxsentwitteR)
library(tidyverse)
library(igraph)
library(visNetwork)
library(viridis)

# 2. ¿Cómo funciona politicxsentwitteR? ----------------------------------

# 2.a. Podemos descargar para un funcionario en particular:
data_vizzotti <- get_timeline_data(screen.name = "carlavizzotti")

# 2.b. Podemos descargar la cantidad de seguidores / seguidos que tiene un usuario:
data_fr_fol <- get_friends_followers(screen.name = c("alferdez", "horaciorlarreta"))
library(ggpp)
install.packages("ggpp")
data_fr_fol %>% 
  ggplot() + 
  geom_line(aes(x=as.Date(date), y=as.numeric(followers_count), color=screen_name)) + theme_bw() +
  scale_color_manual(values = c("#00459e", "#ba9200")) +
  labs( x = "Fecha", y = "Cantidad", title = "Evolución de followers de @alferdez y @horaciorlarreta") +
  facet_wrap(~screen_name, scale="free_y") +
  geom_vline(xintercept = as.Date("2021-04-14"),
           color = "black") +
  geom_text_npc(data = data.frame(x = c("center"),
                                  y = c("top"),
                                  label = c("14/04   -   Cierre escuelas 🎓")),
                mapping = aes(npcx = x, npcy = y, label = label),
                size = 3)

 
# 2.c. Podemos descargar las conexiones que tuvieron entre ellos 
network_diputadxs <- get_network_data(category = "deputies", start_date = "2021-06-01", end_date = "2021-07-31")
#write.csv(network_diputadxs, file = "network_diputadxs.csv", row.names = F)
network_diputadxs <- read.csv("network_diputadxs.csv") # ACA CARGAR LINK DE GITHUB

# 3. Social Network -----------------------------------------------------------

# 3.a. Limpieza de datos previa

# 3.b. Armado de nube

# 3.c. Graficamos la nube!

# 3.d. Exploremos un poco la data

# 4. Customizacion --------------------------------------------------------

# 4.a. 


# 5. Degree ---------------------------------------------------------------


# 6. Layouts --------------------------------------------------------------


# 7. Comunidades ----------------------------------------------------------


# 8. Agreguemos Interactividad! -------------------------------------------






