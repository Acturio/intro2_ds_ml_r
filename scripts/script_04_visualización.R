
pacman::p_load(
 dplyr,
 ggplot2,
 reshape2,
 DataExplorer,
 stringr,
 plotly,
 scales
)


data("diamonds")
glimpse(diamonds)

for (j in 1:10) {
 diamonds[sample(35940, 20000),j] <- NA
}

plot_intro(diamonds)
plot_missing(diamonds)

#### Gráficos univariados ####

diamonds %>% 
  ggplot( aes( x = price)) + 
  geom_histogram(color= "purple", fill= "blue", bins = 60) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = comma_format()) +
  ggtitle("Distribución de precio")


### Ejercicio: Realizar otro histograma con alguna otra variable numérica. 
### Cambiar tantos parámetros como se desee.

diamonds %>% 
  ggplot( aes( x = price)) + 
  geom_histogram(
   aes(y = ..density..), 
   color= "Blue", fill= "White", bins = 30) +
  stat_density(geom = "line", colour = "black", size = 1) +
  scale_x_continuous(labels = scales::label_dollar()) +
  scale_y_continuous(labels = scales::comma_format()) +
  ggtitle("Distribución de precio") +
  theme(plot.title = element_text(hjust = 0.5)) 


ggplot(data = diamonds, aes( x = price)) + 
  geom_boxplot(color= "Blue", fill= "lightblue") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
   title = "Distribución de precio",
   subtitle = "Boxplot",
   x = "Precio",
   caption = "Fuente: Diamonds"
   ) +
 coord_flip()

### Ejercicio: Realizar otro boxplot con alguna otra variable numérica. 
### Cambiar tantos parámetros como se desee.

diamonds %>% 
  ggplot( aes( x = carat)) + 
  geom_boxplot(color= "purple", fill= "pink", alpha= 0.3) +
  scale_x_continuous(labels = scales::comma_format()) +
  ggtitle("Distribución de peso de los diamantes") +
  theme_classic() +
  coord_flip()


diamonds %>% 
  ggplot( aes( x = carat)) + 
  geom_histogram(binwidth = 0.025, color= "purple", fill= "pink", alpha= 0.3) +
  scale_y_continuous(labels = scales::comma_format()) +
  ggtitle("Distribución de peso de los diamantes") +
  theme_bw()


#### Variables nominales / categóricas

diamonds %>% 
  ggplot( aes( x = cut)) + 
  geom_bar( color= "darkblue", fill= "#c4c8d4", alpha= 0.7) +
  scale_y_continuous(labels = scales::comma_format()) +
  ggtitle("Distribución de calidad de corte") +
  theme_dark()


df_pie <- diamonds %>%
  group_by(cut) %>% 
  summarise(
   freq = n(), 
#   media = mean(price, na.rm = T),
#  sd = sd(price, na.rm = T),
   .groups='drop'
   ) 

diamonds %>%
  group_by(cut) %>% 
  tally()


df_pie %>% 
  filter(!is.na(cut)) %>% 
  ggplot( aes( x = "", y=freq,  fill = factor(cut)))  +
  geom_bar(width = 1, stat = "identity")  +
  coord_polar(theta = "y", start=0)

diamonds %>% 
  filter(!is.na(cut)) %>% 
  ggplot() +
  geom_bar( mapping = aes(x = cut, fill = cut), show.legend = T, width = 1) +
  theme(aspect.ratio = 1) +
  labs(x= NULL, y = NULL) +
  coord_polar()

#### Ejercicio: Crear gráfico de pie con el precio medio por categoría

df_pie <- diamonds %>% 
 group_by(clarity) %>% 
 summarise(mean = mean(price, na.rm=T), .groups='drop') 

df_pie %>% 
 filter(!is.na(clarity)) %>% 
 ggplot() + 
 geom_bar(aes(x = "", y=mean, fill = clarity), 
          show.legend = T, width = 1, stat = "identity") + 
 theme(aspect.ratio = 1) + 
 labs(x= NULL, y = NULL) + 
 coord_polar("y", start =0) + 
 theme_classic() 








diamonds %>% 
 filter(!is.na(clarity)) %>% 
  ggplot( aes( y = clarity)) + 
  geom_bar( color= "darkblue", fill= "black", alpha= 0.7) +
  geom_text(aes(label = scales::comma(..count..)), stat = "count", 
            vjust = 0.5, hjust = 0, colour = "black") +
  scale_x_continuous(labels = scales::comma_format()) +
  ggtitle("Distribución claridad") +
  theme_bw() +
 coord_flip()

diamonds %>% 
 filter(!is.na(clarity)) %>% 
  ggplot( aes( y = clarity)) + 
  geom_bar( color= "darkblue", fill= "black", alpha= 0.7) +
  geom_text(aes(label = scales::percent(..count../sum(..count..) ) ), 
            stat = "count", vjust = -0.25, colour = "darkblue") +
  geom_text(aes(label = scales::comma(..count..) ), 
            stat = "count", vjust = 1, colour = "yellow") +
  scale_x_continuous(
   labels = scales::comma_format(), 
   n.breaks = 15) +
  ggtitle("Distribución claridad") +
  coord_flip()


#### Análisis multivariado ####

diamonds %>% 
 ggplot(aes(y= price, x = cut, color = cut))  + 
 #geom_point() +
 geom_jitter(size = 0.6, alpha = 0.3, height = 2) +
 geom_boxplot(color = "black", alpha = 0.5)

diamonds %>% 
  ggplot(aes(y = price, x = cut, color = cut))  + 
  geom_boxplot(size=1.2, alpha= 0.5)

diamonds %>% 
  ggplot(aes(x= price, fill = cut))  + 
  geom_histogram(position = 'identity', alpha = 0.5, color = "black")

diamonds %>% 
 filter(!is.na(price), !is.na(cut)) %>% 
  ggplot(aes(x = price, fill = cut))  + 
  geom_histogram(position = 'identity', alpha = 0.5, color = "black") +
  facet_wrap(~ cut, ncol = 3, scales = "free")

diamonds %>% 
  ggplot( aes(x = carat , y = price)) +
  geom_point(aes(col = clarity) ) +
  geom_smooth()






diamonds %>% 
  ggplot( aes(x = carat, y = price)) +
  geom_point(aes(col = clarity) ) +
  facet_wrap(~ clarity) +
  geom_smooth()

diamonds %>% 
  ggplot( aes(x = carat ,y=price)) +
  geom_point(aes(col = clarity) ) +
  geom_smooth(aes(color=clarity))

diamonds %>% 
  ggplot( aes(x = carat ,y=price)) +
  geom_point(aes(col = clarity), alpha = 0.15 ) +
  facet_wrap(~clarity)+
  geom_smooth(method = "lm")


#### Visualización interactiva ####

fun_mean <- function(x){
  
  mean <- data.frame(
    y = mean(x, na.rm = T),
    label = mean(x, na.rm = T)
    )
  
  return(mean)
  }

means <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(price = round(mean(price, na.rm=T), 1))

plot <- diamonds %>% 
  ggplot(aes(x = clarity, y = price)) +
  geom_boxplot(aes(fill = clarity)) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    colour = "darkred", 
    shape = 18,
    size = 2
    ) +
  geom_text(
    data = means, 
    aes(label = str_c("$", price), y = price + 600)
    ) +
  ggtitle("Precio vs Claridad de diamantes") +
  xlab("Claridad") +
  ylab("Precio")

plotly::ggplotly(plot)





























