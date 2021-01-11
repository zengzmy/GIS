library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(spatialreg)
library(corrr)
library(grid)
library(dplyr)
library(spgwr)

Londonwards_2018<-dir_info(here::here("data","London-wards-2018","London-wards-2018_ESRI"
))%>%
  #$ means exact match
  dplyr::filter(str_detect(path,"London_Ward_CityMerged.shp$"))%>%
  dplyr::select(path)%>%
  pull()%>%
  #read in the file in
  st_read()
#green
green_cover_ward <- read_csv(here::here("data","green_cover_ward_summary_0.05.csv"))
#population and 65+
old_people_ward <- read_csv(here::here("data","old.csv"))

Lonwardgreen <- Londonwards_2018%>%
  left_join(.,
            green_cover_ward, 
            by = c("GSS_CODE" = "ward_code"))
#gather all variables
df <-
  left_join(Lonwardgreen,
            old_people_ward  %>% dplyr::select("Ward Code 1","65_plus"), 
            by = c("GSS_CODE" = "Ward Code 1")
  )


#clean names
df<-df %>%
  clean_names()

df <- df %>%
  mutate( old_density = round(x65_plus/hectares,2) )

#set dummy variable
innerlondon=c('Camden','Greenwich','Hackney','Hammersmith and Fulham','Islington','City and County of the City of London','Kensington and Chelsea','Lambeth','Lewisham','Southwark','Tower Hamlets','Wandsworth','City of Westminster')
outerlondon=c('Barking and Dagenham','Barnet','Bexley','Brent','Bromley','Croydon','Ealing','Enfield','Haringey','Harrow','Havering','Hillingdon','Hounslow','Kingston upon Thames','Merton','Newham','Redbridge','Richmond upon Thames','Sutton','Waltham Forest')

df <- df %>%
  mutate(
    inner_outer = case_when(
      district %in% innerlondon ~ 0,
      district %in% outerlondon ~ 1
    )
  )
#map dummy variable
qtm(df, 
    fill = "inner_outer", 
    borders = NULL,  
    fill.palette = "Blues")

#explore outliers
par(mfrow=c(1, 3))  # divide graph area in 3 columns
percent_green <- boxplot(df$percent_green, main="Boxplot of green space",
                         ylab="green space (%)",col = "#69b3a2")
percent_blue <- boxplot(df$percent_blue, main="Boxplot of blue space",
                        ylab="blue space (%)",col = "#56B4E9")
x65_plus <- boxplot(df$x65_plus, main="Boxplot of aging 65+",
                    ylab="old_density",col = "#E69F00")


#map location of London in UK
UK_outline <- st_read(here::here("data", 
                                 "gadm36_GBR_shp", 
                                 "gadm36_GBR_0.shp")) %>%
  st_transform(., 27700)
Worldcities <- st_read(here::here("data", 
                                  "World_Cities", 
                                  "a4013257-88d6-4a68-b916-234180811b2d202034-1-1fw6kym.nqo.shp")) %>%
  st_transform(., 27700)

Worldcities2 <- Worldcities %>%
  filter(CNTRY_NAME=='United Kingdom'&
           Worldcities$CITY_NAME=='London')

newbb <- c(xmin=-296000, ymin=5408, xmax=655696, ymax=1000000)

UK_outlinecrop <- UK_outline$geometry %>%
  st_crop(., newbb)

inset = tm_shape(UK_outlinecrop) + tm_polygons() +
  tm_borders(col = "grey40", lwd = 3)+
  tm_layout(frame=F,bg.color = "transparent")+
  tm_shape(Worldcities2) +
  tm_symbols(col = "red", scale = .5)+
  tm_text("CITY_NAME", xmod=-1.5, ymod=-0.5)+
  tm_scale_bar(position = c("right", "bottom"), text.size = .5)+
  tm_compass(type = "8star", position = c("right","top"))

inset

## tmap distribution variable

#green
tm_shape(df) +
  tm_polygons(col="percent_green",
              style = "pretty",
              palette = "YlGn",
              legend.hist = TRUE,
              border.alpha = 0.1,
              legend.hist.title = "Frequency of percent of green",
              title="Percent Green",
              n=6) +
  
  tm_layout(  legend.hist.bg.color = '#dedede',
              legend.title.size = 0.8,
              legend.outside.size=0.5,
              legend.outside = T,
              legend.outside.position = 'right',
              frame=F,
              title = "(a) Percent of Green Space", title.position = c("left","bottom"),
              title.size = 0.9
  ) +
  tm_scale_bar(position = c(0.00, 0.00), text.size = 0.40)+
  tm_compass(type = "8star", size = 1, position = c(0.00, 0.1))

#aging
tm_shape(df) +
  tm_polygons(col="old_density",
              style = "jenks",
              palette = "Oranges",
              legend.hist = TRUE,
              border.alpha = 0.1,
              legend.hist.title = "Frequency of 65+ denisty",
              title="65+ density") +
  
  tm_layout(  legend.hist.bg.color = '#dedede',
              legend.title.size = 0.8,
              legend.outside.size=0.5,
              legend.outside = T,
              legend.outside.position = 'right',
              frame=F,
              title = "(c) Elderly population density", title.position = c("left","bottom"),
              title.size = 0.9
  ) +
  tm_scale_bar(position = c(0.00, 0.00), text.size = 0.40)+
  tm_compass(type = "8star", size = 1, position = c(0.00, 0.1))

percent <- c(0,.01,.1,.5,.9,.99,1)

var <- df["percent_blue"] %>% st_set_geometry(NULL)
quantile(var[,1],percent)
get.var <- function(vname,df) {
  # function to extract a variable as a vector out of an sf data frame
  # arguments:
  #    vname: variable name (as character, in quotes)
  #    df: name of sf data frame
  # returns:
  #    v: vector with values (without a column name)
  v <- df[vname] %>% st_set_geometry(NULL)
  v <- unname(v[,1])
  return(v)
}
percent <- c(0,.01,.1,.5,.9,.99,1)
var <- get.var("percent_blue",df)
bperc <- quantile(var,percent)
tm_shape(df) +
  tm_fill("percent_blue",title="Percent Blue",breaks=bperc,palette="-RdBu",
          legend.hist = TRUE,
          legend.hist.title = "Frequency of Blue Space",
          labels=c("< 1%", "1% - %10", "10% - 50%", "50% - 90%","90% - 99%", "> 99%"))  +
  tm_borders() +
  tm_layout(title = "(b) Blue Space Percentile Map", 
            title.position = c("left","bottom"),
            legend.title.size = 0.8,
            legend.outside.size=0.5,
            legend.outside = T,
            legend.outside.position = c("right","bottom"),
            frame=F,
            title.size = 0.8)+
  tm_scale_bar(position = c(0.00, 0.00), text.size = 0.40)+
  tm_compass(type = "8star", size = 1, position = c(0.00, 0.1))


#map the frequency of variable
#people
ggplot(df , aes(x =  x65_plus/hectares)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#green
ggplot(df , aes(x =  percent_green)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)
ggplot(df , aes(x = (percent_blue))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

symbox(~percent_blue, 
       df, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

#pearson
cor.test(df$x65_plus/df$hectares, df$percent_green, method="pearson")
cor.test(df$x65_plus/df$hectares, df$percent_blue, method="pearson")

#OLS model

Regressiondata<- df%>%
  clean_names()%>%
  dplyr::select(x65_plus,hectares,percent_green,percent_blue,inner_outer)

model <- lm(x65_plus/hectares ~ 
              percent_green+
              percent_blue+
              inner_outer, 
            data = Regressiondata)

#show the summary of those outputs
glance(model)
tidy(model)
vif(model)
summary(model)
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model)

model_data <- model %>%
  augment(., Regressiondata)

#write the residuals out
# also add them to the shapelayer
df <- df %>%
  mutate(modelresids = residuals(model))

#plot residuals
model_data %>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 

#DW test
DW <- durbinWatsonTest(model)
tidy(DW)

#Moran's I
coordsW <- df%>%
  st_centroid()%>%
  st_geometry()
#par(mfrow=c(1,1))
#plot(coordsW)

#generate a spatial weights matrix. 
#simple binary matrix of queen's case neighbours
LWard_nb <- df %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

#plot them
#plot(LWard_nb, st_geometry(coordsW), col="red")
#plot(LWard_knn, st_geometry(coordsW), col="blue")

#create a spatial weights matrix object from these weights
Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")
Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

#9.5.10
Queen <- df %>%
  st_drop_geometry()%>%
  dplyr::select(modelresids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()
Nearest_neighbour <- df %>%
  st_drop_geometry()%>%
  dplyr::select(modelresids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

Queen
#0.475
Nearest_neighbour
#0.554

#SLM 
#queen
slag_dv_model_queen <- lagsarlm(x65_plus/hectares ~ 
                                  percent_green+
                                  percent_blue+
                                  inner_outer, 
                                data = df, 
                                nb2listw(LWard_nb, style="C"), 
                                method = "eigen")

#what do the outputs show?
tidy(slag_dv_model_queen)
glance(slag_dv_model_queen)
summary(slag_dv_model_queen)
t<-summary(slag_dv_model_queen)

#knn4
slag_dv_model_knn4 <- lagsarlm(x65_plus/hectares ~ 
                                 percent_green+
                                 percent_blue+
                                 inner_outer, 
                               data = df, 
                               nb2listw(LWard_knn, 
                                        style="C"), 
                               method = "eigen")

#what do the outputs show?
tidy(slag_dv_model_knn4)
glance(slag_dv_model_knn4)
summary(slag_dv_model_knn4)
t4 <-summary(slag_dv_model_knn4)


#SLM's Moran's I
df <- df %>%
  mutate(slag_dv_model_knn_resids = residuals(slag_dv_model_knn4))
#qtm(df, fill = "slag_dv_model_knn_resids")

KNN4Moran <- df %>%
  st_drop_geometry()%>%
  dplyr::select(slag_dv_model_knn_resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

KNN4Moran

df <- df %>%
  mutate(slag_dv_model_queen_resids = residuals(slag_dv_model_queen))
#qtm(df, fill = "slag_dv_model_queen_resids")

queenMoran <- df %>%
  st_drop_geometry()%>%
  dplyr::select(slag_dv_model_queen_resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()

queenMoran


#SEM_model
sem_model <- errorsarlm(x65_plus/hectares ~ 
                          percent_green+
                          percent_blue+
                          inner_outer, 
                        data = df,
                        nb2listw(LWard_knn, style="C"), 
                        method = "eigen")
tidy(sem_model)
glance(sem_model)
summary(sem_model)

#SEM's Moran's I
df <- df %>%
  mutate(sem_model_knn_resids = residuals(sem_model))
#qtm(df, fill = "sem_model_knn_resids")

semMoran <- df %>%
  st_drop_geometry()%>%
  dplyr::select(sem_model_knn_resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

semMoran

#GWR model
library(spgwr)

st_crs(df) = 27700

dfSP <- df %>%
  as(., "Spatial")

st_crs(coordsW) = 27700
coordsWSP <- coordsW %>%
  as(., "Spatial")
coordsWSP

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(x65_plus/hectares ~ 
                          percent_green+
                          percent_blue+
                          inner_outer, 
                        data = dfSP, 
                        coords=coordsWSP,
                        adapt=T)

#run the gwr model
gwr.model = gwr(x65_plus/hectares ~ 
                  percent_green+
                  percent_blue+
                  inner_outer, 
                data = dfSP, 
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model


#GWR's Moran's I
gwr.morantest(gwr.model,Lward.knn_4_weight)

#save the results of GWR model

results <- as.data.frame(gwr.model$SDF)
names(results)
#attach coefficients to original SF
dfs <- df %>%
  mutate(coefgreen = results$percent_green,
         coefblue = results$percent_blue,
         
  )
#run the significance test
sigTest1 = abs(gwr.model$SDF$"percent_green")-2 * gwr.model$SDF$"percent_green_se"
sigTest2 = abs(gwr.model$SDF$"percent_blue")-2 * gwr.model$SDF$"percent_blue_se"

#store significance results
dfs <- dfs %>%
  mutate(GWRgreenSig = sigTest1)
dfs <- dfs %>%
  mutate(GWRblueSig = sigTest2)

tm_shape(dfs) +
  tm_polygons(col = "coefgreen", 
              palette = "RdBu", 
              alpha = 0.5)+
  tm_layout(title = "(a) coefficient of green",
            title.position = c("left","bottom"))
tm_shape(dfs) +
  tm_polygons(col = "coefblue", 
              palette = "RdBu", 
              alpha = 0.5)+
  tm_layout(title = "(c) coefficient of blue",
            title.position = c("left","bottom"))
tm_shape(dfs) +
  tm_polygons(col = "GWRgreenSig", 
              palette = "RdYlBu")+
  tm_layout(title = "(b) significance of green",
            title.position = c("left","bottom"))
tm_shape(dfs) +
  tm_polygons(col = "GWRblueSig", 
              palette = "RdYlBu")+
  tm_layout(title = "(d) significance of blue",
            title.position = c("left","bottom"))































