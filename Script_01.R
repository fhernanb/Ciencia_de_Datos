# En este script se analiza la base de datos
# creditos a microempresarios 
# obtenida del repositorio de datos abiertos
# de la Alcaldia de Medellin

# Leyendo la base de datos ------------------------------------------------
library(tidyverse)

datos <- read_csv2(file="bases/creditos_otorgados_a_microempresarios.csv",
                  col_names = TRUE)

datos |> dim()

datos

# Organizando los datos ---------------------------------------------------

datos |> 
  mutate(monto = monto / 1000000) -> datos

datos |> 
  mutate(edad = replace(edad, which(edad < 18), NA)) -> datos

datos |> 
  mutate(sexo = replace(sexo, which(sexo == "#N/A"), NA)) -> datos

library(lubridate)

datos |> 
  mutate(fecha = dmy(fecha)) -> datos

datos |> 
  mutate(ano = year(fecha),
         mes = month(fecha),
         dia = day(fecha)) -> datos

datos |> 
  filter(monto < 1000) -> datos


# # Analisis univariado ---------------------------------------------------

# Para el monto
datos |> 
  pull(monto) |> 
  summary()

datos |> 
  summarise(monto_min=min(monto),
            monto_mediano=median(monto),
            monto_prom=mean(monto),
            monto_max=max(monto),
            desvi_monto=sd(monto))

# Para edad
datos |> 
  pull(edad) |> 
  summary()

# Para sexo
datos |> 
  pull(sexo) |> 
  table()

# Para actividad
datos |> 
  pull(actividad) |> 
  table()

# Para barrio
datos |> 
  pull(barrio) |> 
  table()

# Para comuna
datos |> 
  pull(comuna) |> 
  table()

# Por ano
datos |> 
  pull(ano) |> 
  table()

# Analisis multivariado - cruzando variables ------------------------------

datos |> 
  group_by(sexo) |> 
  summarise(monto_min=min(monto),
            monto_mediano=median(monto),
            monto_prom=mean(monto),
            monto_max=max(monto),
            desvi_monto=sd(monto),
            numero = n())

datos |> 
  group_by(actividad) |> 
  summarise(monto_min=min(monto),
            monto_mediano=median(monto),
            monto_prom=mean(monto),
            monto_max=max(monto),
            desvi_monto=sd(monto),
            numero = n())

datos |> 
  group_by(sexo, actividad) |> 
  summarise(monto_min=min(monto),
            monto_mediano=median(monto),
            monto_prom=mean(monto),
            monto_max=max(monto),
            desvi_monto=sd(monto),
            numero = n())

datos |> 
  group_by(ano) |> 
  summarise(monto_min=min(monto),
            monto_mediano=median(monto),
            monto_prom=mean(monto),
            monto_max=max(monto),
            desvi_monto=sd(monto),
            numero = n())

# Algunos graficos --------------------------------------------------------
library(ggplot2)

# Algunos boxplots

ggplot(data=datos, aes(x=monto)) +
  geom_boxplot() +
  labs(title="Boxplot para el monto", x="Monto (millones $)")
  
datos |> 
  summarise(num_monto_sup_1000 = sum(monto >= 1000))

ggplot(data=datos, aes(y=monto, x=sexo, fill=sexo)) +
  geom_boxplot() +
  labs(x="Sexo", y="Monto (millones $)")
  
ggplot(data=datos, aes(y=monto, x=actividad, fill=actividad)) +
  geom_boxplot() +
  labs(x="Actividad", y="Monto (millones $)")  
  
ggplot(data=datos, aes(y=monto, x=as.factor(ano), fill=as.factor(ano))) +
  geom_boxplot() +
  labs(x="Actividad", y="Monto (millones $)")  

# Densidades

ggplot(data=datos, aes(x=monto)) +
  geom_density(lwd=2, color="tomato") +
  labs(y="Densidad", x="Monto (millones $)")
  
ggplot(data=datos, aes(x=monto, color=sexo)) +
  geom_density(lwd=1.5) +
  labs(y="Densidad", x="Monto (millones $)")

ggplot(data=datos, aes(x=monto, color=sexo)) +
  geom_density(lwd=1.5) +
  labs(y="Densidad", x="Monto (millones $)")

ggplot(data=datos, aes(x=monto, fill=sexo))+
  geom_density(alpha=0.5)+
  facet_wrap("sexo")

# Graficos de dispersion
ggplot(data=datos, aes(x=edad, y=monto)) +
  geom_point()

  
  



