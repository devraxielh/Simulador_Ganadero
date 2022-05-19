#install.packages("readxl")
#install.packages("reticulate")
#install.packages("data.table", dependencies=TRUE)
library(readxl)
library(reticulate)
library(data.table)
library(stringr)
#py_install("pandas")
#py_install("scipy")
#py_install("openpyxl")

t1 = proc.time()
getwd()
rm(list = ls())
ruta = "C:/Users/marvi/Dropbox/Proyecto_Simulador_Ganadero/" 
source(paste(ruta,"Parametros.R",sep=""))
source(paste(ruta,"Pesobrody.R",sep=""))
source(paste(ruta,"Dataframes.R",sep=""))
source(paste(ruta,"Auxiliares.R",sep=""))

replicas = 30
resultados = c()

t1 = proc.time()
for(u in 1:replicas){
  n.animales = round(runif(Cantidad_lotes,Cantidad_min_animales,Cantidad_max_animales))
  
  animales_peso_inicial = c()
  detalle_animal = c()
  for(a in 1:sum(n.animales)){
    genero = sample(c('H','M'),1)
    raza   = sample(c('AC','BAC','BC','CAC','CC'),1)
    edad   = sample(360:400,1)
    animales_peso_inicial = c(animales_peso_inicial,pesobrody(genero=genero,raza=raza,t=edad))
    detalle_animal = c(detalle_animal,c(paste(genero,raza,edad)))
  }
  
  pesos = matrix(animales_peso_inicial, nrow = 1)
  # pepe = matrix(animales_peso_inicial, nrow = 1)
  # colnames(pepe) = paste("Lote",1:3, sep = "_")
  asigna = matrix(sample( c(1:Cantidad_lotes, rep(0,Cantidad_de_Potreros-Cantidad_lotes)) ), nrow = 1)
  
  ga.peso = round(runif(Cantidad_de_Potreros, 200, 400))
  forraje = matrix(round(runif(Cantidad_de_Potreros, Aforo_min,Aforo_max)), nrow = 1)
  forrajec0 = matrix(ffc*forraje,nrow = 1) # forraje de calidad
  forrajenc0 = matrix((1-ffc)*forraje,nrow = 1) # forraje de no calidad
  forrajec = forrajec0
  forrajenc = forrajenc0
  
  asi = asigna[asigna!=0]
  ga = ga.peso[asigna!=0]
  tasa = rep(Tasa_crecimiento_pasto, Cantidad_de_Potreros)
  tasac = rep(Tasa_crecimiento_pasto, Cantidad_de_Potreros)
  tasanc = rep(Tasa_crecimiento_pasto, Cantidad_de_Potreros)
  dias_descanso = matrix(0,1,Cantidad_de_Potreros)
  
  distancias = function(potrero){
    distan = df_potreros[11,potrero]
    I1 = gsub("\\[","",distan)
    I2 = gsub("\\]","",I1)
    I3 = gsub("\\ ","",I2)
    dis = strsplit(I3, split = ",")
    num = as.numeric(unlist(dis))
    return(num)
  }
  D = matrix(distancias(1),1,Cantidad_de_Potreros)
  for(b in 2:Cantidad_de_Potreros){
    D = rbind(D,distancias(b))
  }
  
  for(t in 1:Dias){
    #t = 1
    Temporada = as.character(df_clima[t,11])
    Mes = as.character(df_clima[t,4])
    
    # q = which(asigna[t,]!=0)
    # pe = pesos[t,1:n.animales[1]] + 
    #      ganacia_de_peso(Temporada,q[1],forraje[t,q[1]], sum(pesos[t,1:n.animales[1]]))
    # 
    # for (i in 2:length(n.animales)) {
    #   mi = sum(n.animales[1:(i-1)]) + 1
    #   mf = mi + n.animales[i]-1
    #   pe = c(pe, pesos[t,mi:mf] + 
    #          ganacia_de_peso(Temporada,q[i],forraje[t,q[i]], sum(pesos[t,mi:mf])))
    # }
    # pesos = rbind(pesos, pe)
    # 
    ###### Adición de la ganancia de peso a los pesos de los animales
    foc = rep(0,Cantidad_de_Potreros)
    fonc = rep(0,Cantidad_de_Potreros)
    q = which(asigna[t,]!=0)
    pe = c()
    
    for(i in 1:length(n.animales)){
      if(i==1){
        m.i = 1
        m.f = n.animales[1]
        s = sum(pesos[t,m.i:m.f])
      }else{
        m.i = sum(n.animales[1:(i-1)]) + 1
        m.f = m.i + n.animales[i]-1
        s = sum(pesos[t,m.i:m.f])  
      }
      f1 = forrajec[t,q[i]]
      f2 = forrajenc[t,q[i]]
      consumo = Necesidad_nutricional*s
      dife = Necesidad_nutricional*s - f1
      if(t == 1){
        p0 = which(asigna[t,] == i)
      }else{
        p0 = which(asigna[t-1,]==i)
      }
      p1 = which(asigna[t,]==i)
      if( consumo <= f1){
        pe = c(pe, pesos[t,m.i:m.f] + (1+g_fc)*ganacia_de_peso( Temporada, q[i], f1, s ) - Perdida_por_caminar*D[p0,p1] )
        tasanc[q[i]] = Tasa_crecimiento_forraje(q[i],tasanc[q[i]])
        fonc[q[i]] = max(f2 + f2*(tasanc[q[i]] - Calcular_perdida_por_sequia(Temporada,q[i],Perdida_por_sequia) 
                                  + Calcular_Incremento_por_lluvia(Temporada,Incremento_por_lluvia)
                                  - perdida_epoca_floracion(Mes,q[i],Perdida_por_floracion) ) ,0)
      }else{
        pe = c(pe, pesos[t,m.i:m.f] + 
                 (1+g_fc)*ganacia_de_peso(Temporada, q[i], f1, s) +
                 (1-g_fnc)*(dife/consumo)*ganacia_de_peso(Temporada, q[i], f2, s) - Perdida_por_caminar*D[p0,p1])
        fonc[q[i]] = max(f2 - dife + f2*(Tasa_crecimiento_pasto
                                         - Calcular_perdida_por_sequia(Temporada,q[i],Perdida_por_sequia) 
                                         + Calcular_Incremento_por_lluvia(Temporada,Incremento_por_lluvia)
                                         - perdida_epoca_floracion(Mes,q[i],Perdida_por_floracion) ) , 0)
      }
      foc[q[i]] = max(f1 - consumo + f1*(Tasa_crecimiento_pasto
                                         - Calcular_perdida_por_sequia(Temporada,q[i],Perdida_por_sequia) 
                                         + Calcular_Incremento_por_lluvia(Temporada,Incremento_por_lluvia)
                                         - perdida_epoca_floracion(Mes,q[i],Perdida_por_floracion) ) , 0)
    }
    
    
    pesos = rbind(pesos, pe)
    
    
    ###### Cálculo del forraje para los potreros no ocupados o
    
    for (j in 1:ncol(forraje)){
      
      if(asigna[t,j]==0){
        if(forrajec[t,j]==0){
          foc[j] = forraje_un_dia*ffc*as.numeric(df_potreros[3,j])
        }else{
          tasac[j] = Tasa_crecimiento_forraje(j,tasac[j])
          foc[j] = max(forrajec[t-1,j] 
                       + forrajec[t-1,j]*(tasac[j] - Calcular_perdida_por_sequia(Temporada,j,Perdida_por_sequia) 
                                          + Calcular_Incremento_por_lluvia(Temporada,Incremento_por_lluvia)
                                          - perdida_epoca_floracion(Mes,j,Perdida_por_floracion) ) ,0)
        }
        
        if(forrajenc[t,j]==0){
          fonc[j] = forraje_un_dia*(1-ffc)*as.numeric(df_potreros[3,j])
        }else{
          tasanc[j] = Tasa_crecimiento_forraje(j,tasanc[j])
          fonc[j] = max(forrajenc[t-1,j] 
                        + forrajenc[t-1,j]*(tasanc[j] - Calcular_perdida_por_sequia(Temporada,j,Perdida_por_sequia) 
                                            + Calcular_Incremento_por_lluvia(Temporada,Incremento_por_lluvia)
                                            - perdida_epoca_floracion(Mes,j,Perdida_por_floracion) ) ,0)
        }
      }
    }
    
    forrajec = rbind( forrajec,foc )
    forrajenc = rbind( forrajenc,fonc )
    
    
    
    if(!t%%Ocupacion){
      asigna = rbind(asigna, asig(asigna,v,dias_descanso[t,])$r1 )
      dias_descanso = rbind(dias_descanso,asig(asigna[-(t+1),],v,dias_descanso[t,])$r2)
      z = dias_descanso[t+1,]
      z[z<Descanso] = z[z<Descanso]-1
      z[z<0] = 0
      dias_descanso[t+1,] = z  
    }else{ 
      asigna = rbind(asigna, asigna[t,])
      z = dias_descanso[t,] -1
      z[z<0] = 0
      dias_descanso = rbind(dias_descanso,z)
    }
    
  }
  
  vectorpesos = as.vector(apply(pesos, 1, sum))
  pesopromedio =  (max(vectorpesos)-min(vectorpesos))/sum(n.animales)
  resultados = c(resultados, pesopromedio)
  
  if(!u%%5){
    write.csv(resultados, paste0("resultados",u,".csv"), row.names = F)
  }
  
}
t2 = (proc.time() - t1)/60

hist(resultados)
shapiro.test(resultados)
  
read.csv(file.choose())























t2 = proc.time()-t1; t2

asigna = asigna[1:3,]

asigna[1:20,]
forraje[1:10,1:10]
pesos[1:10,1:10]
n.animales[1:10]
write.csv(asigna, "asignacion.csv", row.names = F)
write.csv(forraje, "forraje.csv", row.names = F)
write.csv(pesos, "pesos.csv", row.names = F)
write.csv(n.animales, "numero_animales_por_lote.csv", row.names = F)


e = 1
plot(1:(Dias+1), forraje[,e], type = "l")
lines(1:(Dias+1), pesos[,e], type = "l")


?matrix

# ganacia_de_peso('Seca',4)
# ganacia_de_peso('Lluvia',4)
# ganacia_de_peso('Transicion',4)
# Calcular_perdida_por_sequia('Seca',4,Perdida_por_sequia)
# Calcular_Incremento_por_lluvia('Lluvia',Incremento_por_lluvia)



  matrix(1:4,2,2, dimnames = list(c("1","2"),c("1","2"))

