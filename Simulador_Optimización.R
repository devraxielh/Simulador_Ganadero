library(lpSolve)
library(lpSolveAPI)


?lp
?lp.assign



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
ruta = "/Users/raxielh/SimuladorGanadero/"
source(paste(ruta,"Parametros.R",sep=""))
source(paste(ruta,"Pesobrody.R",sep=""))
source(paste(ruta,"Dataframes.R",sep=""))
source(paste(ruta,"Auxiliares.R",sep=""))

#set.seed(1234567)

n.animales = round(runif(Cantidad_lotes,Cantidad_min_animales,Cantidad_max_animales))

animales_peso_inicial = c()
detalle_animal = c()
for(a in 1:sum(n.animales)){
  genero = sample(c('H','M'),1)
  raza   = sample(c('AC','BAC','BC','CAC','CC'),1)
  edad   = sample(547:912,1)
  animales_peso_inicial = c(animales_peso_inicial,pesobrody(genero=genero,raza=raza,t=edad))
  detalle_animal = c(detalle_animal,c(paste(genero,raza,edad)))
}

pesos = matrix(animales_peso_inicial, nrow = 1)
# pepe = matrix(animales_peso_inicial, nrow = 1)
# colnames(pepe) = paste("Lote",1:3, sep = "_")
asigna = matrix(sample( c(1:Cantidad_lotes, rep(0,Cantidad_de_Potreros-Cantidad_lotes)) ), nrow = 1)

ga.peso = round(runif(Cantidad_de_Potreros, 200, 400))
forraje = matrix(round(runif(Cantidad_de_Potreros, Aforo_min,Aforo_max)), nrow = 1)

asi = asigna[asigna!=0]
ga = ga.peso[asigna!=0]
tasa = rep(Tasa_crecimiento_pasto, Cantidad_de_Potreros)
dias_descanso = matrix(0,1,Cantidad_de_Potreros)


for(t in 1:Dias){
  t =1
  Temporada = as.character(df_clima[t,11])
  Mes = as.character(df_clima[t,4])
  
  
  d = matrix(0,Cantidad_lotes,Cantidad_de_Potreros)
  G = matrix(0,Cantidad_lotes,Cantidad_de_Potreros)
  for(i in 1:Cantidad_lotes){
    Ca = n.animales[i]
    if(i==1){
      wi = sum(pesos[t,1:n.animales[1]])
    }else{
      m.i = sum(n.animales[1:(i-1)]) + 1
      m.f = m.i + n.animales[i]-1
      wi = sum(pesos[t,m.i:m.f])
      
    }
    for (j in 1:Cantidad_de_Potreros) {
      d[i,j] = (ffc*forraje[t,j])/(Necesidad_nutricional*wi)
      g = ganacia_de_peso(Temporada,j,forraje[t,j], wi)
      G = d[i,j]*g*Ca
    }
  }
  d2 = round(d,0)
 
  
  q = which(asigna[t,]!=0)
  pe = pesos[t,1:n.animales[1]] + ganacia_de_peso(Temporada,q[1],forraje[t,q[1]], sum(pesos[t,1:n.animales[1]]))
  
  for (i in 2:length(n.animales)) {
    mi = sum(n.animales[1:(i-1)]) + 1
    mf = mi + n.animales[i]-1
    pe = c(pe, pesos[t,mi:mf] + ganacia_de_peso(Temporada,q[i],forraje[t,q[i]], sum(pesos[t,mi:mf]) ))
  }
  pesos = rbind(pesos, pe)


  
  
  
  
  fo = c()
  for (j in 1:ncol(forraje)) {
    
    if(forraje[t,j]==0){
      fo = c(fo, forraje_un_dia)
      
    }else{
      
      if(asigna[t,j]!=0){
        
        qlot = asigna[t,j]
        n.qlot = n.animales[qlot]
        if(qlot==1){
          mmi = 1 
          mmf = n.animales[qlot]
        }else{
          mmi = sum(n.animales[1:(qlot-1)]) + 1 
          mmf = sum(n.animales[1:(qlot-1)]) + n.animales[qlot]
        }
        
        fo = c(fo, max(forraje[t,j] 
                       - Necesidad_nutricional*sum(pesos[t, mmi:mmf]) 
                       + forraje[t,j]*(Tasa_crecimiento_pasto
                                       - Calcular_perdida_por_sequia(Temporada,j,Perdida_por_sequia) 
                                       + Calcular_Incremento_por_lluvia(Temporada,Incremento_por_lluvia)
                                       - perdida_epoca_floracion(Mes,j,Perdida_por_floracion)
                       )
                       ,0)
        )
        
      }else{
        tasa[j] = Tasa_crecimiento_forraje(j,tasa[j])
        fo = c(fo, max(forraje[t,j] 
                       + forraje[t,j]*(tasa[j]
                                       - Calcular_perdida_por_sequia(Temporada,j,Perdida_por_sequia) 
                                       + Calcular_Incremento_por_lluvia(Temporada,Incremento_por_lluvia)
                                       - perdida_epoca_floracion(Mes,j,Perdida_por_floracion)
                       )
                       ,0)
        )
      }
    }
    
    
  }
  
  forraje = rbind( forraje,fo )
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

t2 = proc.time()-t1; t2

asigna = asigna[1:3,]

asigna[1:20,1:25]
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
       
       