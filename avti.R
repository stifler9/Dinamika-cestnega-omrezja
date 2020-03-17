library(Rlab)

dolzina = 1000 #m
avti = c(0.1, 0.15, 0.2, 0.5, 0.7, 0.8, 0.85, 0.88, 0.9)*dolzina
hitrosti = c(13, 17, 17, 18, 16, 15, 15, 14, 14) #m/s

avto = 5 #m (dolzina avta)
varnost = 2 #m (koliko prej se zeli ustaviti)
nov_na_sek = 0.2
koncna = 50/3.6

#pospesek
# dv/dt = lambda*(v2 - v1), v2 hitrost avta pred njim (ce je na dovolj veliki razdalji)

# 1. avto lahko zabremza v v1^2/(2*bremza) poti. 
# Ce je (v1^2/(2*bremza)) > (v2^2/(2*bremza)) + s - avto - varnost, potem se avto zadaj ne bo mogel ustaviti
# na varnostni razdalji, ce bo avto pred njim zabremzal. (s je trentna razdalja med njima). ce velja zgornja neenakost,
# avto zadaj zabremza!
acc = function(kje1, v1, kje2, v2){
  lambda = 10
  bremza = -10 #m/s^2
  rez = lambda*(v2 - v1)
  if((v1^2/(2*bremza)) > (v2^2/(2*bremza)) + (kje2 - kje1) - avto - varnost){
    return(bremza)
  }
  return(max(bremza, rez))
}

premakni = function(avti, hitrosti, dt){
  n = length(avti)
  i = 1
  noviavti = vector(length = n)
  novehitrosti = vector(length = n)
  while(i < n){
    noviavti[i] = avti[i] + hitrosti[i]*dt
    novehitrosti[i] = max(hitrosti[i] + acc(avti[i], hitrosti[i], avti[i+1], hitrosti[i+1])*dt, 0)
    i = i+1
  }
  noviavti[n] = avti[n] + hitrosti[n]*dt
  if(noviavti[n] > dolzina){
    n = n-1
    noviavti = noviavti[1:n]
    novehitrosti = novehitrosti[1:n]
  }else{
    novehitrosti[n] = max(0, hitrosti[n] + acc(avti[n], hitrosti[n], dolzina*1.1, koncna)*dt)
  }
  if(rbern(1, dt*nov_na_sek)){
    rez = matrix(nrow = n+1, ncol = 2)
    rez[,1] = c(0.0,noviavti)
    rez[,2] = c(rnorm(1, koncna, 3), novehitrosti)
    return(rez)
  }
  rez = matrix(nrow = n, ncol = 2)
  rez[,1] = noviavti
  rez[,2] = novehitrosti
  return(rez)
}

plot(avti, rep(0, length(avti)), type = 'p', xlim = c(0, dolzina), ylim = c(0, 1))

interval = 1/100 #s
sekund = 60

vsiavti = numeric()
casi = numeric()

t = 0.0
i = 1

n = length(avti)
vsiavti[i:(i+n-1)] = avti
casi[i:(i+n-1)] = rep(t, n)
i = i + n

for(x in 1:(sekund/interval)){
  nov = premakni(avti, hitrosti, interval)
  avti = nov[,1]
  hitrosti = nov[,2]
  t = t + interval
  n = length(avti)
  vsiavti[i:(i+n-1)] = avti
  casi[i:(i+n-1)] = rep(t, n)
  i = i + n
}

plot(vsiavti, casi, type = 'p', xlim = c(0, dolzina), ylim = c(0, sekund), cex = 0.01)
