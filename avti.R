# datoteka za drugacno vizualizacijo premikanja avtomobilov po eni cesti
library(Rlab)

dolzina = 1000 #m
avti = c(0.1, 0.15, 0.2, 0.5, 0.7, 0.8, 0.85, 0.88, 0.9)*dolzina
hitrosti = c(13, 17, 17, 18, 16, 15, 15, 14, 14) #m/s

avto = 5 #m (dolzina avta)
koncna = 70/3.6

#pospesek
# dv/dt = lambda*(v2 - v1), v2 hitrost avta pred njim (ce je na dovolj veliki razdalji)
# ce ima avto 50m pred seboj prostorse prilagaja koncni hitrosti

# 1. avto lahko zabremza v v1^2/(2*bremza) poti. 
# Ce je (v1^2/(2*bremza)) > (v2^2/(2*bremza)) + s - avto - varnost, potem se avto zadaj ne bo mogel ustaviti
# na varnostni razdalji, ce bo avto pred njim zabremzal. (s je trentna razdalja med njima). ce velja zgornja neenakost,
# avto zadaj zabremza!
lambda = 3
bremza = -10
#m/s^2
maxposp = 10
#maksimalni pospesek
nov_na_sek = 0.3

acc = function(v1, v2, gap){
  if((v1^2/(2*bremza)) > (v2^2/(2*bremza)) + gap - avto){
    # Ce velja neenakost, potem se avto zadaj ne bo mogel ustaviti
    # na varnostni razdalji, ce bo avto pred njim zabremzal.
    return(bremza)
  }
  if(gap - avto > 4*v1){
    rez = lambda*(koncna - v1)
  }else{
    rez = lambda*(v2 - v1)
  }
  return(min(max(bremza, rez), maxposp))
}

premakni = function(avti, hitrosti, dt, rdeca){
  n = length(avti)
  noviavti = vector(length = n)
  novehitrosti = vector(length = n)
  i = 1
  while(i < n){
    noviavti[i] = avti[i] + hitrosti[i]*dt
    novehitrosti[i] = max(hitrosti[i] + acc(hitrosti[i], hitrosti[i+1], avti[i+1] - avti[i])*dt, 0)
    i = i+1
  }
  noviavti[n] = avti[n] + hitrosti[n]*dt
  if(noviavti[n] > dolzina){
    # ce vodilni avto pride do konca
    n = n-1
    noviavti = noviavti[1:n]
    novehitrosti = novehitrosti[1:n]
    if(n == 0){
      noviavti = c()
      novehitrosti = c()
    }
  }else{
    if(rdeca){
      # ce je na koncu rdeca luc
      novehitrosti[n] = max(0, hitrosti[n] + acc(hitrosti[n], 0, dolzina - avti[n])*dt)
    }else{
      novehitrosti[n] = max(0, hitrosti[n] + acc(hitrosti[n], koncna, dolzina + 100 - avti[n])*dt)
    }
  }
  if(rbern(1, dt*nov_na_sek)){
    rez = matrix(nrow = n + 1, ncol = 2)
    rez[,1] = c(0.0, noviavti)
    rez[,2] = c(runif(1, koncna - 3, koncna + 3), novehitrosti)
    return(rez)
  }
  rez = matrix(nrow = n, ncol = 2)
  rez[,1] = noviavti
  rez[,2] = novehitrosti
  return(rez)
}

interval = 1/50 #s
sekund = 60
semafor = c(0, 1500)

vsiavti = numeric()
casi = numeric()

i = 1
n = length(avti)
vsiavti[i:(i+n-1)] = avti
casi[i:(i+n-1)] = rep(0, n)
i = i + n

steps = (sekund/interval)
for(x in 1:steps){
  if((semafor[1] <= x) & (x <= semafor[2])){
    nov = premakni(avti, hitrosti, interval, TRUE) #rdeca
  }else{
    nov = premakni(avti, hitrosti, interval, FALSE)
  }
  avti = nov[,1]
  hitrosti = nov[,2]
  n = length(avti)
  vsiavti[i:(i+n-1)] = avti
  casi[i:(i+n-1)] = rep(x, n)
  i = i + n
}

plot(vsiavti, casi, type = 'p', xlim = c(0, dolzina), ylim = c(0, steps), cex = 0.01)
