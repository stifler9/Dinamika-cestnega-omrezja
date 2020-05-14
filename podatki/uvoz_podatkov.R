library(stringr)

#precistimo podatke da imamo informacije od kje do kje pelje cesta in ostale pomembne podatke

podatkiCeste <- read.csv2(file = "podatki/pldp2018noo.csv", sep = ";", stringsAsFactors = FALSE, encoding = 'utf8')

indeksi = c()
for (i in 1:length(podatkiCeste[,1])) {
  if(length(strsplit(as.character(podatkiCeste[i,4]), split = " - ")[[1]]) == 2){
    indeksi = c(indeksi, i)
  }
}

urejeniPodatki <- data.frame(matrix(nrow = length(indeksi), ncol = 5))
colnames(urejeniPodatki) <- c("Kategorija", "Od", "Do", "Dolzina", "Vsa vozila")
j = 1
for (i in indeksi) {
  oddo = strsplit(as.character(podatkiCeste[i, 4]), split = " - ")[[1]]
  # ce je splitano znotraj oklepaja
  if(str_detect(oddo[1], '[(]')){
    if(! str_detect(oddo[1], '[)]')){
      kjeokl = str_locate(oddo[1], '[(]')[1]
      dolzina = str_length(oddo[1])
      spredaj = str_trim(substr(oddo[1], 1, kjeokl))
      oddo[1] = str_trim(substr(oddo[1], kjeokl + 1, dolzina))
      oddo[1] = paste(spredaj, oddo[1], ")", sep = '')
      oddo[2] = paste(spredaj, str_trim(oddo[2]), sep = '')
    }
  }
  urejeniPodatki[j,] = c(podatkiCeste[i,1],
                         str_trim(oddo[1]),
                         str_trim(oddo[2]),
                         as.integer(gsub("[.]", "", as.character(podatkiCeste[i,6]))) - as.integer(gsub("[.]", "", as.character(podatkiCeste[i,5]))),
                         as.integer(gsub("[.]", "", as.character(podatkiCeste[i,10]))))
  j = j + 1
}

write.csv2(urejeniPodatki, file = "podatki/urejeni_podatki.csv", row.names = FALSE)