# konstantni podatki o cestah, povezanosti, prehodnosti
#
dolzine = NULL
dolzine$"ab" = 1000
dolzine$"ad" = 900
dolzine$"bc" = 700
dolzine$"bd" = 800
dolzine$"dc" = 700

ceste = NULL
ceste$"ab" = c("a", "b", 'blue', TRUE)
ceste$"ad" = c("a", "d", 'gold3', TRUE)
ceste$"bc" = c("b", "c", 'red', FALSE)
ceste$"bd" = c("b", "d", 'green4', FALSE)
ceste$"dc" = c("d", "c", 'purple', FALSE)
# ceste[[c]][4] == TRUE, ce je cesta zacetna

povezave = NULL
povezave$"ab" = c("bc","bd")
povezave$"bd" = c("dc")
povezave$"ad" = c("dc")

koor = NULL
koor$"a"$x = 0
koor$"a"$y = 500
koor$"b"$x = 600
koor$"b"$y = 1000
koor$"c"$x = 1000
koor$"c"$y = 600
koor$"d"$x = 600
koor$"d"$y = 50