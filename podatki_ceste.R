ceste = data.frame()
ceste[,1] = character()
ceste[,2] = character()
ceste[,3] = character()
ceste[,4] = logical()
ceste[,5] = numeric()
ceste["a_b",] = c("a", "b", 'blue', TRUE, 1050)
ceste["a_d",] = c("a", "d", 'gold3', TRUE, 900)
ceste["b_c",] = c("b", "c", 'red', FALSE, 700)
ceste["b_d",] = c("b", "d", 'green4', FALSE, 800)
ceste["d_c",] = c("d", "c", 'purple', FALSE, 700)
ceste[,4] = as.logical(ceste[,4])
ceste[,5] = as.numeric(ceste[,5])
# ceste[[c]][4] == TRUE, ce je cesta zacetna
# ceste[[c]][5] = dolzina

povezave = NULL
povezave$"a_b" = c("b_c","b_d")
povezave$"b_d" = c("d_c")
povezave$"a_d" = c("d_c")

koor = data.frame()
koor[,1] = numeric()
koor[,2] = numeric()
koor["a",] = c(0,500)
koor["b",] = c(600,1000)
koor["c",] = c(1000,600)
koor["d",] = c(650,50)

semaforji = list()
semaforji$"d" = data.frame(matrix(nrow = 2, ncol = 2))
colnames(semaforji$"d") = c("a_d__d_c", "b_d__d_c")
semaforji$d[1,] = c(TRUE, FALSE)
semaforji$d[2,] = c(FALSE, TRUE)
