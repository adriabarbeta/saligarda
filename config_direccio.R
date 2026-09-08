# ==============================================================================
# config_direccio.R -- Correccio de l'orientacio de la penella
#
#   L'estacio va patir una intervencio entre 2009 i 2011 (vegeu 09b_ i 13b_).
#   Des de llavors les direccions estan girades en sentit horari respecte del
#   nord geografic. La correccio consisteix a restar l'angle.
#
#   COM S'HA ESTIMAT
#   El full de l'Ajuntament anota la direccio en graus reals fins al 2010 i en
#   rumbs de 16 des del 2012, cosa que permet comparar la mateixa estadistica a
#   banda i banda. Comparant la mitjana vectorial de la ratxa maxima diaria:
#
#     sector nord, ratxa >= 25 km/h : antiga 345,0 | nova 17,1 -> +32,0
#     sector nord, ratxa >= 30 km/h : antiga 343,2 | nova 14,9 -> +31,8
#     sector nord, ratxa >= 35 km/h : antiga 342,3 | nova 13,1 -> +30,8
#     marinada d'estiu (sud)        : antiga 151,1 | nova 185,4 -> +34,3
#
#   Que el drenatge del nord i la marinada del sud, dos regims fisicament
#   independents, hagin girat el mateix es la signatura d'una ROTACIO RIGIDA:
#   una penella mal orientada, no obstacles nous a l'emplacament.
#
#   VALIDACIO EXTERNA
#   L'aparell antic marcava 343,2 graus per al drenatge i el rumb geografic de
#   la vall cap a Aiguafreda es de 343,7: coincideixen a 0,5 graus. Per tant
#   l'epoca antiga es la bona i es la referencia de la correccio.
#
#   INCERTESA
#   Les estimacions van de +30,8 a +34,3 segons el llindar i el sector, de
#   manera que la correccio te una incertesa de l'ordre de +-2 graus. Amb
#   ratxes molt fortes (>= 45 km/h) surt +22, pero alli la quantitzacio en 16
#   rumbs atrapa la mitjana vectorial entre els compartiments de 0 i 22,5 i
#   l'estimacio no es fiable.
#
#   EFECTE SOBRE ELS RESULTATS
#   Cap. L'index es projecta sobre un eix deduit de les mateixes dades, i si
#   giren les direccions gira l'eix amb elles: u_dv queda invariant. Nomes
#   canvien les direccions que es reporten, que ara si son geografiques.
# ==============================================================================

OFFSET_PENELLA <- 32          # graus a restar a les mesures posteriors al 2010
ANY_INTERVENCIO <- 2011       # primer any afectat
ANYS_TRANSICIO <- 2009:2010   # gir progressiu: direccions poc fiables

# Compte: aqui NO es pot fer servir ifelse(). Amb un test de longitud 1
# retornaria un vector de longitud 1 i s'assignaria el mateix valor a tota la
# columna. Es detecta perque la constancia direccional |R| passa a valer 1.
corregeix_direccio <- function(graus, aplica = TRUE) {
  stopifnot(length(aplica) == 1L)
  if (!aplica) return(graus)
  (graus - OFFSET_PENELLA) %% 360
}
