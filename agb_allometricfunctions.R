### AGB allometric functions ###

# Abies
# [0.0754]*[DBH^2.513]
# Avedaño et al., 2009

agb_abies <- function(dbh) {
  ((0.0754 * (dbh^2.513)) * 0.001)
}

# Alchornea latifolia
# [Exp[-3.363]*[DBH^2.2714]*[TH^0.4984]
# Aquino-Ramírez et al., 2015

agb_alchornealatifolia <- function(dbh, h){
  (((exp(-3.363))*(dbh^2.2714)*(h^0.4984)) * 0.001)
}

# Alnus acuminata
# [Exp[-2.14]*[DBH^2.23]]
# Acosta-Mireles et al, 2002

agb_alnusacuminata <- function(dbh) {
  (((exp(-2.14)) * (dbh^2.23)) * 0.001)
}

# Alnus jorullensis
# [0.0195]*[DBH^2.7519]
# Carrillo et al., 2014

agb_alnusjorullensis <- function(dbh) {
  (((0.0195) * (dbh^2.7519)) * 0.001)
}

# Brosimum alicastrum
# [0.479403]*[DBH^2.0884]
# Rodríguez-Laguna et al., 2008

agb_brosimumalicastrum <- function(dbh) {
  (((0.479403) * (dbh^2.0884)) * 0.001)
}

# Cecropia obtusifolia
# [[0.000022]*[D^1.9]*[H]] +  [[-0.56 + 0.02[D^2] + 0.04[H]]/10^3]
# Hughes et al., 1999

agb_cecropiaobtusifolia <- function(dbh, h) {
  (((0.000022) * (dbh^2) * (h)) + ((-0.56 + 0.02*(dbh^2) + 0.04*(h)) / 10^3))
}

# Citrus
# [-6.64]+[0.279*BA]+[0.000514*BA^2]
# Schroth et al., 2002

agb_citrus <- function(ba) {
  ((-6.64) + (0.279*ba) + (0.000514*ba^2)) * 0.001
}

# Clethra
# [Exp[-1.90]*[DBH^2.15]]
# Acosta et al., 2002

agb_clethra <- function(dbh) {
  ((exp(-1.90)) * (dbh^2.15)) * 0.001
}

# Clethra hartwegii
# [Exp[-1.90]*[DBH^2.15]]
# Acosta et al., 2002

agb_clethrahartwegii <- function(dbh) {
  ((exp(-1.90)) * (dbh^2.15)) * 0.001
}

# Clethra mexicana
# [0.4632]*[DBH^1.8168]
# Acosta et al., 2011

agb_clethramexicana <- function(dbh) {
  ((0.4632) * (dbh^1.8168)) * 0.001
}

# Clethra pringlei
# [0.067833]*[DBH^2.50972]
# Rodríguez et al., 2006

agb_clethrapringlei <- function(dbh) {
  ((0.067833) * (dbh ^2.50972)) * 0.001
}

# Cordia alliodora
# [10^-0.755]*[DBH^2.072]
# Segura et al., 2006

agb_cordiaalliodora <- function(dbh) {
  ((10^-0.755) * (dbh^2.072)) * 0.001
}

# Cupressus lusitanica
# [0.5266]*[DBH^1.7712]
# Vigil, 2010

agb_cupressuslusitanica <- function(dbh) {
  (((0.5266) * (dbh^1.7712)) * 0.001)
}

# Dendropanax arboreus
# [0.037241]*[DBH^2.99585]
# Rodríguez-Laguna et al., 2008

agb_dendropanaxarboreus <- function(dbh) {
  (((0.037241) * (dbh^2.99585)) * 0.001)
}

# Eugenia
# [0.4600]+[[0.0370]*[DBH^2]*TH]
# Cairns et al., 2003

agb_eugenia <- function(dbh, h) {
  ((0.4600) + ((0.0370) * (dbh^2) * h)) * 0.001
}

# Fraxinus uhdei
# [362.129]*[[3.1416]*[[[[DBH^2]/4]]^1.100]]
# Cano, 1994

agb_fraxinusuhdei <- function(dbh) {
  (((362.129) * ((3.1416) * (((dbh^2)/4)^1.100))) * 0.000001)
}

# Heliocarpus appendiculatus
# [[Exp[4.9375]] * [[DBH^2]^1.0583]] * [1.14]/ 1000000
# Hughes et al., 1999

agb_heliocarpusappendiculatus <- function(dbh) {
  ((exp(4.9375)) * ((dbh^2)^1.0583) * (1.14))/ 1000000
}

# Inga
# [Exp[-1.76]*[DBH^2.26]]
# Acosta et al., 2002

agb_inga <- function(dbh) {
  ((exp(-1.76)) * (dbh^2.26)) * 0.001
}

# Inga vera
# [Exp[-1.76]*[DBH^2.26]]
# Acosta et al., 2002

agb_ingavera <- function(dbh) {
  ((exp(-1.76)) * (dbh^2.26)) * 0.001
}

# Inga punctata
# [Exp[-3.363]*[DBH^2.4809]*[TH^0.4984]
# Aquino-Ramírez et al., 2015

agb_ingapunctata <- function(dbh, h) {
  ((exp(-3.363)) * (dbh^2.4809) * (h^0.4984)) * 0.001
}

# Juglans olanchana
# [10^-1.417]*[DBH^2.755]
# Segura et al., 2006

agb_juglansolanchana <- function(dbh) {
  ((10^-1.417) * (dbh^2.755)) * 0.001
}

# Juniperus flaccida
# [0.209142]*[DBH^1.698]
# Rodríguez et al., 2009

agb_juniperusflaccida <- function(dbh) {
  ((0.209142) * (dbh^1.698)) * 0.001
}

# Liquidambar
# [Exp[-2.22]*[DBH^2.45]]
# Acosta et al., 2002

agb_liquidambar <- function(dbh) {
  ((exp(-2.22)) * (dbh^2.45)) * 0.001
}

# Liquidambar styraciflua
# [0.180272]*[DBH^2.27177]
# Rodríguez et al., 2006

agb_liquidambarstyraciflua <- function(dbh) {
  ((0.180272) * (dbh^2.27177)) * 0.001
}

# Nectandra ambigens
# [[Exp[4.9375]]*[[DBH^2]^1.0583]]*[1.14]/1000000
# Hughes et al., 1999

agb_nectandra <- function(dbh) {
  (((exp(4.9375)) * ((dbh^2)^1.0583)) * 1.14)/1000000
}

# Pinus
# [0.058]*[[[DBH^2]*TH]^0.919]
# Ayala, 1998

agb_pinus <- function(dbh, h) {
  ((0.058) * (((dbh^2)*h) ^0.919)) * 0.001
}

# Pinus ayacahuite
# [0.058]*[[[DBH^2]*TH]^0.919]
# Ayala, 1998

agb_pinus <- function(dbh, h) {
  ((0.058) * (((dbh^2)*h) ^0.919)) * 0.001
}

# Pinus devoniana
# [0.182]*[DBH^1.936]
# Méndez et al., 2011

agb_pinusdevoniana <- function(dbh) {
  ((0.182) * (dbh^1.936)) * 0.001
}

# Pinus herrerae
# [0.1354]*[DBH^2.3033]
# Návar, 2009

agb_pinusherrerae <- function(dbh) {
  ((0.1354) * (dbh^2.3033)) * 0.001
}

# Pinus leiophylla
# [[Exp^-3.549]*[DBH^2.787]]]
# Návar, 2009

agb_pinusleiophylla <- function(dbh) {
  ((exp(-3.549) * (dbh^2.787))) * 0.001
}

#Pinus oocarpa
# [0.058]*[[[DBH^2]*TH]^0.919]
# Ayala, 1998

agb_pinusoocarpa <- function(dbh, h) {
  ((0.058) * (((DBH^2) * h)^0.919)) * 0.001
}

# Pinus patula
# [0.0514]*[DBH^2.5222]
# Pacheco, 2011

agb_pinuspatula <- function(dbh) {
  ((0.0514)*(dbh^2.5222)) * 0.001
}

# Pinus pseudostrobus
# [0.058]*[[[DBH^2]*TH]^0.919]
# Ayala, 1998

agb_pinuspseudostrobus <- function(dbh, h) {
  ((0.058)*(((dbh^2) * h)^0.919)) * 0.001
}

# Prunus persica
# [Exp[-2.76]*[DBH^2.37]]
# Acosta, 2003

agb_prunuspersica <- function(dbh) {
  ((exp(-2.76)) * (dbh^2.37)) * 0.001
}

# Psidium guajava
# [0.246689]*[DBH^2.24992]
# Rodríguez-Laguna et al., 2008

agb_psidiumguajava <- function(dbh) {
  (((0.246689) * (dbh^2.24992)) * 0.001)
}

# Quercus
# [0.1269]*[DBH^2.5169]
# González, 2008

agb_quercus <- function(dbh) {
  (((0.1269) * (dbh^2.5169)) * 0.001)
}

# Quercus candicans
# [[Exp[-4.775313]*[DBH^1.798292]*[TH^1.570775]]+[[Exp[-3.547008]*[DBH^2.593972]]+[[Exp[-4.752007]*DBH^2]]
# Cortés-Sánchez et al., 2019

agb_quercuscandicans <- function(dbh, h) {
  ( ((exp(-4.775313)) * (dbh^1.798292) * (h^1.570775)) + ((exp(-3.547008)) * (dbh^2.593972)) + ((exp(-4.752007)) * (dbh^2)) ) * 0.001
}                                                

# Quercus crassifolia
# [0.283]*[[[DBH^2]*TH]^0.807]
# Ayala, 1998

agb_quercuscrassifolia <- function(dbh, h) {
  ((0.283) * (((dbh^2) * h)^0.807)) * 0.001
}

# Quercus laurina
# [0.283]*[[[DBH^2]*TH]^0.807]
# Ayala, 1998

agb_quercuslaurina <- function(dbh, h) {
  ((0.283) * (((dbh^2) * h)^0.807)) * 0.001
}

# Quercus obtusata
# [[exp[-3.53684]*[DBH^2.043763]*[TH^0.759522]]+[[Exp[-5.803952]*[DBH^2*TH]^1.224292]]+[[Exp[-6.181035]*[DBH^2.488617]]
# Cortés-Sánchez et al., 2019

agb_quercusobtusata <- function(dbh, h) {
  ( ((exp(-3.53684)) * (dbh^2.043763) * (h^0.759522)) + ((exp(-5.803952)) * ((dbh^2 * h)^1.224292)) + ((exp(-6.181035)) * (dbh^2.488617)) ) * 0.001
}
                                                                                      
# Quercus peduncularis
# [Exp[-2.27]*[DBH^2.39]]
# Acosta, et al., 2002

agb_quercuspeduncularis <- function(dbh) {
  ((exp(-2.27)) * (dbh^2.39)) * 0.001
}

# Quercus rugosa
# [0.283]*[[[DBH^2]*TH]^0.807]
# Ayala, 1998

agb_quercusrugosa <- function(dbh, h) {
  ((0.283) * (((dbh^2) * h)^0.807)) * 0.001
}


# Trema micrantha
# Van Breugel et al., 2011
# [-2.305 + 2.351 * ln[DBH]] * 1.033

agb_trema <- function(dbh) {
  ((-2.305 + 2.351 * log(dbh)) * 1.033)* 0.001
}

# Trichilia havanensis
# [0.130169]*[DBH^2.34924]
# Rodríguez-Laguna et al., 2008

agb_trichiliahavanensis <- function(dbh) {
  (((0.130169) * (dbh^2.34924)) * 0.001)
}

# Trichospermum mexicanum
# [0.449]*[DBH^2]-33.565
# Montes de Oca-Cano et al., 2020

agb_trichospermummexicanum <- function(dbh) {
  (((0.449) * (dbh^2)) -33.565) * 0.001
}

# Zanthoxylum 
# [0.00166]*[DBH^3.6586]
# Manzano, 2010

agb_zanthoxylum <- function(dbh){
  ((0.00166) * (dbh^3.6586)) * 0.001
}
