# TFM-NEST-MST
Scripts en R para construir MST a partir de NESTs y realizar distintos criterios de poda para obtener subestructuras espaciales a gran escala.


`funcionesAuxiliares.R` | Funciones base para distancias y construcción del MST.
`Script1_MST.R` | Construcción de MST sintéticos. 
`Script2_MST_normalizado.R` | Normalización de ramas y construcción de MST_normalizado
`Script2b_analisis_estadistico.R` | Estadística descriptiva del los árboles normalizados.
`Script3_poda_por_metodo.R` | Poda por difrentes criterios: dn_fijo, alfa*dn_max, P90, P95, IQR, Media+sd. 
`Script3b_poda_por_codo.R` | Poda por detección de codo. 
 `Script4_estadisticas_de_poda_por_metodo.R` | Gráfico por regiones de los diferentes métodos de poda. 
 `Script4b_mapa_de_calor_por_grupos.R` | Mapa de calor de los diferentes métodos de poda. 
 `Script6_poda_probabilistica.R` | Asignación de probabilidades de corte a las ramas de los MST. 
 `Script7_comparacion_fijo_probabilistico.R` | Comparación visual entre dos métodos. 
 `Script8_MST_y_Poda_RegionesReales.R` | Aplicación a regiones reales de González et al. 2026 
