# latticles

Importe artigos, classifique pelo Qualis-CAPES (2013-2016) e calcule metricas como Eq. A1 e quantitativo A1+A2+B1, tudo de forma automatica a partir de arquivos XML do curriculo Lattes.
Este aplicativo web foi construído com o pacote shiny do software livre R.

# Instalação
Solicite código token de autorização de instalação ao desenvolvedor do pacote (anderson.silva@ifgoiano.edu.br), e então:

```r
library(devtools)
install_github("arsilva87/latticles", auth_token = "copiartoken")
```

# Execute o app
```r
library(latticles)
latticles()
```
