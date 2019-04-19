# latticles

Importe artigos, classifique pelo Qualis-CAPES (2013-2016) e calcule metricas como Eq. A1 e quantitativo A1+A2+B1, tudo de forma automatica a partir de arquivos XML do curriculo Lattes.
Este aplicativo web foi construído com o pacote shiny do software livre R.

# Instalação

Faça o download de todos os arquivos e extraia em uma pasta. Em seguida, faça a instalação do R-Portable na mesma pasta, mantendo os aqrquivos de instalação num diretório de mesmo nome (R-Portable). 
Execute o arquivo R-Portable.exe e instale os pacotes 'shiny' e 'xml2'.

```r
install.packages(c("shiny", "xml2"))
```

Feche o R e execute o arquivo latticles.exe.
