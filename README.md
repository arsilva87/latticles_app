# Descrição

Importe artigos, classifique pelo Qualis-CAPES (2013-2016) e calcule metricas como Eq. A1 e quantitativo A1+A2+B1, tudo de forma automatica a partir de arquivos XML do curriculo Lattes.

Este aplicativo web foi construído com o pacote shiny do software livre R.

Latticles é uma iniciativa sem fins lucrativos desenvolvido especialmente para autoavaliação de pesquisadores nacionais.

# Instalação

Para instalação do app, será necessária uma versão recente (>3.4) do software livre R (www.r-project.org). Em seguida, o pacote devtools deverá ser instalado.

```r
install.packages(devtools)
```
Depois, basta executar o comando de instalação do latticles a partir da plataforma GitHub.
```r
devtools::install_github("arsilva87/latticles")
```

# Execute o app
```r
latticles::latticles()
```

# Contato
Desenvolvedor/mantenedor do pacote: da Silva, A. R. <anderson.silva@ifgoiano.edu.br>.
Instituto Federal Goiano - Campus Urutaí

# Citação
Para citar o programa em publicações, use:

da Silva, A.R. (2019). Latticles: avaliacao eficiente da producao cientifica. Programa de computador. Registro INPI: BR512019001166-0. <https://arsilva87.github.io/latticles_app>, <https://arsilva87.github.io/latticles2>

# Licenca
GNU General Public Licence, version 3.0

Latticles (C) Copyright 2019 da Silva, A.R.
