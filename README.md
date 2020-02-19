# Sobre o programa

`Versão 1.0, para análise individual`

latticles é um aplictaivo web construído com o pacote shiny do software livre R (<https://www.r-project.org/>) para auxiliar na avaliação de produções científicas contidas na Plataforma Lattes do CNPq (<http://lattes.cnpq.br/>), em especial as publicações em periódicos científicos cadastrados no Qualis-CAPES (<https://sucupira.capes.gov.br/>). Trata-se de uma ferramenta de gestão institucional da Pesquisa e Pós-Graduação que permite importar artigos, classificar pelo Qualis Periódicos (2013-2016, de todas as 49 áreas de avaliação) e calcular métricas como Equivalente A1 e quantitativo A1+A2+B1 e outras, tudo de forma automatizada a partir de arquivos XML do currículo Lattes (observe "XML" no canto superior direito do seu Lattes). Outras informações como produção de livros, capítulos e orientações concluídas também são quantificadas pelo App. O usuário tem acesso à gráficos autoexplicativos 

latticles é uma iniciativa sem fins lucrativos, desenvolvido especialmente para auxiliar na autoavaliação de pesquisadores nacionais. Contribuições, críticas e sugestões são bem-vindas.

`Versão 2.0, para análise simultânea de vários currículos!`

A versão 2.0 do app latticles foi também construída no formato shiny para facilitar a importação de artigos e classificação pelo Qualis-CAPES novo, isto é, sem distinção das áreas de avaliação dos periódicos. Outra novidade é que produções técnicas/tecnológicas como softwares registrados e patentes são também contabilizadas nesta nova versão. Mas o mais importante é que agora o usuário pode analisar simultaneamente os currículos de um grupo de pesquisadores!, por exemplo, um grupo de docentes de um Programa de Pós-Graduação. Para isso, basta inserir todos os arquivos XML dos currículos Lattes num diretório e informar no web-app o caminho para este o diretório, bem como o intervalo de tempo desejado. Os totais das produções informadas no Lattes são automaticamente contabilizadas e apresentadas na forma de tabela, que pode ainda ser exportada para uma planilha Excel (em .csv) para melhor manipulação dos dados e análises mais detalhadas.

latticles2 é uma iniciativa sem fins lucrativos, desenvolvido especialmente para a autoavaliação de Programas de Pós-Graduação nacionais, com o objetivo primário de promover o crescimento e fortalecimento destes.

# Instalação
Para instalação do app é preciso ter instalada uma versão recente (>3.4) do software R (download: <https://cran.r-project.org/>). Em seguida, o pacote `devtools` deverá ser instalado com (copie e cole os comandos a seguir no console do R):

```r
install.packages("devtools")
```

Depois, para instalar a versão 1.0 do app, basta executar o comando (copie e cole no console do R) de instalação a partir da plataforma GitHub.
```r
devtools::install_github("arsilva87/latticles")
```

E, se quiser analisar simulatenamente vários currículos Lattes, instale a versão 2.0 do app com o comando (copie e cole no console do R):
```r
devtools::install_github("arsilva87/latticles2")
```

# Execute o app
```r
latticles::latticles()
```
Ou, para executar a versão 2.0, copie e cole no console do R:
```r
latticles2::latticles2()
```

Para mais instruções, assista aos vídeos demonstrativos em: <https://youtu.be/mi-etEUeL2Y> e <https://youtu.be/f7S0IDj9rk4>

# Citação
Para citar o programa em publicações, use:

da Silva, A.R. (2019). Latticles: avaliacao eficiente da producao cientifica. Programa de computador. Registro INPI: BR512019001166-0. Disponível em: <https://arsilva87.github.io/latticles_app>.

# Contato
Desenvolvedor/mantenedor do pacote: da Silva, A. R. <anderson.silva@ifgoiano.edu.br>.
Instituto Federal Goiano - Campus Urutaí.

Lattes: <http://lattes.cnpq.br/3916683240962357>

# Licença
Latticles (C) Copyright 2019 da Silva, A. R. GNU General Public Licence, version 3.0.


<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Licença Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />O conteúdo desta página está sob uma Licença <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Atribuição-CompartilhaIgual 4.0 Internacional</a>.


<!-- Global site tag (gtag.js) - Google Analytics -->
<a async src="https://www.googletagmanager.com/gtag/js?id=UA-147928758-2"></a>
<a>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-147928758-2');
</a>
