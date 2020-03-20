# Curso Virtual de Text Mining


# Sobre este repositorio

En este repositorio podrán encontrar todo el material que utilizaremos en el curso. Se irá actualizando constanemente y podrán descargarlo si así lo desean o clonarlo por medio de *GitHub*.

## Objetivo del curso

La idea de este curso virtual es la de mostrar algunas herramientas de análisis de texto. Desde su extracción hasta su presentación, pasando, naturalmente, por su análisis. Haciendo uso de las herramientas que provee el `tidyverse` y los paquetes `tidytext`, `tm` y `quanteda`. Asimismo, también tiene como objetivo visualizar y comunicar de manera efectiva los hallazgos de estas herramientas.

## Requisitos

Sólo se necesitan tres cosas:

1. Tener RStudio  y R instalados en sus últimas veriones.

2. Tener concimento básico de la sintaxis del `tidyverse`.

### Para resolver (y buscar dudas)

- [Stack Overflow](https://stackoverflow.com/): No hay de otra, **es crucial preguntar**.
  
- [Statistical tools for high-throughput data analysis](http://www.sthda.com/english/)

- [UC Business Analytics R Programming Guide](https://uc-r.github.io/)
  
- [Prabhakaran - Top 50 ggplot2 Visualizations - The Master List (With Full R Code)](http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html)
  
- [Sebastián Garrido - Recursos para R](http://segasi.com.mx/cursos/recursos_r/recursos/index.html): "Segasi" ha recolectado un montón de recursos introductorios (y no tanto) en su página personal.

#### RMarkdown y otras herramientas

En este curso se pretende que podamos presentar resultados en reportes profesionales y muy elegantes que puean ser leídos (y usados) por cualquiera. Así, usaremos *Markdown* "con sabor" a `R` para generar código y resultados.

- [Xie, Allaire & Grolemund - R Markdown: The Definitive Guide](https://bookdown.org/yihui/rmarkdown/): La Guía definitiva para usar *Markdown* "con sabor" a R.
  
- [A simple guide to LaTeX - Step by Step](https://www.latex-tutorial.com/tutorials/): Si bien no usaremos *LaTeX* directamente, sí es importante tenerlo instalado y conocer su existencia y su sintaxis básica para usarla en *RMarkdown*.

### Para minería de texto en R

Este curso está construido con base en siguientes materiales:

- [Silge & Robinson - Text Mining with R: a Tidy Approach](https://www.tidytextmining.com/index.html). 
  
- [Feinerer - Introduction to the tm Package Text Mining in R](https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf).

- [Holliger- Using the TM package](https://rpubs.com/tsholliger/301914).

- [Quanteda Iniciative - Quantieda Tutorials](https://tutorials.quanteda.io/).

- [Amat- Text mining con R: ejemplo práctico Twitter](https://rpubs.com/Joaquin_AR/334526).

- [Welbers, Van Atteveldt & Benoit - Text Analysis in R](https://kenbenoit.net/pdfs/text_analysis_in_R.pdf)




  
  
## Repositorios de bases de datos

Algunas bases de datos de texto que pueden usar.

- [Awesome Public Datasets](https://github.com/awesomedata/awesome-public-datasets)

## Calendario de sesiones

-  Sesión 1: Introducción a la tokenización y manipulación de cadenas de texto
  - Temas:
    - Breve repaso al uso de `tidyverse` yal IDE *RStudio*
    - ¿Qué es la minería de texto y por qué se dice que se trata de datos "no estructurados"?
    - En busca de la imperfección: ¿por qué la minería de texto es indefectiblemente imperfecta?
    - ¿Qué tipo de patrones buscamos en el llamado *lenguaje natural*?
    - ¿Qué es un *corpus* y por qué usarlo?
    - Breve introducción a las *expresiones regulares*
    - Búsquedas y filtros simples con `Base R` y con `stringr`
    - Manipulaciones simples con `Base R` y con `stringr`


  
- Sesión 2: Introducción al análisis de texto
  - Temas:
    - *N-gramas*: la relación entre palabras
    - Frecuencias de palabras: ¿cómo obtenerlas y qué tan útiles son?
    - La ley de Zipf o de cómo limpiar palabras "vacías"
    - La "importancia" de las palabras: ¿cómo obtener y para qué sirve el *tf-idf* de un texto?
    - Uso de visualización de datos bajo en enfoque de la "gramática de gráficas" (*gg*).


- Sesión 3: Análisis de sentimientos
  - Temas:
    - Diccionarios de sentimientos
    - Las limitaciones del análisis de sentimientos
    - Las limitaciones del análisis de sentimientos *en español* 
    - Gráficas de dispersión léxica
    - Visualización de nubes con sentimientos y otras geometrías.

  
- Sesión 4: Modelaje de tópicos
  - Temas:
    - Gentil introducción a la Distribución Latente de Dirichlet (*LDA*)
    - Clasificación de entidades por su categoría gramatical
    - Clasificación supervisada de palabras
    - Análisis de correspondencia

    
## Sobre el instructor

Mi nombre es Manuel Toral, actualmente trabajo con datos judiciales para el análisis de la política pública del Poder Judicial, fui investigador especializado en datos en *Mexicanos Contra la Corrupción y la Impunidad*. Estudié Política Pública en la *Escuela Harris de la Universidad de Chicago* y Ciencia Política y Relaciones Internacionales en el *Centro de Investigación y Docencia Económicas*. Como instuctor en `R`, soy parte del directorio de instructores de RStudio, [aquí puedes ver mi perfil](https://community.rstudio.com/u/jmtoral/summary).

Llevo 5 años usando `R` de manera profesional en una diversidad de proyectos de corte inmobiliario, análisis de grandes cantidades de datos, seguridad, justicia y, actualmente, en corrupción, transparencia y combate a la impunidad en México. Puedes ver algo de mis últimos trabajos de investigación con datos en el blog Desarmando la Corrupción de MCCI en alianza con la revista Nexos.

Como docente en `R`, he sido el experto residente de la Escuela Harris de la Universidad de Chicago, en la que dirgí la "STATA and R Bar", que asesoraba en el uso de estas herramientas a alumnos de maestría y doctorado. En 2017, fui contratado por la Unidad de Investigación Aplicada de MCCI para capacitar a sus integrantes en el uso de R, equipo al que finalmente [me integré](https://contralacorrupcion.mx/quienes-somos/) como investigador y del que fui parte hasta 2018. Actualmente, hago investigación con datos judiciales para instituciones públicas.