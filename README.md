# simple-genetic-algorithm

Para la implementación del algoritmo genético simple escogí el lenguaje de programación Haskell. Haskell es un lenguaje funcional puro que se presta muy bien a este tipo de problemas por su expresividad lo que se traduce en código claro y conciso.

Aunque no es tan popular como algunos otros lenguajes como C, Python o Java, Haskell es un lenguaje de alto nivel que permite al programador expresarse de una manera muy declarativa lo que le da mucha flexibilidad al compilador para optimización y la generación de código de maquina que incluso puede competir con la velocidad de C.

Como un proyecto futuro sería interesante explorar las capacidades avanzadas de tipos como lo son los **tipos de datos algebraicos generalizados** y los **tipos liquidos** que posee Haskell para incorporar restricciones y estructuras específicas de soluciones difíciles de codificar con lenguajes imperativos tradicionales.

para correr el programa basta escoger la estrategia:

```
data Strategy = Strategy1 -- (m+l)
              | Strategy2 -- (m,l)
              | Strategy3 -- (m,l) + elitism
              deriving Eq
```

en main

```
main :: IO ()
main = run <escoger estrategia> algorithm problemData
```

También se pueden modificar los datos del algoritmo y del problema
cambiando los datos en problemData y algorithm

```
problemData = ProblemData (length gamma) f
algorithm   = Algorithm 0.9 (1/(fromIntegral (length gamma))) 100 100 -- change population size and num of generations
```

El número de bits se puede cambiar en gamma

```
gamma :: Genotype
gamma = [if even x then 0 else 1 | x <- [0,1..n] ]
  where n = 128 -- change bit count
```

El programa se compilo con la versión lts-17.14 (8.10.4) de GHC usando la herramienta Stack.

* Descargar Stack [Install/upgrade Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

* Como usar Stack [PRACTICAL HASKELL - GETTING STARTED WITH STACK](https://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html).