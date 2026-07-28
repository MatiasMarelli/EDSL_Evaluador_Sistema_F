# SF — Evaluador de Sistema F

Intérprete interactivo de **Sistema F** (cálculo lambda polimórfico).

## Cómo ejecutarlo

```bash
stack run
```
Esto abrira una consola en la cual podremos ejecutar diversos comandos.
## Comandos básicos

| Comando | Qué hace |
|---|---|
| `:c archivo.lam` | Carga un archivo con definiciones |
| `:t <expresión>` | Imprime el tipo que se infiere de la expresión dada |
| `:e <expresión>` | Evalúa una expresión y muestra el resultado |
| `:a <expresión>` | Muestra el AST de una expresión |
| `:b <nombre>` | Muestra el valor o tipo de una variable cargada |
| `:p <expresión>` | Imprime una expresión |
| `:s` | Muestra las variables actualmente cargadas |
| `:clear` | Borra todas las definiciones cargadas |
| `:q` | Salir |
| `:h` | Muestra la ayuda |

Una parte clave para un funcionamiento comodo es tener un archivo con definiciones previas.
## Ejemplo rápido
Los archivos deben ser definidos dentro de el directorio Files. Un archivo `.lam` se escribe así:

```
def Nat := 4 N . ((N -> N) -> N -> N)

def zero :: Nat
def zero = /\ N. \s: N->N. \z:N. z

def suc :: Nat -> Nat
def suc = \n:Nat. /\N. \s:N -> N. \z:N. s ((n <N>) s z)

def tres :: Nat
def tres = suc (suc (suc zero))
```

Uso de la consola consola:

```
SF> :c naturales
Variables loaded: 4
SF> :e tres
/\X. \x:X -> X. \y:X. x
                       (/\Y. \z:Y -> Y. \a:Y. z
                                               (/\Z. \b:Z -> Z. \c:Z. b
                                                                       (/\A. \d:A -> A. \e:A. e <Z>
                                                                         b
                                                                         c) <Y>
                                                 z
                                                 a) <X>
                         x
                         y)
SF> :s
["Nat","zero","suc","tres"]
SF> :q
```

