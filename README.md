# Adversarial Search Haskell

> **Búsqueda adversarial en juegos implementada en Haskell con interfaz gráfica**

Un proyecto académico que implementa algoritmos de búsqueda adversarial (Minimax/Negamax con poda alfa-beta) aplicados a tres juegos clásicos, desarrollado completamente en Haskell con interfaz gráfica usando Gloss.

## 📋 Descripción

Este proyecto implementa algoritmos de inteligencia artificial para juegos de adversario, incluyendo:

- **Algoritmo Negamax** con poda alfa-beta
- **Búsqueda en reposo** para mejorar la evaluación
- **Múltiples niveles de dificultad** (aleatorio, fácil, medio, difícil)
- **Interfaz gráfica completa** con menús y opciones
- **Sistema de guardado/carga** de partidas

## 🎮 Juegos Implementados

### 1. **Tres en Raya (Tic-tac-toe)**
- Tablero 3x3 clásico
- Algoritmo optimizado para juego perfecto
- Detección automática de empates

### 2. **Ratón vs Gatos (Cats vs Mouse)**
- El ratón (blanco) debe escapar hacia arriba
- Los gatos (negros) deben bloquear al ratón
- Estrategia asimétrica con objetivos opuestos

### 3. **Damas Inglesas (Checkers)**
- Tablero 8x8 con reglas inglesas
- Promoción a dama al llegar al extremo
- Capturas obligatorias y múltiples

## 🚀 Características

### Algoritmos de IA
- **Negamax básico** con evaluación iterativa
- **Negamax con poda** alfa-beta para eficiencia
- **Negamax completo** con búsqueda en reposo
- **Evaluación heurística** específica por juego

### Interfaz Gráfica
- **Menús intuitivos** para selección de juegos
- **Configuración de dificultad** y opciones
- **Visualización en tiempo real** del estado del juego
- **Indicadores visuales** de turnos y movimientos válidos

### Sistema de Partidas
- **Guardado automático** de partidas
- **Carga de partidas** previas
- **Sistema de deshacer** movimiento
- **Archivos temporales** para recuperación

## 🛠️ Tecnologías

- **Lenguaje**: Haskell (GHC)
- **Gráficos**: Gloss
- **Matemáticas**: Data.Matrix
- **Concurrencia**: Control.Concurrent.Async
- **Sistema de archivos**: System.Directory

## 📦 Instalación

### Prerrequisitos
- GHC (Glasgow Haskell Compiler) 8.10+
- Cabal 3.0+
- Librerías del sistema para gráficos

### Ubuntu/Debian
```bash
sudo apt update
sudo apt install ghc cabal-install freeglut3-dev
```

### Windows
```bash
# Usar Stack o instalar Haskell Platform
stack setup
```

### Compilación
```bash
# Clonar el repositorio
git clone https://github.com/Jastxz/Adversarial-Search-Haskell.git
cd Adversarial-Search-Haskell

# Instalar dependencias
cabal update
cabal install --dependencies-only

# Compilar
cabal build

# Ejecutar
cabal run TFG
```

## 🎯 Uso

### Inicio del Programa
1. **Ejecutar** la aplicación
2. **Seleccionar** juego del menú principal
3. **Configurar** opciones (dificultad, turno, etc.)
4. **Comenzar** partida

### Controles
- **Click izquierdo**: Seleccionar casillas y opciones
- **Botones de interfaz**: Guardar, cargar, volver, opciones

### Niveles de Dificultad
| Nivel | Descripción | Algoritmo |
|-------|-------------|-----------|
| **Random** | Movimientos aleatorios | Aleatorio |
| **Lowest** | Muy fácil | Negamax profundidad 1-2 |
| **Easy** | Fácil | Negamax profundidad 2-3 |
| **Medium** | Medio | Negamax con poda |
| **Hard** | Difícil | Negamax completo |

## 📁 Estructura del Proyecto

```
Adversarial-Search-Haskell/
├── src/
│   ├── Tipos.hs              # Definiciones de tipos
│   ├── Utiles.hs             # Funciones auxiliares
│   ├── UtilesGraficos.hs     # Utilidades gráficas
│   ├── MiniMax.hs            # Algoritmos de búsqueda
│   ├── Interconexion.hs      # Interfaz entre juegos
│   ├── GuardarCargar.hs      # Sistema de persistencia
│   ├── Interaction.hs        # Controlador principal
│   ├── Funciones3enRaya.hs   # Lógica tres en raya
│   ├── FuncionesGato.hs      # Lógica ratón vs gatos
│   ├── FuncionesDamas.hs     # Lógica damas
│   ├── IO3enRaya.hs          # Interfaz tres en raya
│   ├── IOgato.hs             # Interfaz ratón vs gatos
│   └── IOdamas.hs            # Interfaz damas
├── app/
│   └── Main.hs               # Punto de entrada
├── Partidas/                 # Partidas guardadas
├── TFG.cabal                 # Configuración Cabal
└── README.md
```

## 🧠 Algoritmos Implementados

### Negamax con Poda Alfa-Beta
```haskell
negamaxConPoda :: Movimiento -> Int -> String -> String -> Double -> Double -> IO TableroPuntuado
negamaxConPoda (estado, pos) profundidad marcaMaquina juego alfa beta
```

### Funciones de Evaluación

#### Tres en Raya
- Detección de tres en línea: **±10 puntos**
- Bloqueo de victoria: **7.5 puntos**
- Dos en línea: **5 puntos**

#### Ratón vs Gatos
- Escape del ratón: **±30 puntos**
- Progreso hacia meta: **1.25 × fila**
- Coordinación de gatos: **penalización por dispersión**

#### Damas
- Valor de piezas: **Dama 9.4, Reina 10.5**
- Ataques/defensas: **±1.0/0.5 puntos**
- Control territorial: **2.9 puntos**

## 📊 Rendimiento

### Benchmarks (Intel i5-8250U)
| Juego | Profundidad | Tiempo Promedio | Nodos Evaluados |
|-------|-------------|-----------------|-----------------|
| 3 en Raya | 9 | ~50ms | ~5,000 |
| Ratón vs Gatos | 6 | ~200ms | ~15,000 |
| Damas | 4 | ~800ms | ~50,000 |

### Optimizaciones
- **Poda alfa-beta**: Reduce ~75% nodos evaluados
- **Búsqueda en reposo**: Evita horizontes problemáticos
- **Concurrencia**: Evaluación paralela de movimientos
- **Aleatorización**: Selección entre movimientos equivalentes

## 🐛 Problemas Conocidos

- **Encoding**: La interfaz está en inglés por limitaciones de Gloss con acentos
- **Rendimiento**: Damas en dificultad máxima puede ser lenta
- **Windows**: Requiere DLL adicionales (incluidas en release)

## 🔧 Desarrollo

### Agregar Nuevo Juego
1. Crear módulo `FuncionesNuevoJuego.hs`
2. Implementar funciones requeridas:
   ```haskell
   inicial :: Movimiento
   finJuego :: Tablero -> Bool
   movsJuego :: Tablero -> String -> Movimientos
   puntuaJuego :: Tablero -> Pos -> IO Double
   ```
3. Crear interfaz en `IONuevoJuego.hs`
4. Registrar en `Interconexion.hs`

## 📝 Licencia

Este proyecto no especifica licencia explícita. Para uso académico y educativo.

## 👨‍💻 Autor

**Javier Gil Blázquez**
- Email: javicraft14@gmail.com
- GitHub: [@Jastxz](https://github.com/Jastxz)

## 🙏 Agradecimientos

- **Universidad** por el marco académico del TFG
- **Comunidad Haskell** por las excelentes librerías
- **Gloss** por hacer gráficos accesibles en Haskell

## 📚 Referencias

- Russell, S. & Norvig, P. (2020). *Artificial Intelligence: A Modern Approach*
- Knuth, D. & Moore, R. (1975). *An Analysis of Alpha-Beta Pruning*
- Wikipedia: [Negamax Algorithm](https://en.wikipedia.org/wiki/Negamax)

---

⭐ **¡Si te ha gustado el proyecto, dale una estrella!** ⭐
