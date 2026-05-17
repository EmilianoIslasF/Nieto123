# Modelos Espaciales y Dinámicos para el Análisis de Desapariciones en México

Proyecto desarrollado para el curso de Métodos Lineales Generalizados (2026), enfocado en el análisis estadístico, espacial y temporal de desapariciones registradas en la Ciudad de México mediante modelos Poisson, modelos Bayesianos CAR y enfoques dinámicos.

---
# Autores

Proyecto desarrollado por:

- Andrés Padrón Quintana
- Daniel Miranda Badillo
- Emiliano Islas Flores
- Manuel Alonso De la Tejera González
---

## Descripción del proyecto

Las desapariciones representan uno de los fenómenos sociales y de seguridad más complejos en México. Su comportamiento presenta patrones espaciales y temporales que no pueden modelarse adecuadamente bajo supuestos de independencia entre regiones.

Este proyecto busca modelar la ocurrencia de desapariciones utilizando herramientas estadísticas avanzadas capaces de capturar:

- Dependencia espacial entre alcaldías vecinas.
- Persistencia temporal y tendencias dinámicas.
- Heterogeneidad geográfica no observada.
- Relación entre desapariciones y variables de criminalidad.

El análisis se centra principalmente en las 16 alcaldías de la Ciudad de México durante el periodo 2015–2025.

---

## Objetivos

### Objetivo general

Analizar el comportamiento espacial y temporal de las desapariciones registradas en la Ciudad de México mediante modelos estadísticos espaciales y dinámicos, incorporando variables relacionadas con criminalidad, violencia y características demográficas. :contentReference[oaicite:2]{index=2}

### Objetivos específicos

- Realizar análisis exploratorio espacial y temporal.
- Evaluar relaciones entre desapariciones y variables delictivas.
- Construir modelos Poisson clásicos.
- Implementar modelos espaciales Bayesianos tipo CAR.
- Analizar residuos y efectos espaciales aleatorios.
- Incorporar modelos dinámicos y de series de tiempo.
- Comparar desempeño entre modelos espaciales y no espaciales. 
---

## Fuente de datos

La información utilizada proviene principalmente del:

- Registro Nacional de Personas Desaparecidas y No Localizadas (RNPDNO)
- Comisión Nacional de Búsqueda (CNB)

Además, el proyecto utiliza información complementaria relacionada con:

- Homicidios
- Robos
- Violencia familiar
- Feminicidios
- Extorsión
- Variables demográficas
- Percepción de seguridad

La base espacial final contiene:

- 878 observaciones
- 33 variables
- Información mensual por alcaldía

Periodo analizado:

```text
2015 – 2025