---
title: "Navegador de complementos"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ffbf087ea102e00b7057bf6bad9b6e2cb8f75ad05c7f26f0f2818d10f34392ce
---
El navegador de complementos le permite explorar el sistema de menús y ver dónde están instalados complementos específicos.

## Abrir el navegador de complementos

Vaya a **Ayuda → Programación → Navegador de complementos**.

## Lo que muestra

Mientras que el Explorador de procedimientos se centra en las *funciones* sin procesar en el PDB, el Explorador de complementos es una vista de subconjunto centrada en el descubrimiento de la interfaz de usuario. Filtra específicamente el PDB para mostrar "cosas que parecen complementos instalados en el menú".

Internamente, esto utiliza una consulta que solo devuelve procedimientos que tienen un archivo asociado en el disco y una ruta de menú registrada.

- **Árbol de menú**: muestra una representación en árbol de la estructura del menú Lumi.
- **Ubicaciones de complementos**: le ayuda a encontrar dónde se ha anidado un complemento recién instalado en los menús.
- **Metadatos**: muestra información sobre el autor, la versión y la fecha del complemento.

## Uso

Utilice el Explorador de complementos cuando sepa que existe una función pero no pueda encontrarla en los menús, o cuando esté diseñando su propio complemento y desee ver dónde se encuentran herramientas similares.
