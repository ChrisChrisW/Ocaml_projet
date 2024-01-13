# Propositional Logic Solver

## Overview

This project provides a Propositional Logic Solver implemented in OCaml. It supports the manipulation and evaluation of propositional logic formulas, generating decision trees and Binary Decision Diagrams (BDDs), and checking properties such as tautology and equivalence.

### Key Features

1. **Formula Representation:**
   - Formulas are represented using a custom type (`tformula`) supporting logical operations like conjunction, disjunction, implication, equivalence, negation, and variables.

2. **Decision Tree Generation:**
   - The project generates decision trees (`decTree`) from propositional logic formulas, aiding in visualizing the structure of logical expressions.

3. **BDD Construction and Simplification:**
   - Binary Decision Diagrams (BDDs) are constructed from formulas and then simplified to enhance efficiency.

4. **Tautology Checking:**
   - The solver includes functions to determine whether a given formula is a tautology.

5. **Equivalence Checking:**
   - It checks whether two propositional logic formulas are equivalent.

6. **DOT File Generation:**
   - The project can generate DOT files compatible with Graphviz for visualizing the constructed BDDs and decision trees.

### Usage

- The project is flexible and can be utilized for educational purposes, logic programming courses, or for anyone interested in exploring propositional logic.

### Prerequisites

- OCaml (version >= 4.10.0)
- Graphviz (for DOT file visualization)

### Installation

1. Clone the repository.
2. Compile the OCaml files using `make`.

### Examples

- Check the provided `examples.ml` file for usage examples and additional test cases.

### Visualization

- Visualize the generated DOT files using Graphviz. For example:
  ```bash
  dot -Tpng -o visualization.png bdd_visualization.dot
  ```
