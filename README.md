# OCaml Project - Ford Fulkerson implementation

## Running the project

### Run the algorithm

Build the program

`ocamlbuild ftest.native`

then run

`./ftest.native <source_file> <source_node> <sink_node> <output_file>`

### Run the example use case - circulation/demand problem

Build the program

`ocamlbuild ftestRoads.native`

then run

`./ftestRoads.native <source_file> <output_file>`

The program will output to the terminal and append a comment to the resolved graph to indicate if all the needs of the villages are met.

The provided file should have the following format:

```text
v <village_name> <village_needs_as_int>

f <factory_name> <factory_production_as_int>

r <factory_name> <village_name> <road_capacity_as_int>
```

You can specify as much villages, factory and roads as you want, however roads should be declared after villages and factories

### Output a viewvable png from a gv file

`dot -Tpng -O <filename>`

## Example use case explanation

The goal is to check if the needs of the different villages can be met by carrying the ressources produced by the factories on the different roads (that each have a max capacity).

## Test cases

Test cases for the algorithm and the example are available in `./tests`.