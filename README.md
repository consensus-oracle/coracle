## Coracle

Welcome to Coracle, a consensus algorithm simulator for heterogeneous networks.

#### Installation

Compile from source as follows:

```
# get source & deps
git clone https://github.com/heidi-ann/coracle
opam install sexplib
cd coracle
# build
ocaml setup.ml -configure
ocaml setup.ml -build
ocaml setup.ml -install
```

#### Usage

We currently support two interfaces: command line implementation and simulation.

##### Implementation

Run an consensus algorithm as follows:
```
./cmdline.byte 1 --max=5
```
This instance will bind locally to port 5001 (5000+id) and will try to communicate with other instances at ports 5000 to 5004 (5000 to 5000+max-1).

##### Simulation

Ran a simulation as follows:
```
./simulator.byte -n 5
```
This will ran a simualator with 5 nodes.