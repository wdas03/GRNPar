# GRNPar
Parallel implementation of inferring gene regulatory networks using an information theoretic approach with boolean networks. Algorithm adopted from Barman & Kwon: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0171097 using mutual information-based feature selection (MIFS).

## Setup
- To generate output images, graphviz must be installed on local machine:
  
    `brew install graphviz`

    OR

    `sudo apt install graphviz`
- Running `dot -v` should output something like:

    `dot - graphviz version 7.0.4 (20221203.1631)`

## Running Executables
From GRNPar directory, run: 

`stack setup`

`stack build`

To run the executable, run:
`stack exec grnPAR-exe <csvFilename> <k> <outputFile> <mode>`

- csvFilename: gene-expression time-series data
- k: fixed number of input nodes for each target node
- outputFile: output png filename for generated boolean network
- mode: "seq" or "par" (seq for sequential algorithm and par for parallel implementation)

Example: 

`stack exec grnPAR-exe "src/data/nodes_100_time_300.csv" "./src/output_files/nodes_100_time_300" 3 par -- +RTS -N8 -lf -s -threaded`

`threadscope GRNPar-exe.eventlog`

