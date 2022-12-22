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

### GRNPar Executable
From GRNPar directory, run: 

> `stack setup`
> 
> `stack build`

To run the executable, run:

> `stack exec GRNPar-exe <csvFilename> <k> <genExpressions> <genImage> <mode>`

- _csvFilename_: gene-expression time-series data
- _k_: fixed number of input nodes for each target node
- _genExpressions_: whether or not to generate boolean expressions for each node (1 == True, 0 = False)
- _genImage_: whether or not to generate image of boolean network (1 == True, 0 = False)
  - If 1, then outputFile will be equal to csvFilename with png extension
- _mode_: "seq" or "par" (seq for sequential algorithm and par for parallel implementation)

### Example
To run parallel implementation on 4 cores, and only generate an image:
  
> `stack exec GRNPar-exe "src/data/nodes_100_time_300.csv" 3 0 1 par -- +RTS -ls -N4`
>
> `threadscope GRNPar-exe.eventlog`

To run on E. coli dataset on 8 cores with _k_ = 4:

> `stack exec GRNPar-exe "src/data/e_coli.csv" 4 0 1 par -- +RTS -ls -N8`
> 
> `threadscope GRNPar-exe.eventlog`

### Generating Random Gene Expression Data
_Requires Python and pandas._

From GRNPar, run:

> `python src/generate_data.py --numNodes 100 --time 300 --outputFile "src/data/nodes_100_time_300.csv"`

- Creates a random gene expression time-series consisting of 100 genes (nodes) with 300 timesteps.