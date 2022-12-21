# Generate large test samples
"""
Main script for generating random large datasets of input/output gene expression samples
"""

import random
import pandas as pd
import numpy as np
import argparse

def generate_io_pairs(num_nodes, time):
    nodes = ["v" + str(i) for i in range(1, num_nodes + 1)] + ["v" + str(i) + "'" for i in range(1, num_nodes + 1)]
    state_series = []

    inp_seen = []
    out_seen = []

    for t in range(1, time + 1):
        rand_states_inp = [random.randint(0, 1) for _ in range(num_nodes)]
        rand_states_out = [random.randint(0, 1) for _ in range(num_nodes)]

        inp_done = False
        out_done = False

        while not (inp_done and out_done):
            if rand_states_inp not in inp_seen:
                inp_seen.append(rand_states_inp)
                inp_done = True
            else:
                rand_states_inp = [random.randint(0, 1) for _ in range(num_nodes)]

            if rand_states_out not in out_seen:
                out_seen.append(rand_states_out)
                out_done = True
            else:
                rand_states_out = [random.randint(0, 1) for _ in range(num_nodes)]

        state_series.append(rand_states_inp + rand_states_out)
    
    state_series_df = pd.DataFrame(state_series, columns = nodes)
    return state_series_df


def generate_data_time_series(num_nodes, time):
    nodes = ["v" + str(i) for i in range(1, num_nodes + 1)]
    state_series = []
    states_seen = []
    for t in range(1, time + 1):
        rand_states = [random.randint(0, 1) for _ in range(len(nodes))]
        while True:
            if rand_states not in states_seen:
                states_seen.append(rand_states)
                break
            else:
                rand_states = [random.randint(0, 1) for _ in range(len(nodes))]
            
        state_series.append([t] + rand_states)
    
    state_series_df = pd.DataFrame(state_series, columns = ["time"] + nodes)
    return state_series_df

def output_data_to_file(state_series_df, fname):
    state_series_df.to_csv(fname, index=False)
    print("Printed data to {}".format(fname))

def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("--numNodes", type=int, help="Number of nodes")
    parser.add_argument("--time", type=int, help="Length of time-series")
    parser.add_argument("--outputFile", help="Output file")

    args = parser.parse_args()
    
    return args

# Example: python generate_data.py --numNodes 100 --time 300 --outputFile "test.csv"
if __name__ == "__main__":
    args = parse_args()
    data = generate_data_time_series(args.numNodes, args.time)
    print(data)
    output_data_to_file(data, args.outputFile)
    