#!/bin/bash

# Batch script to run all hetid analysis scripts
# This script can be executed from the command line

echo "=========================================="
echo "Running hetid Analysis Pipeline"
echo "Started at: $(date)"
echo "=========================================="

# Change to the scripts directory
cd "$(dirname "$0")"

# Run the main R script
Rscript run_all_scripts.R

echo "=========================================="
echo "Pipeline completed at: $(date)"
echo "=========================================="
