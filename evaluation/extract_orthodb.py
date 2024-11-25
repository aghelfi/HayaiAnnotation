# Script Name: extract_orthodb.py
# Purpose: Generate UniProt accession - OrthoDB ID from UniProt .dat files
# Author: Andrea Ghelfi
# Date: November 20, 2024
# License: GNU GPL-3.0 License
# Software: Python 3.10
# Usage: python extract_orthodb.py

import re

def extract_orthodb_with_na(input_file, output_file):
    """
    Extracts accession IDs and OrthoDB IDs from a UniProt .dat file, adding NA for accessions without OrthoDB IDs.
    
    Args:
        input_file (str): Path to the input .dat file.
        output_file (str): Path to the output .txt file to save results.
    """
    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        accession_id = None  # Placeholder for the current accession ID
        ortho_db_id = "NA"  # Default to NA for accessions without OrthoDB
        for line in infile:
            # Check if the line contains an accession ID (AC)
            if line.startswith("AC"):
                # Write the previous accession (if exists) before moving to the next
                if accession_id is not None:
                    outfile.write(f"{accession_id}\t{ortho_db_id}\n")
                # Update to the new accession ID
                accession_id = line.split()[1].strip(";")
                ortho_db_id = "NA"  # Reset OrthoDB ID to NA for the new accession

            # Check if the line contains an OrthoDB ID (DR   OrthoDB;)
            elif re.match(r"^DR\s+OrthoDB;", line):
                ortho_db_id = line.split(";")[1].strip()  # Extract the OrthoDB ID

        # Write the last accession if it exists
        if accession_id is not None:
            outfile.write(f"{accession_id}\t{ortho_db_id}\n")

# File paths
input_files = [
    ("uniprot_sprot_plants.dat", "zen_odb_v11/sp_acc2orthodb.txt"),
    ("uniprot_trembl_plants.dat", "zen_odb_v11/tr_acc2orthodb.txt")
]

# Run the extraction for both input files
for input_file, output_file in input_files:
    extract_orthodb_with_na(input_file, output_file)
    print(f"Processed '{input_file}' and saved results to '{output_file}'.")
