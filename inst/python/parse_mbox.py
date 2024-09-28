import os
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed

# Define paths and number of threads for parallel processing
mbox_folder = "../../../save_mbox_mail"  # Folder containing .mbox files to process
output_folder = "../../../parsed_mbox_mail"  # Folder to store the output CSV files
r_script_path = "../../exec/parsembox.R"  # Path to the R script that processes .mbox files
tools_yml = "../../tools.yml"  # Path to the YAML configuration file for tools
num_threads = 4  # Number of threads to use for parallel processing

def run_r_parse_mbox(mbox_file: str) -> None:
    """
    Runs the R script to parse a single .mbox file and save the result as a CSV file.

    Args:
        mbox_file (str): The path to the .mbox file to process.
    
    Returns:
        None
    """
    # Construct the output file path by replacing '.mbox' with '.csv'
    output_file = os.path.join(output_folder, f"parsed_{os.path.basename(mbox_file).replace('.mbox', '.csv')}")

    # Command to execute the R script with the necessary arguments
    cmd = ["Rscript", r_script_path, "parse", tools_yml, mbox_file, output_file]

    # Run the command to execute the R script
    # Suppress output by redirecting stdout and stderr to DEVNULL
    subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def main() -> None:
    """
    Main function that coordinates the parallel processing of .mbox files.

    This function checks for the existence of required directories, lists
    all .mbox files in the folder, and processes them in parallel using
    ThreadPoolExecutor.

    Args:
        None

    Returns:
        None
    """
    # Check if the mbox folder exists; exit with an error message if it doesn't
    if not os.path.exists(mbox_folder):
        print(f"Error: Directory {mbox_folder} does not exist. Exiting.")
        return

    # Check if the output folder exists; create it if it doesn't
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    # List all .mbox files in the mbox folder by filtering for files ending with '.mbox'
    mbox_files = [os.path.join(mbox_folder, f) for f in os.listdir(mbox_folder) if f.endswith(".mbox")]

    # If no .mbox files are found, print an error message and exit
    if not mbox_files:
        print(f"No .mbox files found in {mbox_folder}. Please add files and try again.")
        return

    # Process the .mbox files in parallel using ThreadPoolExecutor
    # The number of threads is controlled by the `num_threads` variable
    with ThreadPoolExecutor(max_workers=num_threads) as executor:
        # Submit each mbox_file to the executor for processing
        futures = {executor.submit(run_r_parse_mbox, mbox_file): mbox_file for mbox_file in mbox_files}

        # Track the progress of each task as it completes
        for future in as_completed(futures):
            # Get the file associated with the future object
            mbox_file = futures[future]
            try:
                # Call future.result() to raise any exceptions encountered during execution
                future.result()
                # If successful, print a success message
                print(f"Parsed {mbox_file}")
            except Exception as exc:
                # If an exception occurred, print an error message with details
                print(f"✘ Failed to parse {mbox_file}: {exc}")


if __name__ == "__main__":
    # Call the main function to start processing the .mbox files
    main()
