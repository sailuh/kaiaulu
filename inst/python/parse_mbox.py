import os
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed

# Dynamically resolve paths relative to the current script
script_dir = os.path.dirname(os.path.abspath(__file__))

mbox_folder = os.path.abspath(os.path.join(script_dir, "../../../rawdata/helix/mod_mbox/save_mbox_mail"))
output_folder = os.path.abspath(os.path.join(script_dir, "../../../rawdata/helix/mod_mbox/parsed_mbox_mail"))
r_script_path = os.path.abspath(os.path.join(script_dir, "../../exec/mailinglist.R"))
tools_yml = os.path.abspath(os.path.join(script_dir, "../../tools.yml"))
conf_yml = os.path.abspath(os.path.join(script_dir, "../../conf/helix.yml"))

project_key = "project_key_1"
num_threads = 4

def run_r_parse_mbox(mbox_file: str) -> None:
    """
    Runs the R script to parse a single .mbox file and save the result as a CSV file.
    """
    output_file = os.path.join(output_folder, f"parsed_{os.path.basename(mbox_file).replace('.mbox', '.csv')}")
    cmd = ["Rscript", r_script_path, "parse", tools_yml, conf_yml, project_key, output_file]

    try:
        subprocess.run(cmd, check=True)
        print(f"✔ Successfully parsed {mbox_file}")
    except subprocess.CalledProcessError as e:
        print(f"✘ Failed to parse {mbox_file}: {e}")

def main() -> None:
    """Main function to run the parsing in parallel."""
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    mbox_files = [os.path.join(mbox_folder, f) for f in os.listdir(mbox_folder) if f.endswith(".mbox")]

    if not mbox_files:
        print("No .mbox files to process.")
        return

    with ThreadPoolExecutor(max_workers=num_threads) as executor:
        futures = {executor.submit(run_r_parse_mbox, mbox): mbox for mbox in mbox_files}

        for future in as_completed(futures):
            mbox_file = futures[future]
            try:
                future.result()
            except Exception as e:
                print(f"Error processing {mbox_file}: {e}")

if __name__ == "__main__":
    main()
