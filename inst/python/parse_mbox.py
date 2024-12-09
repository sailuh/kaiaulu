import os
import yaml
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed
import sys

def run_r_parse_mbox(input_file_rel):
    # Generate output file name relative to output_dir
    base_name = os.path.splitext(os.path.basename(input_file_rel))[0]
    output_file_rel = os.path.join(output_dir, f"parsed_{base_name}.csv")
    
    # Prepare command by replacing placeholders
    cmd = [arg.format(
        exec_script=exec_script,
        tools_yml=tools_yml,
        input_file=input_file_rel,
        output_file=output_file_rel,
    ) for arg in command_template]
    
    try:
        subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
        print(f"✔ Successfully parsed {input_file_rel}")
    except subprocess.CalledProcessError as e:
        print(f"✘ Failed to parse {input_file_rel}: {e}")

def main():
    global exec_script, tools_yml, command_template, input_dir, output_dir

    # Load configuration
    config_file = "config.yaml"
    if len(sys.argv) > 1:
        config_file = sys.argv[1]

    with open(config_file, "r") as file:
        config = yaml.safe_load(file)
    
    # Extract parameters from config
    command_template = config["command_template"]
    exec_script = config["exec_script"]
    tools_yml = config["tools_yml"]
    input_dir = config["input_dir"]
    output_dir = config["output_dir"]
    num_threads = config.get("num_threads", 4)
    input_file_extension = config.get("input_file_extension", ".mbox")

    # Ensure output directory exists
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # Discover all input files
    mbox_files = [
        os.path.join(input_dir, f) for f in os.listdir(input_dir) if f.endswith(input_file_extension)
    ]

    if not mbox_files:
        print(f"No files found in the input directory with extension '{input_file_extension}'.")
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
