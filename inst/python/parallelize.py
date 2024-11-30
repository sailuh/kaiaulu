import os
import yaml
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed
import sys

def main():
    # Load configuration
    config_file = "config.yaml"
    if len(sys.argv) > 1:
        config_file = sys.argv[1]

    with open(config_file, "r") as file:
        config = yaml.safe_load(file)

    command_template = config["command_template"]
    exec_script = os.path.abspath(config["exec_script"])
    tools_yml = os.path.abspath(config["tools_yml"])
    conf_yml = os.path.abspath(config["conf_yml"])
    project_key = config["project_key"]
    input_dir = os.path.abspath(config.get("input_dir", ""))
    output_dir = os.path.abspath(config.get("output_dir", ""))
    num_threads = config.get("num_threads", 4)
    input_file_extension = config.get("input_file_extension", "")
    process_individual_files = config.get("process_individual_files", True)

    # Ensure output directory exists
    if output_dir and not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # If process_individual_files is True, process each input file
    if process_individual_files:
        if not os.path.exists(input_dir):
            print(f"Input directory {input_dir} does not exist.")
            exit(1)

        # Discover all input files
        input_files = [
            os.path.join(input_dir, f) for f in os.listdir(input_dir)
            if f.endswith(input_file_extension)
        ]

        if not input_files:
            print(f"No files found in the input directory with extension '{input_file_extension}'.")
            exit(0)
    else:
        # No input files to process
        input_files = [None]

    def run_command(input_file):
        """
        Runs the specified command for a single input file.
        """
        # Generate output file name
        if input_file:
            base_name = os.path.splitext(os.path.basename(input_file))[0]
            output_file = os.path.join(output_dir, f"parsed_{base_name}.csv")
        else:
            output_file = os.path.join(output_dir, "output.csv") if output_dir else "output.csv"

        # Prepare command by replacing placeholders
        cmd = [arg.format(
            exec_script=exec_script,
            command=config.get("command", ""),
            tools_yml=tools_yml,
            conf_yml=conf_yml,
            project_key=project_key,
            output_file=output_file,
            input_file=input_file if input_file else "",
        ) for arg in command_template]

        # Remove empty arguments (e.g., if input_file is None)
        cmd = [arg for arg in cmd if arg != ""]

        try:
            subprocess.run(cmd, check=True)
            if input_file:
                print(f"✔ Successfully processed {input_file}")
            else:
                print("✔ Command executed successfully")
        except subprocess.CalledProcessError as e:
            if input_file:
                print(f"✘ Failed to process {input_file}: {e}")
            else:
                print(f"✘ Command execution failed: {e}")

    # Run jobs in parallel
    if process_individual_files:
        with ThreadPoolExecutor(max_workers=num_threads) as executor:
            futures = {executor.submit(run_command, f): f for f in input_files}
            for future in as_completed(futures):
                file = futures[future]
                try:
                    future.result()
                except Exception as e:
                    print(f"Error processing {file}: {e}")
    else:
        # Single execution
        run_command(None)

if __name__ == "__main__":
    main()
