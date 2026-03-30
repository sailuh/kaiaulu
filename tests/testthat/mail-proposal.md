# Third Party Tools
## Perceval
* `”testthat if Perceval is specified on tools.yml, then it returns the help message”`
* `”testthat the Perceval path points to a folder called perceval”`

# Downloader Pipermail
## Download Pipermail
* `”testthat given valid arguments, the function returns the folder path to where the downloaded pipermail files are stored”`
* `”testthat if the mailing_list URL does not end with slash character, the function still runs as if the slash were there”`
* `”testthat if the GET request sent to the mailing list URL receives a response code that is not 200, function will error with “failed to access the mailing list page.””`
* `”testthat if the date for a month file from the mailing list URL cannot be converted into a valid date object, the function will send a warning “Date could not be parsed: ””`
* `”testthat if a cleaned date for a month file from the mailing list URL has no link for its data from its website, the function will send a warning “No link found in row for date: ””`
* `”testthat the function will only download pipermail information from the months specified in its arguments”`

* `”testthat if the date parsed from the file’s base name does not exist or is NA, the function will send a warning "Could not parse date from link: "”`
* `”testthat if the function attempts to download the .txt file for a month and fails, it will fallback to the .gz version”`
* `”testthat if the function attempts to both the download the .txt and .gz file for a month and fails, it will produce a warning and specify the failed month and its year”`

* `”testthat the function will print its diagnostic info if the verbose argument is set to TRUE”`
* `”testthat the function will not print its diagnostic info if the verbose argument is set to FALSE”`

* `”testthat the function downloaded correct pipermail information inside a given month”`
* `”testthat the function saves the downloaded pipermail information to save_file_path”`

* `”testthat if a month has failed, the function will return a warning displaying which months could not be downloaded”`
* `”testthat if the verbose argument is set to TRUE, the function will print the minimum date to maximum date of the mail it downloaded”`
* `”testthat if the verbose argument is set to TRUE and there are no downloaded files, the function will print that there are no files found”`
* `”testthat if the verbose argument is set to TRUE and there are months missing in the date range, the function will print the missing months”`

* `”testthat the function’s returned filepath contains all the downloaded .mbox files between start_year_month and end_year_month`”

## Refresh Pipermail
* `”testthat given valid arguments, the function returns a vector of the newly downloaded pipermail files”`
* `”testthat if the folder path specified is empty and start_year_month is not specified, the function throws an error saying it has no existing data“`
* `”testthat if the folder path specified is empty and start_year_month is specified, the function will download all pipermail files starting from that month until the most recent month“`
* `”testthat the function will delete the most recent month file before redownloading it”`
* `”testthat if the verbose argument is set to TRUE the function will about the most recent deleted file”`
* `”testthat if the function will download month files up to the current date”`
* `”testthat if the verbose argument is set to TRUE the function will print that it is processing .gz files to .mbox files after the refresh”`

## Process gz to mbox in folder
* `”testthat if given valid arguments, the function returns a list of created or updated .mbox files it changed from the .gz file format"`
* `”testthat if the verbose argument is set to TRUE and there are no .gz files at the specified folder path, print that the folder does not contain any .gz files”`
* `”testthat if there are no .gz files at the specified folder path, the function returns NULL”`
* `”testthat if the verbose argument is set to TRUE, the function will display the respective .gz files it is unzipping”`
* `”testthat all the original .gz files are deleted at the specified folder path”`
* `”testthat all the original .gz files are replaced by .mbox files at the specified folder path”`
* `”testthat the function returns the vector of the created or updated .mbox files”`
* `”testthat the function does not create duplicate files for the same month given a start date behind the most updated month in the files”`

# Downloader Mod Mbox
## Download Mod Mbox
* `”testthat given valid arguments, the function returns the folder path to where the downloaded mbox files are stored”`
* `”testthat if the verbose argument is set to TRUE, the mailing list name printed matches the actual list name in the URL"`
* `”testthat if the verbose argument is set to TRUE, the constructed URLs for each year and month for each file and where they are being saved is printed”`
* `”testthat the function only iterates through years and months between start_year_month and end_year_month"`
* `”testthat if the verbose argument is set to TRUE, successful downloads will be printed with their download URL”`
* `”testthat if the verbose argument is set to TRUE, a failed download will be printed with its respective status code”`
* `”testthat the name(YYYYMM.mbox) of a file saved to the specified file path matches the mail data for its respective name"`
* `”testthat any files that failed to download from their URL are deleted and no longer exist at the specified folder path”`
* `”testthat if a month has failed, the function will return a warning displaying which months could not be downloaded”`
* `”testthat if the verbose argument is set to TRUE and files were downloaded, the function prints the correct mail from the minimum to maximum dates”`
* `”testthat if the verbose argument is set to TRUE and no files were downloaded, the function prints that there were no files found at the specified file path”`
* `”testthat if the verbose argument is set to TRUE and no missing files, the function prints that there are no missing months”`
* `”testthat if the there are missing files, the function will produce a warning specifiying the months missing in the date range”`

* `”testthat the function returns the folder path where the mbox files are saved”`
* `”testthat all downloaded mbox files are saved at the save_folder_path”`

## Refresh Mod Mbox
* `”testthat given valid arguments, the function returns a vector of the newly downloaded mbox mail files”`
* `”testthat if the folder path specified is empty and start_year_month is not specified, the function throws an error saying it has no existing data and to specify a starting date“`
* `”testthat if the folder path specified is empty, the function will download mod_mbox files starting from the date specified”`
* `”testthat if the verbose argument is set to TRUE and the folder path is empty, the function will print that it is empty”`
* `”testthat if the verbose argument is set to TRUE and the folder path is empty, the function will print the starting dates and ending dates of mod_mbox files it downloading”`
* `”testthat if the specified folder is not empty, the most recent month is deleted”`
* `”testthat if the specified folder is not empty and if the verbose argument is set to TRUE,  the function will print the most recent file deleted”`
* `”testthat if the specified folder is not empty, the most recent month is redownloaded`”
* `”testthat the function redownloaded the files from the specified start date up to the most current month`”
* `”testthat the function does not create duplicate files for the same month given a start date behind the most updated month in the files”`

# Parsers
## Parse Mbox
* `”testthat calling parse_mbox with correct perceval and mbox path returns a data table”`
* `”testthat incorrect Perceval path returns an error stating Perceval execution failed”`
* `”testthat if there are no valid JSON lines, the function returns an error stating to check the mbox files or Perceval configuration"`
* `”testthat if there is an error during JSON parsing, the function will catch it and return the respective error”`
* `”testthat incorrect mbox path to parse_mbox returns empty table”`
* `”testthat parse_mbox will parse data correctly even if it is missing columns of interest”`
* `”testthat parse_mbox correctly renames the correct columns which correspond to its columns of interest”`

## Parse Mbox Latest Date
* `”testthat the function returns the name of the latest mbox file”`
* `”testthat if the specified save_folder_path doesn’t exist, the function displays an informative error message”`
* `”testthat if there are no .mbox files in the specified folder, the function returns NULL”`
* `”testthat the file returned has the latest date of all files in the folder”`
* `”testthat the function ignores non-numeric file names”`
