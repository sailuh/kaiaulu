I considered using an old branch of Helix before to test parsing Depends/Understand java code, but I will instead create an example function that will make use of java code in order to test the tools and Kaiaulu’s own code. 

# Example
* Create an example function that generates multiple java files, as well as some R files, with function calls between the java files and R files. This will allow me to fully test all of Understand/scitools, Depends, and Kaiaulu’s own R dependency parsers. Function calls will also be called multiple times in order to properly test the weight column in the transforms. 
* Also create another example function that serves as a "correct output" for the exporter function. This will be a .xml file which will be compared to the output the exporter function generates. If the function fails by the .xml file changing in any way, that would indicate that something about how the .udb database processes data changed as well. 

# Third Party Tools
## Depends
* `”testthat if Depends is specified on tools.yml, then --help returns the help message”`
* ~~testthat if Depends is specified on tools.yml, the Depends path is a jar file~~

## Understand/Scitools
* `”testthat if scitools is specified on tools.yml, then und help add at the path returns the help message”`
* ~~testthat if scitools is specified on tools.yml, then und license at the path returns a valid license~~

## utags
* `”testthat if utags is specified on tools.yml, then it returns the help message”`
* ~~testthat if utags is specified on tools.yml, the utags path is an executable file called ctags-universal~~

## RefactoringMiner
* `”testthat if RefactoringMiner is specified on tools.yml, then it returns the help message”`

## scc
* `”testthat if scc is specified on tools.yml, then it returns the help message”`

# Understand Project Builder
## build_understand_project
* `”testthat calling build_understand_project with valid arguments returns a path to a .und database”`
* `"testthat given an incorrect path to scitools, the function will display an error message informing the user"` 

## export_understand_dependencies
* `”testthat calling export_understand_dependencies with valid arguments and parse_type = file returns a path to a .xml file”`
* `”testthat calling export_understand_dependencies with valid arguments and parse_type = class returns a path to a .xml file”`
* `”testthat the generated .xml file at the path exactly matches the .xml format example”`
* `”testthat calling export_understand_dependencies with an invalid scitools_path returns an error in running command warning”`
* `”testthat calling export_understand_dependencies with an invalid scitools_path returns a warning about an error in running command”`
* `”testthat calling export_understand_dependencies with an invalid parse_type returns an error about invalid parse_type”`

# Parsers
## parse_understand_dependencies
* `”testthat given valid XML file the function returns two data tables”`
* `”testthat the function returns the correct amount of nodes in node_list”`
* `”testthat the function returns the correct amount of edges in edge_list”`
* `”testthat the function has no repeating ids in its id row in node_list”`
* `”testthat in the function’s edge_list, the label_from row arguments correctly correspond to the label_to row arguments”`
* `”testthat in the function’s edge_list, the id_from row arguments correctly correspond to the id_to_row arguments”`
* `”testthat the function’s id_to row in edge_list matches the test data”`
* `”testthat the function’s dependency_kind row in edge_list matches the test data”`

## parse_dependencies
* `”testthat given a valid depends jar path and git repository containing java code, the function will return a nodes list and edgelist”`
* `”testthat when filtering the language for java, the function’s node list contains all correct filepaths containing java code”`
* `”testthat the returned edgelist’s src_filepath to dest_filepath relationships are correct”`
* `”testthat for a given src_filepath to dest_filepath pairing, the amount and type of dependencies connecting them is correct”`
* `”testthat 14 rows are created in the edgelist output”`

## parse_java_code_refactoring_json
I was unable to get this function to run correctly on my machine, and so I didn’t feel confident in making tests for it. I will keep trying to run it and create a discussion if I’m still stuck for a while. 

## parse_line_metrics
* `”testthat given a valid scc_path and git_repo_path, the function will return a data table”`
* `”testthat the Language row data correctly matches the Filename row for each file”`
* `”testthat given an invalid path to scc, the function will display an error message informing the user`”
* `”testhat given a git_repo_path with no .git folder, the function will display an error message informing the user”`
* `”testthat given a valid scc_path and git_repo_path, the function returns 11 rows in its data table

## parse_line_type_file
* `”testthat given a valid utags_path, path to file, and kinds, the function will return a data table”`
* `”testthat given an invalid utags_path, the function will return an informative error message”`
* `”testthat given an invalid kinds input, the function will return an informative error message”`
* `”testthat given valid arguments, the function’s returned data table with have 7 rows"`
* `”testthat the entity_name and entity_type rows contain the correct matching data”`
* `”testthat given an empty file, the function will create an empty table”`

## parse_r_dependencies
* `”testthat given a valid folder path to R files, the function will return a data table”`
* `”testthat given an invalid path to R files, the function will return an informative error message”`
* `”testthat given a folder path that contains no R files, the function will return an empty data table”`
* `”testthat given a folder path containing both R files and other files, the function will only return the dependencies between the R files in the data table”`
* `”testthat given a valid folder path to R files, the function will return a data table containing six rows”`
* `”testthat the src_function_caller_name row correctly identifies function calls from the src_function_call_name row”`
* `”testthat the src_function_caller_filename row correctly identifies function calls from the src_function_call_filename row”`
* `”testthat the function’s src_line_functions_call_start row accurately identifies function calls in the correct file”`
* `”testthat the function’s src_line_functions_call_end row accurately identifies function calls in the correct file”`

# Network Transform
## transform_understand_dependencies_to_network
* `”testthat given a nodes and edge_list data table parsed from understand, the function returns a nodes and edge_list data table of its own”`
* `”testthat if no weight_types are specified, the function returns its original parsed data arguments”`
* `”testthat if the function filters out all available edges, it will produce an error message saying that are none”`
* `”testthat the function’s edge_list only contains Call edges when call is specified in weight_types”`
* `”testthat specifying an invalid weight_type returns an informative error message”`

## transform_dependencies_to_network
* `”testthat given a nodes and edge_list data table parsed from depends, the function returns a nodes and edge_list data table of its own”`
* `”testthat if no weight_types are specified, the function returns an empty edgelist”`
* `”testthat the function’s edge_list only contains Call edges when call is specified in weight_types”`
* `”testthat the function’s edgelist’s weight column values correctly return the expected weight values”`

## transform_r_dependencies_to_network
* `”testthat given a valid r_dependencies_edgelist and valid dependency type, the function returns two data tables”`
* `”testthat setting the dependency_type to function returns all functions in the nodes data table”`
* `”testthat setting the dependency_type to file returns all files in the nodes data table”`
* `”testthat when the dependency_type is set to function, the edgelist data table correctly displays all function dependencies”`
* `”testthat when the dependency_type is set to file, the edgelist data table correctly correctly displays all file dependencies”` 
* `”testthat when the dependency_type is set to function, the weight row in the edgelist data table correctly displays the amount of function dependencies for each edge”`
* `”testthat when the dependency_type is set to file, the weight row in the edgelist data table correctly displays the amount of file dependencies for each edge”`
