# morphosyn-alternation-largescale

This is the GitHub repository for my current project on a large-scale approach to multifactorial analysis in morphosyntactic alternations.

The files are supposed to be run in order (1-extract.py is the first to be run, and so on.)

There are currently five steps in the process (the files at each process are subject to change):

-Step 1: Extract the data from the treebank and turn them into a Python-readable form
-Step 2: Extract clausal information from the treebank, focusing on within-sentence features
-Step 2.5-3: Extract clausal information from the treebank, focusing on inter-sentence features
-Step 4: Manually correct errors in automatic extraction
-Step 5: Encode and analyse data

The code files are those with the step ID at the beginning. In addition, there are CSV files generated from those code files. Currently, 'dec25table.csv' is used as the main source of data in Step 5. Correction files are those containing extra columns which correct the values of the automatically-extracted columns of steps 2-3; the currently used correction files are:

-dec22-table-withintersentence-modified-3000clauses.csv


Attributions:

Universal Dependencies - English gold standard treebank

https://github.com/UniversalDependencies/UD_English-EWT

Syllable count:

https://github.com/eaydin/sylco