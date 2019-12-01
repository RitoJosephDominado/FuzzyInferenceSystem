# FuzzyInferenceSystem
A Shiny GUI for creating and using fuzzy inference systems


### Quick demo using a saved fuzzy inference system
On the "Upload" module, click "Browse"
Select the JSON file in the "JSON" folder named "FIS health.json" then click "Upload"
Click "Evaluation" on the sidebar, then click on the "Upload"
Select the csv file in the "Inputs" folder named "FIS health - input.csv" then click "Upload"

Clicking on "Linguistc variables" and "Fuzzy rules" on the sidebar shows you the parts of the uploaded model
Clicking on "Save" on the sidebar shows you the JSON representation of the fuzzy inference system and allows you to save it



### Creating linguistic variables
Type the name of your linguistic variable, and the range of values for it, then click “Add”

A new box will appear where you’ll see an interface for adding fuzzy sets. Put a name, select a type of fuzzy set and then enter the parameters for them.


### Creating fuzzy rules
Select a type and then click “Add”.

There are three types: simple, intersection and union.

Simple fuzzy propositions just evaluate the membership of a feature to a particular linguistic variable’s fuzzy set. 

Intersections and unions can be composed of other fuzzy propositions. Intersections get the minimum of the memberships of it’s arguments, and unions get the maximum.

The three can all be negated with the “Negated” switch. This will result in their outputs being one minus the supposed membership.



### Warning
Uploading a second json file in the Upload module will cause errors.


### For development
1. Fix errors when uploading a second file. The desired behavior would have the previously uploaded file overwritten by the second. But while the HTML can easily be deleted, the previous callModule call for linguistic variables can't be undone. A work around would be use an incrementing name for the linguistic variable modules, but a lot of things have to be refactored.

2. More comprehensive exception handling with file uploads

3. Add examples and pictures in this README file

4. Add more fuzzy sets. Sigmoid functions will probably be added next

5. Add more fuzzy propositions. The product t-norm and its t-conorm will be added next
