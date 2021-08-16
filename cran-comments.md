## Test environments
* local Linux install, R 4.1.0
* Github Action: MacOS-latest (Mac OS X 10.15.7)
* win-builder (release and devel)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE

This is a new submission -- first resubmission after comments by CRAN reviewer

## Reviewer Comments

### Comment 1
Please add \value to .Rd files regarding exported methods and explain the
functions results in the documentation. Please write about the structure of the
output (class) and also what the output means. (If a function does not return a
value, please document that too, e.g. \value{No return value, called for side
effects} or similar)

Missing Rd-tags:
    * gg_to_npc.ggtikzCanvas.Rd: \value
    * ggtikz.Rd: \value

### Response 1
Thank you for checking the submission!

The .Rd files specified above have been updated with the missing \value tags as
requested.
