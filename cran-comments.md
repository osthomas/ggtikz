This is a maintenance release to 0.1.3.

It addresses test failures due to changes in a required upstream package and
updates the maintainer's contact information.

## Test environments
* local Linux install, R 4.3.3
* Github Actions:
    - MacOS-latest (R release)
    - windows-latest (R release)
    - ubuntu-latest (R release)
    - ubuntu-latest (R devel)
    - ubuntu-latest (R oldrel-1)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

A previous version of ggtikz was removed from CRAN due to an expired
institutional email address. This has been fixed.
