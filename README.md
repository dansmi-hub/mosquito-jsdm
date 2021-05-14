mosquito-jsdm
==============================

Generating JSDM for Mosquitoes using VectorNet data.

Project Organization
--------------------

    .
    ├── AUTHORS.md
    ├── LICENSE
    ├── README.md
    ├── bin -- Objects for the project. (Saved model files etc. Not included in commit by default because of size.
    ├── config -- Config files (Will inlcude R Package list in future and any dependencies needed).
    ├── data -- Not included in commit by defualt, though scripts included can generate much of it for you through downloads.
    │   ├── external -- Any external data in the project. Not included in git commit by defualt.
    │   ├── interim -- Any data currently being processed. Not included in git commit.
    │   ├── processed -- Processed ready to use data. 
    │   └── raw -- Any raw data needing to be cleaned and processed.
    |
    |           Docs / Reports / Notebook are all very similar. Will be organised more cleanly later. 
    |                       Though in essence they all contain reports or quick analysis.
    ├── docs
    ├── notebooks
    ├── reports
    │   └── figures
    └── src -- Here are where the scripts are stored.
        ├── data -- Any scripts to manage data (functions, etc).
        ├── external -- Any external scripts for the project (Code examples, etc).
        ├── models -- Modelling scripts and code.
        ├── tools -- Will move funcitons here to keep project directory clean.
        └── visualization -- Scripts to manage visulaisation of any results or processes.
