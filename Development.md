## Testing

It is considered good practice that every (public) function in this package inclues its own set of unit tests. A bunch of them is collected in `Tests/Tests.wl` file, using the Mathematica testing [framework](https://reference.wolfram.com/language/guide/SystematicTestingAndVerification.html). It is strongly reccomended that tests are run periodically during development and especially before every commit. 

#### Integration of test in Git hook

Unit test can be run automatically before every commit via Git client-side [hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks). File `pre-commit` should contain call to `Tests/RunTests.wls` script, which exits with value 0 if all tests pass and aborts the commit otherwise. Minimal example of `pre-commit` file content is:

    #!/bin/sh
    ./Tests/RunTests.wls

## How to build MeshTools

#### Prerequisites
* Version 11.3 or greater of Mathematica
* [Wolfram Workbench](https://www.wolfram.com/workbench/)

#### Building MeshTools 
First, import MeshTools in Workbench:

* Select File -> Import...
* Git - Projects from Git (Next)
* Existing local repository (Next)
* Add... (browse to MeshTools, select, Next)
* Import as general project (Finish)

Importing of the MeshTools source needs to be done only once.


Next, build the documentation:
  
* In the MeshTools folder right click on docbuild.xml
* Choose Run As...
* Choose 2 Ant Build...
* Deselect all 
* Select *MeshTools*
* Run


This will create a folder named build, which will contain a folder MeshTools that contains the build documentation of package.

Copy source code and create and install the paclet:

* Open Build.nb
* Follow instructions to copy the source code
* Follow instructions to create and install the paclet

This will leave you with a path to a MeshTools-X.Y.Z.paclet in the build folder.
