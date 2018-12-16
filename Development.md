## Testing

It is considered good practice that every (public) function in this package inclues its own set of unit tests. 
A bunch of them is collected in `Tests/Tests.wl` file, using the Mathematica 
testing [framework](https://reference.wolfram.com/language/guide/SystematicTestingAndVerification.html). 
It is reccomended that you run them periodically during development and especially before every commit. 
This can be done by calling script file `Tests/RunTests.wls` in command line (first change directory to project root directory) 
or by evaluating whole notebook `Tests/RunTests.nb`.

#### Integration of tests in Git hook

Unit test can be run automatically before every commit via Git client-side [hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks). File `pre-commit` should contain call to `Tests/RunTests.wls` script, which exits with value 0 if all tests pass and aborts the commit otherwise. Minimal example of `pre-commit` file content is:

    #!/bin/sh
    ./Tests/RunTests.wls

## How to build MeshTools paclet

#### Prerequisites
* [Mathematica](https://www.wolfram.com/mathematica/) version 11.1 or later
* [Wolfram Workbench](https://www.wolfram.com/workbench/) (for documentation)

#### Building documentation 
First, import _MeshTools_ in Workbench:

* Select File -> Import...
* Git - Projects from Git (Next)
* Existing local repository (Next)
* Add... (browse to MeshTools, select, Next)
* Import as general project (Finish)

Importing of the _MeshTools_ source needs to be done only once.
Next, build the documentation:
  
* In the MeshTools folder right click on docbuild.xml
* Choose Run As...
* Choose 2 Ant Build...
* Deselect all 
* Select _MeshTools_
* Run


This will create a folder named `build` with subfolder `MeshTools`, that contains the built package documentation.

#### Packaging the paclet

Open terminal window (command line) in MeshTools root directory and run file `Build.wls`. 
This will create MeshTools-X.Y.Z.paclet file in the `build` folder and install it to `$UserBasePacletsDirectory`.
