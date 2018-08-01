## How to build MeshTools

#### Prerequisites
* Version 11.3 or greater of Mathematica
* Wolfram Workbench

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
