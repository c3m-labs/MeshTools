# MeshTools
Utilities for manipulating Mathematica `ElementMesh` objects. 

![screenshot](https://i.imgur.com/Uy7AV3X.png)


## Usage

To access the documentation, open the notebook interface help viewer, and search for MeshTools. 
The first hit will be a summary page enumerating the most commonly used functions in MeshTools. 

* Create structured meshes of quadrilaterals or hexahedra over different geometric shapes
* Perform geometric transformation on meshes 
* Merge different meshes



## Installation

The MeshTools release comes in the form of a `.paclet` file, which contains the entire package and its documentation. 
Download the latest release from the [Github repo's releases page](https://github.com/c3m-labs/MeshTools/releases). 
To install, run the following command in the Wolfram Language:

    PacletInstall["/full/path/to/MeshTools.paclet"]

This will permanently install the MeshTools paclet. The Wolfram Language will always use the latest installed version of MeshTools. 
Installed versions can be enumerated using the command:

    PacletFind["MeshTools"]

And all versions can be uninstalled using the command:

    PacletUninstall["MeshTools"]

To make use of the documentation it may be necessary to restart.


## Bug reports

Please use the repository issues page to submit general bug issues.



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
