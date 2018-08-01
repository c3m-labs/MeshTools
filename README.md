# MeshTools
Utilities for creating and manipulating Mathematica `ElementMesh` objects. 

![screenshot](https://i.imgur.com/Uy7AV3X.png)


## Installation

The MeshTools release comes in the form of a `.paclet` file, which contains the entire package and its documentation. 
Download the latest release from the [Github repo's releases page](https://github.com/c3m-labs/MeshTools/releases). 
To install, run the following command in the Wolfram Language:

    PacletInstall["/full/path/to/MeshTools-X.Y.Z.paclet"]

This will permanently install the MeshTools paclet. The Wolfram Language will always use the latest installed version of MeshTools. 
Installed versions can be enumerated using the command:

    PacletFind["MeshTools"]

And all versions can be uninstalled using the command:

    PacletUninstall["MeshTools"]

To update the documentation it may be necessary to restart Mathematica.


## Usage

To access the documentation, open the notebook interface help viewer and search for MeshTools. 
The first hit will be a summary page enumerating the most commonly used functions in MeshTools that enable you to perform the following tasks:

* Create structured meshes of quadrilaterals or hexahedra over basic geometric shapes
* Perform geometric transformation on meshes 
* Merge different meshes


## Contributing and bug reports

Please use the repository [issues](https://github.com/c3m-labs/MeshTools/issues) page to submit bugs or feature ideas. 

Contributions to this repository are very welcome. Guidelines on how to build paclet file from source code can be found in [Development.md]( Development.md ) file.

