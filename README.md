# MeshTools
Utilities for creating and manipulating Mathematica `ElementMesh` objects. 

![example1](Graphics/ExampleMeshes.png)


## Installation

The following description is for people who just want to use the package functionality and 
are not interested in package development. 
To use _MeshTools_ package you need Mathematica version 11. or later.

_MeshTools_ package is released in the `.paclet` file format, which contains code, 
documentation and other necesseary resources. 
Download the latest `.paclet` file from the [repository "releases" page](https://github.com/c3m-labs/MeshTools/releases) 
to your computer and install it by evaluating the following command in the Mathematica:

```mathematica
(* This is ussualy loaded automatically at kernel startup. *)
Needs["PacletManager`"] 

(* Download .paclet file from repository "releases" page. *)
PacletInstall["full/path/to/MeshTools-X.Y.Z.paclet"] 
```

This will permanently install the _MeshTools_ package to `$UserBasePacletsDirectory`. 
To update the documentation it may be necessary to restart Mathematica. 
Mathematica will always use the latest installed version of package and all installed versions 
can be enumerated by evaluating `PacletFind["MeshTools"]`.
You can get more detailed information about the package with `PacletInformation["MeshTools"]`.
All versions can be uninstalled with:

```mathematica
PacletUninstall["MeshTools"]
```



## Usage

After you have installed the paclet, load it to Mathematica session with `Get`. 
Then you can, for example, make a `ElementMesh` object from basic geometric shape and visualise it.

```mathematica
Get["MeshTools`"]
    
outerMesh = AnnulusMesh[{0, 0}, {2/3, 1}, {0, 3 Pi/2}, {24, 4}];
innerMesh = AnnulusMesh[{0, 0}, {1/2, 2/3}, {0, 3 Pi/2}, {24, 2}];
mesh = MergeMesh[{
    AddMeshMarkers[outerMesh, 1],
    AddMeshMarkers[innerMesh, 2]
}];

mesh["Wireframe"[
    "MeshElementStyle" -> FaceForm /@ {ColorData[112, 3], ColorData[112, 2]}]
]
```
    
![screenshot](Graphics/DoubleAnnulus.png )

To access the documentation, open the notebook interface help viewer and search for MeshTools. 
The first hit will be a summary page enumerating the most commonly used functions in MeshTools that enable you to perform the following tasks:

* Create structured meshes of quadrilaterals or hexahedra over basic geometric shapes
* Perform geometric transformation on meshes 
* Merge different meshes


## Contributing and bug reports

Please use the repository [issues](https://github.com/c3m-labs/MeshTools/issues) page to submit bugs or feature ideas. 

Contributions to this repository are very welcome. 
Guidelines on how to build paclet file from source code can be found in [Development.md]( Development.md ) file.

