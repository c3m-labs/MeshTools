(* ::Package:: *)

(* Paclet Info File *)
Paclet[
	Name -> "MeshTools",
	Version -> "0.1.0",
	WolframVersion -> "11.+",
    Description -> "Utilities for creating and manipulating ElementMesh objects.",
    Creator -> "info@c3m.si",
    Publisher->"C3M d.o.o.",
    URL -> "https://github.com/c3m-labs/MeshTools",
    Tags -> {"finite-elements","mesh"},
    Categories -> {"FEM"},
	Extensions -> {
		{"Kernel", Root -> ".", Context ->{"MeshTools`"}},
		{"Documentation",  Language -> "English", MainPage -> "Guides/MeshTools"}
	}
]
