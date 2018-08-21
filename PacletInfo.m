(* ::Package:: *)

(* Paclet Info File *)
Paclet[
	Name -> "MeshTools",
	Version -> "0.3.1",
	WolframVersion -> "11.+",
	Description -> "Utilities for creating and manipulating ElementMesh objects.",
	Creator -> "C3M d.o.o.",
	URL -> "https://github.com/c3m-labs/MeshTools",
	Thumbnail->"Icon.png",
	Extensions -> {
		{"Kernel",
			Root -> ".",
			Context ->{"MeshTools`"}
		},
		{"Documentation",
			Language -> "English",
			MainPage -> "Guides/MeshTools"
		},
		{"PacletServer",
			"Tags" -> {"finite-elements","mesh","FEM"},
			"Categories" -> {"FEM"},
			"Description" -> "A package with utilities for creating and manipulating ElementMesh objects.",
			"License" -> "MIT"
		}
	}
]
