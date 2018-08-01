(* ::Package:: *)

(* Paclet Info File *)
Paclet[
	Name -> "MeshTools",
	Version -> "0.3.0",
	WolframVersion -> "11.+",
	Description -> "Utilities for creating and manipulating ElementMesh objects.",
	Creator -> "C3M d.o.o. (info@c3m.si)",
	URL -> "https://github.com/c3m-labs/MeshTools",
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
