(* ::Package:: *)

(* Paclet Info File *)
(* BuildNumber and Internal values should be inserted during build procedure. *)
Paclet[
	Name -> "MeshTools",
	Version -> "0.5.0",
	WolframVersion -> "11.+",
	Description -> "Utilities for creating and manipulating ElementMesh objects.",
	Creator -> "Matevz Pintar",
	Publisher -> "C3M d.o.o.",
	URL -> "https://github.com/c3m-labs/MeshTools",
	Thumbnail->"FrontEnd/Icon.png",
	Extensions -> {
		{"Kernel",
			Root -> ".",
			Context ->{"MeshTools`"}
		},
		{"Documentation",
			Language -> "English",
			MainPage -> "Guides/MeshTools"
		},
		(* Metadata for PacletServer (https://paclets.github.io/PacletServer) *)
		{"PacletServer",
			"Tags" -> {"finite-elements","mesh","FEM"},
			"Categories" -> {"FEM"},
			"Description" -> "A package with utilities for creating and manipulating ElementMesh objects.",
			"License" -> "MIT"
		}
	}
]
