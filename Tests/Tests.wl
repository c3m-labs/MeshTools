(* ::Package:: *)

BeginTestSection["Tests"]


(* ::Subsection::Closed:: *)
(*Utility functions*)


VerificationTest[
	With[{
		dir=If[
			$Notebooks,
			ParentDirectory[DirectoryName[$InputFileName]/. ""->NotebookDirectory[]],
			Directory[]
		]
		},
		Get["MeshTools.wl",Path->dir];
		MemberQ[$Packages,"MeshTools`"]
	],
	True,
	TestID->"LoadPackage"
]


(* ::Subsection::Closed:: *)
(*ExtrudeMesh*)


With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.`,0.`},{0.`,1.`},{1.`,0.`},{1.`,1.`},{2.`,0.`},{2.`,1.`}},
		"MeshElements"->{QuadElement[{{1,3,4,2},{3,5,6,4}},{1,2}]}
	]
	},
	VerificationTest[
		ExtrudeMesh[mesh,1,1],	
		ElementMesh[
			{{0., 0., 0.}, {0., 1., 0.}, {1., 0., 0.}, {1., 1., 0.}, {2., 0., 0.}, {2., 1., 0.}, 
			{0., 0., 1.}, {0., 1., 1.}, {1., 0., 1.},  {1., 1., 1.}, {2., 0., 1.}, {2., 1., 1.}},
			{HexahedronElement[{{1, 3, 4, 2, 7, 9, 10, 8}, {3, 5, 6, 4, 9, 11, 12, 10}}, {1, 2}]},
			{QuadElement[{{1, 3, 4, 2}, {8, 10, 9, 7}, {1, 7, 9, 3}, {3, 9, 10, 4}, {4, 10, 8, 2}, 
			{2, 8, 7, 1}, {3, 5, 6, 4}, {10, 12, 11, 9}, {3, 9, 11, 5}, {5, 11, 12, 6}, {6, 12, 10, 4}}]}
		],
		TestID->"ExtrudeMesh_1"
	]
]


(* ::Subsection::Closed:: *)
(*StructuredMesh*)


VerificationTest[
	StructuredMesh[{{{0,0},{2,0}},{{0,1},{2,1}}},{2,1}],
	ElementMesh[
		{{0.,0.},{1.,0.},{2.,0.},{0.,1.},{1.,1.},{2.,1.}},
		{QuadElement[{{1,2,5,4},{2,3,6,5}}]},
		{LineElement[{{1,2},{5,4},{4,1},{2,3},{3,6},{6,5}}]}
	],
	TestID->"StructuredMesh_2D_1"
]


VerificationTest[
	StructuredMesh[{{{0,0,0},{2,0,0}},{{0,1,0},{2,1,0}}},{2,1}],
	ElementMesh[
		{{0.,0.,0.},{1.,0.,0.},{2.,0.,0.},{0.,1.,0.},{1.,1.,0.},{2.,1.,0.}},
		Automatic,
		{QuadElement[{{1,2,5,4},{2,3,6,5}},{1,1}]},
		{PointElement[{{1},{2},{3},{4},{5},{6}}]}
	],
	TestID->"StructuredMesh_3D_1"
]


VerificationTest[
	With[
		{a=3,b=2,c=1},
		StructuredMesh[{{{{0,0,0},{a,0,0}},{{0,b,0},{a,b,0}}},{{{0,0,c},{a,0,c}},{{0,b,c},{a,b,c}}}},{3,2,1}]
	],
	ElementMesh[
		{{0.,0.,0.},{1.,0.,0.},{2.,0.,0.},{3.,0.,0.},{0.,1.,0.},{1.,1.,0.},{2.,1.,0.},{3.,1.,0.},{0.,2.,0.},{1.,2.,0.},{2.,2.,0.},{3.,2.,0.},{0.,0.,1.},{1.,0.,1.},{2.,0.,1.},{3.,0.,1.},{0.,1.,1.},{1.,1.,1.},{2.,1.,1.},{3.,1.,1.},{0.,2.,1.},{1.,2.,1.},{2.,2.,1.},{3.,2.,1.}},
		{HexahedronElement[{{1,2,6,5,13,14,18,17},{2,3,7,6,14,15,19,18},{3,4,8,7,15,16,20,19},{5,6,10,9,17,18,22,21},{6,7,11,10,18,19,23,22},{7,8,12,11,19,20,24,23}}]},
		{QuadElement[{{1,2,6,5},{17,18,14,13},{1,13,14,2},{5,17,13,1},{2,3,7,6},{18,19,15,14},{2,14,15,3},{3,4,8,7},{19,20,16,15},{3,15,16,4},{4,16,20,8},{5,6,10,9},{21,22,18,17},{10,22,21,9},{9,21,17,5},{6,7,11,10},{22,23,19,18},{11,23,22,10},{7,8,12,11},{23,24,20,19},{8,20,24,12},{12,24,23,11}}]}
	],
	TestID->"StructuredMesh_3D_2"
]


(* ::Subsection::Closed:: *)
(*DiskMesh*)


VerificationTest[
	DiskMesh[1],
	DiskMesh[1],
	{DiskMesh::noelems},
	TestID->"DiskMesh_1"
]


VerificationTest[
	DiskMesh[2,Method->"unknown"],
	$Failed,
	{DiskMesh::method},
	TestID->"DiskMesh_2"
]


VerificationTest[
	Head@DiskMesh[2,Method->"Projection"],
	ElementMesh,
	TestID->"DiskMesh_3"
]


VerificationTest[
	Head@DiskMesh[2,Method->"Block"],
	ElementMesh,
	TestID->"DiskMesh_4"
]


(* ::Subsection::Closed:: *)
(*SphereMesh*)


VerificationTest[
	Head@SphereMesh[{1,2,3},3,3],
	ElementMesh,
	TestID->"SphereMesh_1"
]


VerificationTest[
	Head@SphereMesh[3],
	ElementMesh,
	TestID->"SphereMesh_2"
]


VerificationTest[
	SphereMesh[1],
	$Failed,
	{SphereMesh::noelems},
	TestID->"SphereMesh_3"
]


(* ::Subsection::Closed:: *)
(*MeshElementMeasure*)


(* ::Text:: *)
(*Returns the volume in 3D embedding, area in 2D embedding or length in 1D embedding.*)


(* Length *)
With[{
	mesh=ToElementMesh[
		"Coordinates"->Partition[Subdivide[0,1,3],1],
		"MeshElements"->{LineElement[{{1,2},{2,3},{3,4}}]}
	]
	},
	VerificationTest[
		MeshElementMeasure[mesh],	
		mesh["MeshElementMeasure"],
		TestID->"MeshElementMeasure_1"
	]
]


(* Area *)
With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.},{1.,0.},{1.,1.},{0.,1.}},
		"MeshElements"->{TriangleElement[{{1,2,3},{3,4,1}}]}
	]
	},
	VerificationTest[
		MeshElementMeasure[mesh],	
		mesh["MeshElementMeasure"],
		TestID->"MeshElementMeasure_2"
	]
]


(* Area - 2nd order mesh *)
With[{
	mesh=ToElementMesh[
		(* One node is slightly offsed to create curved sides. *)
		"Coordinates"->{{0.,0.},{0.,1.},{1.,0.},{1.,1.},{0.,0.5},{0.5,0.},{0.6,0.6},{1.,0.5},{0.5,1.}},
		"MeshElements"->{TriangleElement[{{2,1,3,5,6,7},{3,4,2,8,9,7}}]}
	]
	},
	VerificationTest[
		MeshElementMeasure[mesh],
		mesh["MeshElementMeasure"],
		TestID->"MeshElementMeasure_3"
	]
]


(* Area - 2nd order mesh *)
With[{
	mesh=ToElementMesh[
		(* One node is slightly offsed to create curved sides. *)
		"Coordinates"->{{0.,0.},{0.,1.},{1.,0.},{1.,1.},{0.5,0.1},{1.,0.5},{0.5,1.},{0.,0.5}},
		"MeshElements"->{QuadElement[{{1,3,4,2,5,6,7,8}}]}
	]
	},
	VerificationTest[
		MeshElementMeasure[mesh],
		mesh["MeshElementMeasure"],
		TestID->"MeshElementMeasure_4"
	]
]


(* Volume *)
With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.,0.},{0.,0.,1.},{0.,1.,0.},{0.,1.,1.},{1.,0.,0.},{1.,0.,1.},{1.,1.,0.},{1.,1.,1.}},
		"MeshElements"->{TetrahedronElement[{{1,2,8,4},{8,1,6,2},{5,1,6,8},{5,7,1,8},{1,8,7,3},{8,3,1,4}}]}
	]
	},
	VerificationTest[
		MeshElementMeasure[mesh],
		mesh["MeshElementMeasure"],
		TestID->"MeshElementMeasure_5"
	]
]


(* Volume *)
With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.,0.},{0.,0.,1.},{0.,1.,0.},{0.,1.,1.},{1.,0.,0.},{1.,0.,1.},{1.,1.,0.},{1.,1.,1.}},
		"MeshElements"->{HexahedronElement[{{1,5,7,3,2,6,8,4}}]}
	]
	},
	VerificationTest[
		MeshElementMeasure[mesh],
		mesh["MeshElementMeasure"],
		TestID->"MeshElementMeasure_6"
	]
]


(* ::Subsection::Closed:: *)
(*BoundaryElementMeasure*)


(* ::Text:: *)
(*Returns the surface of boundary elements in 3D embedding and length of boundary elements in 2D embedding.*)


(* ::Subsubsection::Closed:: *)
(*Perimeter*)


With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.},{1.,0.},{1.,1.},{0.,1.}},
		"MeshElements"->{TriangleElement[{{1,2,3},{3,4,1}}]}
	]
	},
	VerificationTest[
		BoundaryElementMeasure[mesh],	
		{{1.,1.,1.,1.}},
		TestID->"BoundaryElementMeasure_1"
	]
]


With[{
	mesh=ToElementMesh[
		(* One node is slightly offsed to create curved sides. *)
		"Coordinates"->{{0.,0.},{0.,1.},{1.,0.},{1.,1.},{0.,0.5},{0.5,0.},{0.6,0.6},{1.,0.5},{0.5,1.}},
		"MeshElements"->{TriangleElement[{{2,1,3,5,6,7},{3,4,2,8,9,7}}]}
	]
	},
	VerificationTest[
		BoundaryElementMeasure[mesh],
		{{1.,1.,1.,1.}},
		TestID->"BoundaryElementMeasure_2"
	]
]


(* ::Subsubsection::Closed:: *)
(*Surface area*)


(*With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.,0.},{0.,0.,1.},{0.,1.,0.},{0.,1.,1.},{1.,0.,0.},{1.,0.,1.},{1.,1.,0.},{1.,1.,1.}},
		"MeshElements"->{TetrahedronElement[{{1,2,8,4},{8,1,6,2},{5,1,6,8},{5,7,1,8},{1,8,7,3},{8,3,1,4}}]}
	]
	},
	VerificationTest[
		BoundaryElementMeasure[mesh],
		{{1.,1.,1.,1.,1.,1.}},
		TestID->"BoundaryElementMeasure_3"
	]
]*)


(*With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.,0.},{0.,0.,1.},{0.,1.,0.},{0.,1.,1.},{1.,0.,0.},{1.,0.,1.},{1.,1.,0.},{1.,1.,1.}},
		"MeshElements"->{HexahedronElement[{{1,5,7,3,2,6,8,4}}]}
	]
	},
	VerificationTest[
		BoundaryElementMeasure[mesh],
		{{1.,1.,1.,1.,1.,1.}},
		TestID->"BoundaryElementMeasure_4"
	]
]*)


(* ::Subsection::Closed:: *)
(*EndTestSection*)


EndTestSection[]
