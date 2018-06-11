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
	Head@SphereMesh[{1,2,3},4,3],
	ElementMesh,
	TestID->"SphereMesh_1"
]


VerificationTest[
	Head@SphereMesh[4],
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
(*Mesh measurements*)


(* ::Text:: *)
(*Returns the volume in 3D embedding, area in 2D embedding or length in 1D embedding.*)


(* ::Text:: *)
(*Length*)


VerificationTest[
	mesh=ToElementMesh["Coordinates"->Partition[Range[0.,1.,1/9],1],"MeshElements"->{LineElement[{{1,2},{2,3},{3,4},{4,5},{5,6},{6,7},{7,8},{8,9},{9,10}}]}];
	MeshElementMeasure[mesh],
	mesh["MeshElementMeasure"],
	TestID->"MeshElementMeasure_Line_1"
]


(* ::Text:: *)
(*Area*)


VerificationTest[
	mesh=ToElementMesh["Coordinates"->{{0.,0.},{1.,0.},{1.,1.},{0.,1.}},"MeshElements"->{TriangleElement[{{1,2,3},{3,4,1}}]}];
	MeshElementMeasure[mesh],
	mesh["MeshElementMeasure"],
	TestID->"MeshElementMeasure_1"
]


VerificationTest[
	mesh=ToElementMesh[Disk[],MaxCellMeasure->1/4,"MeshOrder"->2];
	MeshElementMeasure[mesh],
	mesh["MeshElementMeasure"],
	TestID->"MeshElementMeasure_2",
	SameTest->(Total[Flatten[#1]]==Total[Flatten[#2]]&)
]


VerificationTest[
	mesh=ToElementMesh[Disk[],MaxCellMeasure->1/4,"MeshOrder"->1];
	MeshElementMeasure[mesh],
	mesh["MeshElementMeasure"],
	TestID->"MeshElementMeasure_3",
	SameTest->(Total[Flatten[#1]]==Total[Flatten[#2]]&)
]


(* ::Text:: *)
(*Volume*)


VerificationTest[
	mesh=ToElementMesh[Ball[],"MeshOrder"->2,MaxCellMeasure->1];
	MeshElementMeasure[mesh],
	mesh["MeshElementMeasure"],
	TestID->"MeshElementMeasure_4",
	SameTest->(Total[Flatten[#1]]==Total[Flatten[#2]]&)
]


VerificationTest[
	mesh=ToElementMesh[Ball[],"MeshOrder"->1];
	MeshElementMeasure[mesh],
	mesh["MeshElementMeasure"],
	TestID->"MeshElementMeasure_5",
	SameTest->(Total[Flatten[#1]]==Total[Flatten[#2]]&)
]


(* ::Text:: *)
(*Returns the surface of boundary elements in 3D embedding and length of boundary elements in 2D embedding.*)


(* ::Text:: *)
(*Perimeter*)


VerificationTest[
	mesh=ToElementMesh[Disk[],"MeshOrder"->2];
	err=1/10^3;BoundaryElementMeasure[mesh],
	2 \[Pi],
	TestID->"BoundaryElementMeasure_Perimeter_1",
	SameTest->(Abs[(#2-Total[Total[#1]])/Total[Total[#1]]]<err&)
]


VerificationTest[
	mesh=ToBoundaryMesh[Disk[],"MeshOrder"->2];
	err=1/10^3;BoundaryElementMeasure[mesh],
	2 \[Pi],
	TestID->"BoundaryElementMeasure_Perimeter_2",
	SameTest->(Abs[(#2-Total[Total[#1]])/Total[Total[#1]]]<err&)
]


(* ::Subsection::Closed:: *)
(*EndTestSection*)


EndTestSection[]
