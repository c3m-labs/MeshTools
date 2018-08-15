(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Description*)


(* ::Text:: *)
(*These are unit test for "MeshTools" paclet. Each test should normally run fast enough (i.e. < 0.1 second), so that there can be many of them and the whole procedure doesn't take too long.*)


(* "MeshTools.wl" must be loaded before running these tests, otherwise testing is aborted. *)
If[
	Not@MemberQ[$Packages,"MeshTools`"],
	Print["Error: Package is not loaded!"];Abort[];
];


(* Currently it is unclear what this line does, it is automatically gnerated during conversion to .wlt *)
BeginTestSection["Tests"]


(* ::Subsection::Closed:: *)
(*TransformMesh*)


(* ::Subsubsection::Closed:: *)
(*2D*)


With[{
	(* one element mesh *)
	mesh=ToElementMesh[Triangle[],MaxCellMeasure->1,"MeshOrder"->1],
	(* Reflection over Y axis *)
	rt=ReflectionTransform[{1,0},{0,0}]
	},
	VerificationTest[
		TransformMesh[mesh,rt],	
		ElementMesh[
			{{0., 0.}, {-1., 0.}, {0., 1.}}, 
			{TriangleElement[{{2, 1, 3}}]}, 
			{LineElement[{{1, 3}, {3, 2}, {2, 1}}]}
		],
		TestID->"TransformMesh_1"
	]
]


With[{
	(* one element mesh *)
	mesh=ToElementMesh[Triangle[],MaxCellMeasure->1,"MeshOrder"->2],
	(* Reflection over Y axis *)
	rt=ReflectionTransform[{1,0},{0,0}]
	},
	VerificationTest[
		TransformMesh[mesh,rt],	
		ElementMesh[
			{{0., 0.}, {-1., 0.}, {0., 1.}, {-0.5, 0.}, {-0.5, 0.5}, {0., 0.5}}, 
			{TriangleElement[{{2, 1, 3, 4, 6, 5}}]}, 
			{LineElement[{{1, 3, 6}, {3, 2, 5}, {2, 1, 4}}]}
		],
		TestID->"TransformMesh_2"
	]
]


(* ::Subsubsection::Closed:: *)
(*3D*)


With[{
	(* one element mesh *)
	mesh=ToElementMesh[Tetrahedron[],MaxCellMeasure->1,"MeshOrder"->1],
	(* Reflection over Y axis *)
	rt=ReflectionTransform[{1,0,0},{0,0,0}]
	},
	VerificationTest[
		TransformMesh[mesh,rt],	
		ElementMesh[
			{{0., 0., 0.}, {-1., 0., 0.}, {0., 1., 0.}, {0., 0., 1.}}, 
			{TetrahedronElement[{{4, 1, 2, 3}}]}, 
			{TriangleElement[{{3, 2, 1}, {3, 4, 2}, {3, 1, 4}, {4, 1, 2}}]}
		],
		TestID->"TransformMesh_3"
	]
]


With[{
	(* one element mesh *)
	mesh=ToElementMesh[Tetrahedron[],MaxCellMeasure->1,"MeshOrder"->2],
	(* Reflection over Y axis *)
	rt=ReflectionTransform[{1,0,0},{0,0,0}]
	},
	VerificationTest[
		TransformMesh[mesh,rt],	
		ElementMesh[
			{{0.,0.,0.},{-1.,0.,0.},{0.,1.,0.},{0.,0.,1.},{-0.5,0.,0.},{0.,0.,0.5},{-0.5,0.,0.5},{0.,0.5,0.},{0.,0.5,0.5},{-0.5,0.5,0.}}, 
			{TetrahedronElement[{{4, 1, 2, 3, 6, 5, 7, 8, 10, 9}}]}, 
			{TriangleElement[{{3, 2, 1, 10, 5, 8}, {3, 4, 2, 9, 7, 10}, {3, 1, 4, 8, 6, 9}, {4, 1, 2, 6, 5, 7}}]}
		],
		TestID->"TransformMesh_4"
	]
]


(* ::Subsection::Closed:: *)
(*SelectElements*)


(* ::Subsubsection::Closed:: *)
(*SelectElementsByMarker*)


With[{
	(* A mesh with mixed element types. *)
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.},{1.,0.},{2.,0.},{2.5,0.5},{0.,1.},{1.,1.},{2.,1.},{3.,1.},{2.5,1.5},{0.,2.},{1.,2.},{2.,2.}},
		"MeshElements"->{
			QuadElement[{{1,2,6,5},{2,3,7,6},{5,6,11,10},{6,7,12,11}},{1,1,2,2}],
			TriangleElement[{{3,4,7},{4,8,7},{7,9,12},{7,8,9}},{1,1,2,2}]
		}]
	},
	VerificationTest[
		SelectElementsByMarker[mesh,1],	
		ElementMesh[
			{{0.,0.},{1.,0.},{2.,0.},{2.5,0.5},{0.,1.},{1.,1.},{2.,1.},{3.,1.}},
			{TriangleElement[{{3,4,7},{4,8,7}}],QuadElement[{{1,2,6,5},{2,3,7,6}}]},
			{LineElement[{{3,4},{8,7},{4,8},{1,2},{6,5},{5,1},{2,3},{7,6}}]}
		],
		TestID->"SelectElementsByMarker_1"
	]
]


With[{
	(* A mesh with mixed element types. *)
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.},{1.,0.},{2.,0.},{2.5,0.5},{0.,1.},{1.,1.},{2.,1.},{3.,1.},{2.5,1.5},{0.,2.},{1.,2.},{2.,2.}},
		"MeshElements"->{
			QuadElement[{{1,2,6,5},{2,3,7,6},{5,6,11,10},{6,7,12,11}},{1,1,2,2}],
			TriangleElement[{{3,4,7},{4,8,7},{7,9,12},{7,8,9}},{1,1,2,2}]
		}]
	},
	VerificationTest[
		SelectElementsByMarker[mesh,2],	
		ElementMesh[
			{{0., 1.}, {1., 1.}, {2., 1.}, {3., 1.}, {2.5, 1.5}, {0., 2.},{1., 2.}, {2., 2.}},
			{TriangleElement[{{3, 5, 8}, {3, 4, 5}}],QuadElement[{{1, 2, 7, 6}, {2, 3, 8, 7}}]},
			{LineElement[{{5, 8}, {4, 5}, {3, 4}, {1, 2}, {7, 6}, {6, 1}, {2, 3}, {8, 7}}]
		}],
		TestID->"SelectElementsByMarker_2"
	]
]


With[{
	(* A mesh with mixed element types. *)
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.},{1.,0.},{2.,0.},{2.5,0.5},{0.,1.},{1.,1.},{2.,1.},{3.,1.},{2.5,1.5},{0.,2.},{1.,2.},{2.,2.}},
		"MeshElements"->{
			QuadElement[{{1,2,6,5},{2,3,7,6},{5,6,11,10},{6,7,12,11}},{1,1,2,2}],
			TriangleElement[{{3,4,7},{4,8,7},{7,9,12},{7,8,9}},{1,1,2,2}]
		}]
	},
	VerificationTest[
		SelectElementsByMarker[mesh,0],	
		_ElementMesh,
		{SelectElementsByMarker::marker},
		SameTest->MatchQ,
		TestID->"SelectElementsByMarker_3"
	]
]


(* ::Subsubsection::Closed:: *)
(*SelectElements*)


With[{
	mesh=ToElementMesh[
		Rectangle[],
		MaxCellMeasure->1/2,
		"MeshOrder"->1
	]
	},
	VerificationTest[
		SelectElements[mesh,#1>=0.5&&#2>=0.5&],	
		ElementMesh[
			{{0.5, 0.5}, {0.5, 1.}, {1., 0.5}, {1., 1.}},
			{QuadElement[{{1, 3, 4, 2}}, {0}]},
			{LineElement[{{1, 3}, {3, 4}, {4, 2}, {2, 1}}]}
		],
		TestID->"SelectElements_1"
	]
]


With[{
	mesh=ToElementMesh[
		Rectangle[],
		MaxCellMeasure->1/2,
		"MeshOrder"->1
	]
	},
	VerificationTest[
		SelectElements[mesh,#1>=2&],	
		$Failed,
		{SelectElements::noelms},
		TestID->"SelectElements_2"
	]
]


With[{
	mesh=ToElementMesh[
		Rectangle[],
		MaxCellMeasure->1/2,
		"MeshOrder"->1
	]
	},
	VerificationTest[
		SelectElements[mesh,#1>=0.5&&#2>=0.5&&#3>=0.5&],	
		$Failed,
		{SelectElements::funslots},
		TestID->"SelectElements_3"
	]
]


(* ::Subsection::Closed:: *)
(*ExtrudeMesh*)


With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.},{0.,1.},{1.,0.},{1.,1.},{2.,0.},{2.,1.}},
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
(*ToTriangleMesh*)


With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.},{1.,0.},{1.,1.},{0.,1.}},
		"MeshElements"->{QuadElement[{{1,2,3,4}}]}
	]
	},
	VerificationTest[
		ToTriangleMesh[mesh],	
		ElementMesh[
			{{0., 0.}, {1., 0.}, {1., 1.}, {0., 1.}}, 
			{TriangleElement[{{1, 2, 3}, {1, 3, 4}}]}, 
			{LineElement[{{2, 3}, {1, 2}, {3, 4}, {4, 1}}]}
		],
		TestID->"ToTriangleMesh_1"
	]
]


(* ::Subsection::Closed:: *)
(*ToTetrahedronMesh*)


With[{
	mesh=ToElementMesh[
		"Coordinates"->{{0.,0.,0.},{0.,0.,1.},{0.,1.,0.},{0.,1.,1.},{1.,0.,0.},{1.,0.,1.},{1.,1.,0.},{1.,1.,1.}},
		"MeshElements"->{HexahedronElement[{{1,5,7,3,2,6,8,4}}]}
	]
	},
	VerificationTest[
		ToTetrahedronMesh[mesh],
		ElementMesh[
			{{0.,0.,0.},{0.,0.,1.},{0.,1.,0.},{0.,1.,1.},{1.,0.,0.},{1.,0.,1.},{1.,1.,0.},{1.,1.,1.}}, 
			{TetrahedronElement[{{3, 1, 5, 2}, {8, 2, 5, 6}, {3, 5, 7, 8}, {3, 2, 5, 8}, {3, 2, 8, 4}}]}, 
			{TriangleElement[{{2,5,1},{2,1,3},{3,1,5},{6,5,2},{6,8,5},{6,2,8},{8,7,5},{8,3,7},{3,5,7},{4,8,2},{4,3,8},{4,2,3}}]}
		],
		TestID->"ToTetrahedronMesh_1"
	]
]


(* ::Subsection::Closed:: *)
(*StructuredMesh*)


VerificationTest[
	MeshTools`Private`StructuredMesh[{{{0,0},{2,0}},{{0,1},{2,1}}},{2,1}],
	ElementMesh[
		{{0.,0.},{1.,0.},{2.,0.},{0.,1.},{1.,1.},{2.,1.}},
		{QuadElement[{{1,2,5,4},{2,3,6,5}}]},
		{LineElement[{{1,2},{5,4},{4,1},{2,3},{3,6},{6,5}}]}
	],
	TestID->"StructuredMesh_2D_1"
]


VerificationTest[
	MeshTools`Private`StructuredMesh[{{{0,0,0},{2,0,0}},{{0,1,0},{2,1,0}}},{2,1}],
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
		MeshTools`Private`StructuredMesh[{{{{0,0,0},{a,0,0}},{{0,b,0},{a,b,0}}},{{{0,0,c},{a,0,c}},{{0,b,c},{a,b,c}}}},{3,2,1}]
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
(*TriangleMesh*)


VerificationTest[
	TriangleMesh[{{0,0},{1,0},{0,1}},2,"MeshElementType"->TriangleElement],
	ElementMesh[
		{{0., 0.}, {0.5, 0.}, {0., 0.5}, {1., 0.}, {0.5, 0.5}, {0., 1.}}, 
		{TriangleElement[{{1, 2, 3}, {2, 4, 5}, {2, 5, 3}, {3, 5, 6}}]},
		{LineElement[{{3, 1}, {1, 2}, {4, 5}, {2, 4}, {5, 6}, {6, 3}}]}
	],
	TestID->"TriangleMesh_1"
]


VerificationTest[
	TriangleMesh[{{0,0},{1,0},{0,1}},2,"MeshElementType"->QuadElement],
	ElementMesh[
		{{0., 0.}, {0.5, 0.}, {0., 0.5}, {0.3333333333333333, 0.3333333333333333}, {1., 0.}, {0.5, 0.5}, {0., 1.}},
		{QuadElement[{{1, 2, 4, 3}, {2, 5, 6, 4}, {3, 4, 6, 7}}, {0, 0, 0}]}, 
		{LineElement[{{1, 2}, {3, 1}, {2, 5}, {5, 6}, {6, 7}, {7, 3}}]}
	],
	TestID->"TriangleMesh_2"
]


VerificationTest[
	TriangleMesh[{{0,0},{1,0},{0,1}},1],
	$Failed,
	{TriangleMesh::quadelms},
	TestID->"TriangleMesh_3"
]


VerificationTest[
	TriangleMesh[{{0,0},{1,0},{0,1}},2,"MeshElementType"->"BadValue"],
	$Failed,
	{TriangleMesh::badtype},
	TestID->"TriangleMesh_4"
]


(* ::Subsection::Closed:: *)
(*SphereMesh*)


VerificationTest[
	SphereMesh[{1,2,3},3,3],
	_ElementMesh,
	TestID->"SphereMesh_1",
	SameTest->MatchQ
]


VerificationTest[
	SphereMesh[1],
	$Failed,
	{SphereMesh::noelems},
	TestID->"SphereMesh_2"
]


(* ::Subsection::Closed:: *)
(*SphericalShellMesh*)


(* Test default SphericalShell with default "MeshOrder" *)
VerificationTest[
	SphericalShellMesh[{4,2}],
	_ElementMesh,
	TestID->"SphericalShellMesh_1",
	SameTest->MatchQ
]


(* Test default SphericalShell with "MeshOrder"->2 *)
VerificationTest[
	SphericalShellMesh[{4,1},"MeshOrder"->2],
	_ElementMesh,
	TestID->"SphericalShellMesh_2",
	SameTest->MatchQ
]


(* Test SphericalShell with arbitrary position and size. *)
VerificationTest[
	SphericalShellMesh[{1,2,3},{2,3},{6,2}],
	_ElementMesh,
	TestID->"SphericalShellMesh_3",
	SameTest->MatchQ
]


(* ::Subsection::Closed:: *)
(*BallMesh*)


VerificationTest[
	Head@BallMesh[{0,0,0},1,1],
	ElementMesh,
	TestID->"BallMesh_1"
]


(* ::Subsection::Closed:: *)
(*TetrahedronMesh*)


VerificationTest[
	TetrahedronMesh[{{0,0,0},{1,0,0},{0,1,0},{0,0,1}},2,"MeshElementType"->TetrahedronElement],
	ElementMesh[
		{{0., 0., 0.}, {0.5, 0., 0.}, {0., 0.5, 0.}, {0., 0., 0.5}, {1., 0., 0.}, {0.5, 0.5, 0.}, {0.5, 0., 0.5}, {0., 1., 0.}, {0., 0.5, 0.5}, {0., 0., 1.}}, 
		{TetrahedronElement[{{1, 2, 3, 4}, {2, 5, 6, 7}, {2, 3, 7, 6}, {3, 6, 8, 9}, {2, 3, 4, 7}, {3, 6, 9, 7}, {3, 4, 7, 9}, {4, 7, 9, 10}}]},
		{TriangleElement[{{4, 1, 3}, {4, 2, 1}, {1, 2, 3}, {7, 6, 5}, {7, 5, 2}, {2, 5, 6}, {6, 3, 2}, {9, 8, 6}, {9, 3, 8}, {3, 6, 8}, {7, 2, 4}, {7, 9, 6}, {9, 4, 3}, {10, 9, 7}, {10, 4, 9}, {10, 7, 4}}]}
	],
	TestID->"TetrahedronMesh_1"
]


VerificationTest[
	TetrahedronMesh[{{0,0,0},{1,0,0},{0,1,0},{0,0,1}},1,"MeshElementType"->TetrahedronElement],
	$Failed,
	{TetrahedronMesh::tetelms},
	TestID->"TetrahedronMesh_2"
]


VerificationTest[
	TetrahedronMesh[{{0,0,0},{1,0,0},{0,1,0},{0,0,1}},2,"MeshElementType"->HexahedronElement],
	ElementMesh[
		{{0.,0.,0.},{0.5, 0., 0.},{0.,0.5,0.},{1/3,1/3,0.},{0.,0.,0.5},{1/3,0., 1/3},{0.,1/3,1/3},{0.25,0.25,0.25},{1.,0.,0.},{0.5,0.5,0.},{0.5,0.,0.5},{1/3,1/3,1/3},{0.,1.,0.},{0.,0.5,0.5},{0.,0.,1.}},
		{HexahedronElement[{{1,2,4,3,5,6,8,7},{2,9,10,4,6,11,12,8},{3,4,10,13,7,8,12,14},{5,6,8,7,15,11,12,14}},{0,0,0,0}]},
		{QuadElement[{{1,2,4,3},{1,5,6,2},{3,7,5,1},{2,9,10,4},{2,6,11,9},{9,11,12,10},{3,4,10,13},{10,12,14,13},{13,14,7,3},{14,12,11,15},{5,15,11,6},{7,14,15,5}}]}
	],
	TestID->"TetrahedronMesh_3"
]


VerificationTest[
	TetrahedronMesh[{{0,0,0},{1,0,0},{0,1,0},{0,0,1}},1,"MeshElementType"->HexahedronElement],
	$Failed,
	{TetrahedronMesh::hexelms},
	TestID->"TetrahedronMesh_4"
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
