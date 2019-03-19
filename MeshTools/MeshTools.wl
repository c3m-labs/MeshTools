(* ::Package:: *)

(* ::Section::Closed:: *)
(*Header comments*)


(* :Title: MeshTools *)
(* :Context: MeshTools` *)
(* :Author: Matevz Pintar *)
(* :Summary: Utilities for generating and manipulating ElementMesh objects. *)
(* :Copyright: C3M d.o.o., Slovenia, 2018 *)

(* :Acknowledgements: Jan Tomec, Joze Korelc, Oliver Ruebenkoenig *)


(* ::Section::Closed:: *)
(*Begin package*)


(* Mathematica FEM functionality (context "NDSolve`FEM`") is needed. *)
BeginPackage["MeshTools`",{"NDSolve`FEM`"}];


(* Clear definitions from package symbols in public and private context. *)
ClearAll["`*","`*`*"];


(* ::Subsection::Closed:: *)
(*Public symbols*)


AddMeshMarkers;
IdentifyMeshBoundary;
SelectElements;

MergeMesh;
TransformMesh;
ExtrudeMesh;

SmoothenMesh;
QuadToTriangleMesh;
TriangleToQuadMesh;
HexToTetrahedronMesh;

ElementMeshCurvedWireframe;

MeshElementMeasure;
BoundaryElementMeasure;

StructuredMesh;
RectangleMesh;
TriangleMesh;
DiskMesh;
AnnulusMesh;
CircularVoidMesh;

CuboidMesh;
HexahedronMesh;
TetrahedronMesh;
PrismMesh;
CylinderMesh;
SphereMesh;
SphericalShellMesh;
BallMesh;


(* ::Section::Closed:: *)
(*Code*)


(* Begin private context *)
Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Mesh operations*)


(* ::Subsubsection::Closed:: *)
(*AddMeshMarkers*)


insertMarkers[mesh_,elementType_,value_]:=Module[
	{original,new},
	original=ElementMarkers@mesh[elementType];
	new=If[
		value===Automatic,
		original,
		ConstantArray[value,Dimensions@original]
	];

	MapThread[
		Append[Take[#1,1],#2]&,
		{mesh[elementType],new}
	]
];


AddMeshMarkers::usage="AddMeshMarkers[mesh, keyword->marker] adds integer marker element type according to keyword.";
AddMeshMarkers::badkey="`1` is not recognized keyword \"MeshElementsMarker\", \"BoundaryElementsMarker\" or \"PointElementsMarker\".";

AddMeshMarkers//SyntaxInformation={"ArgumentsPattern"->{_,_}};

AddMeshMarkers[mesh_ElementMesh,list_List]:=Fold[AddMeshMarkers[#1,#2]&,mesh,list];

(* TODO: Add FrontEnd autocomplete for type argument. *)
AddMeshMarkers[mesh_ElementMesh,type_String->int_Integer]:=Module[
	{mMark,bMark,pMark},

	mMark=bMark=pMark=Automatic;
	
	Switch[type,
		"MeshElementsMarker",mMark=int,
		"BoundaryElementsMarker",bMark=int,
		"PointElementsMarker",pMark=int,
		_,Message[AddMeshMarkers::badkey,type];Return[mesh,Module]
	];
	
	If[
		mesh["MeshElements"]===Automatic,
		ToBoundaryMesh[
			"Coordinates"->mesh["Coordinates"],
			"BoundaryElements"->insertMarkers[mesh,"BoundaryElements",bMark],
			"PointElements"->insertMarkers[mesh,"PointElements",pMark]
		],
		ToElementMesh[
			"Coordinates"->mesh["Coordinates"],
			"MeshElements"->insertMarkers[mesh,"MeshElements",mMark],
			"BoundaryElements"->insertMarkers[mesh,"BoundaryElements",bMark],
			"PointElements"->insertMarkers[mesh,"PointElements",pMark]
		]
	]
];


(* ::Subsubsection::Closed:: *)
(*IdentifyMeshBoundary*)


boundingBoxMeasure[coordinates_,incidents_]:=Times@@Flatten[
	Differences/@MinMax/@Transpose@Part[
		coordinates,
		Union[Join@@incidents]
	]
];


(* 
Function assigns different integer marker to "BoundaryElements" on each disconnected boundary. 
Boundary with largest bounding box measure has marker 1. 
This is external boundary in case of holes.
Method is adjusted from https://mathematica.stackexchange.com/a/189117
*)
IdentifyMeshBoundary::usage="IdentifyMeshBoundary[mesh] identifies distinct boundaries of mesh.";
IdentifyMeshBoundary::mixed="Mixed element type on boundary mesh is not supported.";

IdentifyMeshBoundary//SyntaxInformation={"ArgumentsPattern"->{_}};

IdentifyMeshBoundary[mesh_ElementMesh]:=Module[
	{incidents,elementType,vbc,mat,components,boundariesConn,markers},
	
	If[
		Length[mesh["BoundaryElements"]]>1,
		Message[IdentifyMeshBoundary::mixed];Return[$Failed,Module]
	];
	
	incidents=Join@@ElementIncidents[mesh["BoundaryElements"]];
	elementType=Head@First@mesh["BoundaryElements"];
	vbc=mesh["VertexBoundaryConnectivity"];
	mat=Transpose[vbc].vbc;
	components=SparseArray`StronglyConnectedComponents[mat];
	
	(* Sort elements on boundary by measure (area or volume) of their bounding box
	from largest to smallest. The largest should be the outside boundary. *)
	boundariesConn=Reverse@SortBy[
		Map[
			(Developer`ToPackedArray/@Part[incidents,#])&,
			components
		],
		boundingBoxMeasure[mesh["Coordinates"],#]&
	];
	
	markers=Join@@MapIndexed[
		ConstantArray[First@#2,#1]&,
		Length/@boundariesConn
	];
	
	ToElementMesh[
		"Coordinates"->mesh["Coordinates"],
		"MeshElements"->mesh["MeshElements"],
		"BoundaryElements"->{elementType[Join@@boundariesConn,markers]},
		"PointElements"->mesh["PointElements"]
	]
];


(* ::Subsubsection::Closed:: *)
(*SelectElements*)


(* Returned ElementMesh is without markers. *)
selectElementsByMarkers[mesh_ElementMesh,ints_List]:=Module[
	{elementType,head,types,connectivity,markers,selectedElements,
	selectedNodes,renumbering},
	
	{elementType,head}=If[
		mesh["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	types=Head/@mesh[elementType];
	connectivity=ElementIncidents@mesh[elementType];
	markers=ElementMarkers@mesh[elementType];
	
	selectedElements=MapThread[
		Pick[#1,#2,Alternatives@@ints]&,
		{connectivity,markers}
	];
	selectedNodes=Union@Flatten@selectedElements;
	renumbering=Thread[selectedNodes->Range@Length[selectedNodes]];

	head[
		"Coordinates" -> Part[mesh["Coordinates"],selectedNodes],
		elementType -> MapThread[#1[#2]&,{types,selectedElements/.renumbering}]
	]
];


selectElementsByCoordinates[mesh_ElementMesh,crit_Function]:=Module[
	{crds,elementType,head,elementHeads,connectivity,markers,renumbering,
	selectedNodes,selectedElementNumbers,selectedElements,selectedMarkers,elementTypeList},
	
	{elementType,head}=If[
		mesh["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	crds=mesh["Coordinates"];
	elementHeads=Head/@mesh[elementType];
	connectivity=ElementIncidents@mesh[elementType];
	markers=ElementMarkers@mesh[elementType];
	
	selectedNodes=Pick[Range@Length[crds],crit@@@crds];
	renumbering=Thread[selectedNodes->Range@Length[selectedNodes]];
	
	(* TODO: Try to write vectorized variant of element selector. *)
	selectedElementNumbers=MapIndexed[
		If[ContainsAll[selectedNodes,#1],Last@#2,Nothing]&,
		connectivity,
		{2}
	];
	(* This catches majority of bad crit Function(s), since no elements are selected.
	Message is issued in the top public function. *)
	If[
		MatchQ[selectedElementNumbers,{{}..}],
		Return[$Failed,Module]
	];
	
	selectedElements=MapThread[Part,{connectivity,selectedElementNumbers}]/.renumbering;
	selectedMarkers=MapThread[Part,{markers,selectedElementNumbers}];
	
	(* Quick hack to allow mixed mesh types. 
	It removes element type with empty connectivity vector. *)
	elementTypeList=Quiet@ReplaceAll[
		MapThread[
			#1[#2,#3]&,
			{elementHeads,selectedElements,selectedMarkers}
		],
		_[{},__]->Nothing
	];

	head[
		"Coordinates"->Part[crds,selectedNodes],
		elementType->elementTypeList
	]
];


SelectElements::usage=(
	"SelectElements[mesh, crit] creates ElementMesh made only out of nodes which match Function crit."<>"\n"<>
	"SelectElements[mesh, ElementMarker==m] select elements with marker m."
);
SelectElements::noelms="No elements in ElementMesh match the criterion.";
SelectElements::funslots="Criterion Function has too many Slots for ElementMesh with \"EmbeddingDimension\"==`1`.";
SelectElements::nomark="Specified marker `1` does not exist and unmodified ElementMesh is returned.";
SelectElements::intmark="ElementMarker should be non-negative integer.";

SelectElements//SyntaxInformation={"ArgumentsPattern"->{_,_}};

SelectElements[mesh_ElementMesh,crit_Function]:=Module[
	{dim,newMesh},
	
	(* Test for appropriate number of Slots in crit Function. *)
	dim=mesh["EmbeddingDimension"];
	If[
		Count[crit,_Slot,Infinity]>dim,
		Message[SelectElements::funslots,dim];Return[$Failed,Module]
	];
	
	newMesh=selectElementsByCoordinates[mesh,crit];
	If[
		Head[newMesh]===ElementMesh,
		newMesh,
		Message[SelectElements::noelms];$Failed
	]
];

(* Select elements by marker function. *)
SelectElements[mesh_ElementMesh,func_/;Not@FreeQ[func,ElementMarker]]:=Module[
	{ints,existingMarkers},
	
	ints=Flatten[{ElementMarker/.Solve[func, ElementMarker]}];
	If[
		Not@VectorQ[ints,IntegerQ],
		Message[SelectElements::intmark];Return[$Failed,Module]
	];
	
	existingMarkers=If[
		mesh["MeshElements"]===Automatic,
		mesh["BoundaryElementMarkerUnion"],
		mesh["MeshElementMarkerUnion"]
	];
	
	If[
		Not@MemberQ[existingMarkers,Alternatives@@ints],
		Message[SelectElements::nomark,ints];Return[mesh,Module]
	];
	
	selectElementsByMarkers[mesh,ints]
];


(* ::Subsubsection::Closed:: *)
(*TransformMesh*)


(* Helper function to reorder connectivity of elements (TriangleElement, etc.)
if transformation function has a negative jacobian. *)

transformElements[elementObj_List,reorderQ_]:=Map[
	transformElements[#,reorderQ]&,
	elementObj
];

transformElements[elementObj_,reorderQ_]:=Module[
	{head,connectivity,markers,length,numbering},
	
	head=Head@elementObj;
	connectivity=ElementIncidents@elementObj;
	markers=ElementMarkers@elementObj;
	length=Last@Dimensions[connectivity];
	(* Common renumbering for 1st and 2nd order. *)
	numbering=Take[
		head/.{
			TriangleElement->{2,1,3,4,6,5},
			QuadElement->{2,1,4,3,5,8,7,6},
			TetrahedronElement->{3,2,1,4,6,5,7,8,10,9},
			HexahedronElement->{2,1,4,3,6,5,8,7,9,12,11,10,13,16,15,14,18,17,20,19},
			LineElement->{2,1,3},
			PointElement->{1}
			},
		length
	];
	
	If[
		TrueQ[reorderQ],
		head[connectivity[[All,numbering]],markers],
		head[connectivity,markers]
	]
];


TransformMesh::usage="TransformMesh[mesh, tfun] transforms ElementMesh mesh according to TransformationFunction tfun";

TransformMesh//Options=Options@ElementMesh;

TransformMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,OptionsPattern[]}};

TransformMesh[mesh_ElementMesh,tfun_TransformationFunction,opts:OptionsPattern[]]:=Module[
	{reorderQ},
	
	(* If TransformationFunction represents reflection (or inversion), 
	then element incidents have to be reordered. *)
	reorderQ=Negative@Det[TransformationMatrix[tfun]];
	
	If[
		mesh["MeshElements"]===Automatic,
		ToBoundaryMesh[
			"Coordinates"->tfun/@mesh["Coordinates"],
			"BoundaryElements"->transformElements[mesh["BoundaryElements"],reorderQ],
			"PointElements"->transformElements[mesh["PointElements"],reorderQ],
			FilterRules[{opts},Options@ElementMesh]
		],
		ToElementMesh[
			"Coordinates"->tfun/@mesh["Coordinates"],
			"MeshElements"->transformElements[mesh["MeshElements"],reorderQ],
			"BoundaryElements"->transformElements[mesh["BoundaryElements"],reorderQ],
			"PointElements"->transformElements[mesh["PointElements"],reorderQ],
			FilterRules[{opts},Options@ElementMesh]
		]
	]
];


(* ::Subsubsection::Closed:: *)
(*MergeMesh*)


(* Code is adjusted after this: https://mathematica.stackexchange.com/q/156445 *)
MergeMesh::usage="MergeMesh[{mesh1,mesh2,...}] merges a list of ElementMesh objects with the same embedding dimension.";
MergeMesh::order="Meshes must have the same \"MeshOrder\".";
MergeMesh::dim="Meshes must have the same \"EmbeddingDimension\".";

MergeMesh//Options=Options@ElementMesh;

MergeMesh//SyntaxInformation={"ArgumentsPattern"->{_,OptionsPattern[]}};

MergeMesh[list_List/;Length[list]>=2,opts:OptionsPattern[]]:=Fold[MergeMesh[#1,#2,opts]&,list];

MergeMesh[mesh1_ElementMesh,mesh2_ElementMesh,opts:OptionsPattern[]]:=Module[
	{elementType,head,c1,c2,newCrds,newElements,elementTypes,elementMarkers,inci1,inci2},
	
	If[
		mesh1["MeshOrder"]=!=mesh2["MeshOrder"],
		Message[MergeMesh::order];Return[$Failed]
	];
	If[
		mesh1["EmbeddingDimension"]=!=mesh2["EmbeddingDimension"],
		Message[MergeMesh::dim];Return[$Failed]
	];
		
	{elementType,head}=If[
		mesh1["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	c1=mesh1["Coordinates"];
	c2=mesh2["Coordinates"];
	newCrds=Join[c1,c2];
	elementTypes=Join[Head/@mesh1[elementType],Head/@mesh2[elementType]];
	elementMarkers=ElementMarkers/@{mesh1[elementType],mesh2[elementType]};
	
	inci1=ElementIncidents[mesh1[elementType]];
	inci2=ElementIncidents[mesh2[elementType]]+Length[c1];
	(* If all elements are of the same type, then this type is specified only once. *)
	newElements=If[
		SameQ@@elementTypes,
		{First[elementTypes][Join[Join@@inci1,Join@@inci2],Flatten[elementMarkers]]},
		MapThread[#1[#2,#3]&,{elementTypes,Join[inci1,inci2],Join@@elementMarkers}]
	];
	
	head[
		"Coordinates"->newCrds,
		elementType->newElements,
		FilterRules[{opts},Options@ElementMesh]
	]
];


(* ::Subsubsection::Closed:: *)
(*ExtrudeMesh*)


ExtrudeMesh::usage="ExtrudeMesh[mesh, thickness, layers] extrudes 2D quadrilateral mesh to 3D hexahedron mesh.";
ExtrudeMesh::order="Only first order mesh is supported.";
ExtrudeMesh::eltype="Only mesh with QuadElement(s) is supported.";

ExtrudeMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_}};

ExtrudeMesh[mesh_ElementMesh,thickness_?Positive,layers_Integer?Positive]:=Module[
	{n,nodes2D,nodes3D,elements2D,elements3D,markers2D,markers3D},
	
	If[
		mesh["MeshOrder"]=!=1,
		Message[ExtrudeMesh::order];Return[$Failed,Module]
	];
	(* Mesh "EmbeddingDimension" is already checked with correct "MeshElements" type. *)
	If[
		(Head/@mesh["MeshElements"])=!={QuadElement},
		Message[ExtrudeMesh::eltype];Return[$Failed,Module]
	];
		
	nodes2D=mesh["Coordinates"];
	elements2D=First@ElementIncidents[mesh["MeshElements"]];
	markers2D=First@ElementMarkers[mesh["MeshElements"]];
	n=Length[nodes2D];
	
	nodes3D=Join@@Map[
		(Transpose@Join[Transpose[nodes2D],{ConstantArray[#,n]}])&,
		Subdivide[0.,thickness,layers]
	];

	elements3D=Join@@Table[
		Map[Join[n*(i-1)+#,n*i+#]&,elements2D],
		{i,layers}
	];

	markers3D=Flatten@ConstantArray[markers2D,layers];
	
	ToElementMesh[
		"Coordinates"->nodes3D,
		"MeshElements"->{HexahedronElement[elements3D,markers3D]}
	]
];


(* ::Subsubsection::Closed:: *)
(*Mesh smoothing*)


(* 
This implements Laplacian mesh smoothing method as described in 
https://mathematica.stackexchange.com/a/156669 
*)
SmoothenMesh::usage="SmoothenMesh[mesh] improves the quality of 2D mesh.";
SmoothenMesh::badType="Smoothing is only supported for first order 2D meshes.";

SmoothenMesh//SyntaxInformation={"ArgumentsPattern"->{_}};

SmoothenMesh[mesh_ElementMesh]:=Module[
	{n,vec,mat,adjacencyMatrix,mass,typoOpt,
	bndVertices,interiorvertices,stiffness,load,newCoords},
	
	If[
		Or[mesh["MeshOrder"]=!=1,mesh["EmbeddingDimension"]=!=2],
		Message[SmoothenMesh::badType];Return[$Failed]
	];
	
	n = Length[mesh["Coordinates"]];
	vec = mesh["VertexElementConnectivity"];
	mat = Unitize[vec.Transpose[vec]];
	vec = Null;
	adjacencyMatrix = mat - DiagonalMatrix[Diagonal[mat]];
	mass = DiagonalMatrix[SparseArray[Total[adjacencyMatrix, {2}]]];
	stiffness = N[mass - adjacencyMatrix];
	adjacencyMatrix = Null;
	mass = Null;
	
	bndVertices = Flatten[Join @@ ElementIncidents[mesh["PointElements"]]];
	interiorvertices = Complement[Range[1, n], bndVertices];
	
	stiffness[[bndVertices]] = IdentityMatrix[n, SparseArray][[bndVertices]];
	
	load = ConstantArray[0., {n, mesh["EmbeddingDimension"]}];
	load[[bndVertices]] = mesh["Coordinates"][[bndVertices]];
	newCoords = LinearSolve[stiffness, load];
	
	typoOpt = If[
		$VersionNumber <= 11.3,
		"CheckIncidentsCompletness" -> False,
		"CheckIncidentsCompleteness" -> False
	];
	
	ToElementMesh[
		"Coordinates" -> newCoords,
		"MeshElements" -> mesh["MeshElements"],
		"BoundaryElements" -> mesh["BoundaryElements"],
		"PointElements" -> mesh["PointElements"],
		typoOpt,
		"CheckIntersections" -> False,
		"DeleteDuplicateCoordinates" -> False,
		"RegionHoles" -> mesh["RegionHoles"]
	]
];


(* ::Subsubsection::Closed:: *)
(*QuadToTriangleMesh*)


(* Split a quadrilateral over its shorter diagonal. This makes triangles with better quality. *)
smartSplit[crds_,connectivity_]:=Module[
	{pts,split},
	
	pts=Part[crds,#]&/@connectivity;
	split=If[
		TrueQ[Norm[#1[[1]]-#1[[3]]]>Norm[#1[[2]]-#1[[4]]]],
		{{#2[[1]],#2[[2]],#2[[4]]},{#2[[2]],#2[[3]],#2[[4]]}},
		{{#2[[1]],#2[[2]],#2[[3]]},{#2[[1]],#2[[3]],#2[[4]]}}
	]&;
	Join@@MapThread[split,{pts,connectivity}]
];


QuadToTriangleMesh::usage="QuadToTriangleMesh[mesh] converts quadrilateral mesh to triangular mesh.";
QuadToTriangleMesh::order="Only the first order mesh is currently supported.";

QuadToTriangleMesh//Options={"SplitDirection"->Automatic};

QuadToTriangleMesh//SyntaxInformation={"ArgumentsPattern"->{_,OptionsPattern[]}};

QuadToTriangleMesh[mesh_ElementMesh,opts:OptionsPattern[]]:=Module[
	{method,elementType,head,crds,connectivity,markers,triangles,triMarkers},
	
	method=OptionValue["SplitDirection"]/.(Except[Left|Right|Automatic]->Automatic);
	
	If[
		mesh["MeshOrder"]=!=1,
		Message[QuadToTriangleMesh::order];Return[$Failed,Module]
	];
	
	{elementType,head}=If[
		mesh["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	crds=mesh["Coordinates"];
	connectivity=ElementIncidents@First@mesh[elementType];
	markers=ElementMarkers@First@mesh[elementType];
	
	triangles=Switch[method,
		Left,
		Join@@({#[[{1,2,3}]],#[[{1,3,4}]]}&/@connectivity),
		Right,
		Join@@({#[[{1,2,4}]],#[[{2,3,4}]]}&/@connectivity),
		Automatic,
		smartSplit[crds,connectivity]
	];
	triMarkers=Join@@Map[{#,#}&,markers];

	head[
		"Coordinates"->crds,
		elementType->{TriangleElement[triangles,triMarkers]}
	]
];


(* ::Subsubsection::Closed:: *)
(*HexToTetrahedronMesh*)


HexToTetrahedronMesh::usage="HexToTetrahedronMesh[mesh] converts hexahedral mesh to tetrahedral mesh.";
HexToTetrahedronMesh::type="ElementMesh should contain only hexadedral elements.";

HexToTetrahedronMesh//SyntaxInformation={"ArgumentsPattern"->{_}};

(* This function returns valid mesh only on properly structured hex meshes. *)
HexToTetrahedronMesh[mesh_ElementMesh]:=Module[
	{connectivity,markers,tetConnect,restructure,newConnectivity,newMarkers},
	
	If[
		Head/@mesh["MeshElements"]=!={HexahedronElement},
		Message[HexToTetrahedronMesh::type];Return[$Failed,Module]
	];
	
	connectivity=ElementIncidents@First[mesh["MeshElements"]];
	markers=ElementMarkers@First[mesh["MeshElements"]];
	
	tetConnect={
		{1,2,4,5},
		{2,4,5,8},
		{2,5,6,8},
		{2,8,6,3},
		{2,3,4,8},
		{3,8,6,7}
	};
	restructure=Function[
		{hexNodes},
		Map[Part[hexNodes,#]&,tetConnect]
	];
	
	newConnectivity=Join@@Map[restructure,connectivity];
	newMarkers=Join@@Map[
		ConstantArray[#,Length[tetConnect]]&,
		markers
	];
	
	ToElementMesh[
		"Coordinates"->mesh["Coordinates"],
		"MeshElements"->{TetrahedronElement[newConnectivity,newMarkers]}
	]
];


(* ::Subsubsection::Closed:: *)
(*TriangleToQuadMesh*)


distortion:=distortion=Compile[
	{{n1, _Real, 1}, {n2, _Real, 1}, {n3, _Real, 1}, {n4, _Real, 1}},
	2/Pi * Max @ Map[
		Abs[Pi/2 - If[ # < 0, Pi - #, #] ]& @ 
			ArcTan[#[[1, 1]] #[[2, 1]] + #[[1, 2]] #[[2, 2]],
					#[[1, 1]] #[[2, 2]] - #[[1, 2]] #[[2, 1]]
				]&,
		{{n2-n1, n4-n1}, {n3-n2, n1-n2}, {n4-n3, n2-n3}, {n1-n4, n3-n4}}
	]
];


(*
Source of algorithm for mesh conversion is:
	Houman Borouchaki, Pascal J. Frey;
	Adaptive triangular\[Dash]quadrilateral mesh generation;
	International Journal for Numerical Methods in Engineering;
	1998; Vol. 41, p. (915-934)

Original implementation is by Prof. Joze Korelc taken from AceFEM package 
(http://symech.fgg.uni-lj.si/). Layout of function and other properties
have been improved by Oliver Ruebenkoenig from WRI.
*)
TriangleToQuadMesh::usage="TriangleToQuadMesh[mesh] converts triangular mesh to quadrilateral mesh.";
TriangleToQuadMesh::elmtype = "Only conversion of pure triangular meshes is supported.";

TriangleToQuadMesh//SyntaxInformation={"ArgumentsPattern"->{_}};

TriangleToQuadMesh[meshIn_] /; ElementMeshQ[meshIn] && !BoundaryElementMeshQ[meshIn] &&
	meshIn["EmbeddingDimension"] === 2 := Module[
	{coor, econn, elem, dist, ncoor, taken, quad, triag, edge, nc, allquads, 
	alltriag, marker, nodes, markerQ, mesh, increaseOrderQ},

	mesh = meshIn;
	
	If[
		mesh["MeshOrder"] =!= 1,
		mesh = MeshOrderAlteration[mesh, 1]
	];
		increaseOrderQ = True;

	If[ Union[Head /@ mesh["MeshElements"]] =!= {TriangleElement},
		Message[TriangleToQuadMesh::elmtype]; Return[$Failed, Module]
	];

	coor = mesh["Coordinates"];
	econn = Join @@ mesh["ElementConnectivity"];
	(* zero is used as a default marker *)
	marker = Join @@ ElementMarkers[ mesh["MeshElements"]];
	markerQ = ElementMarkersQ[mesh["MeshElements"]];
	elem = Join @@ ElementIncidents[ mesh["MeshElements"]];	
	
	dist = MapThread[ (
		{
		If[ #2[[1]] == 0,
			Nothing,
			distortion[Sequence@@coor[[nodes = {#[[1]], #[[2]], Complement[elem[[#2[[1]]]], #1][[1]], #[[3]]}]]]->{#3, #2[[1]], nodes}
		],
		If[ #2[[2]] == 0,
			Nothing,
			distortion[Sequence@@coor[[nodes = {#[[1]], #[[2]], #[[3]], Complement[elem[[#2[[2]]]], #1][[1]]}]]]->{#3, #2[[2]], nodes}
		],
		If[ #2[[3]] == 0,
			Nothing,
			distortion[Sequence@@coor[[nodes = {#[[1]], Complement[elem[[#2[[3]]]], #1][[1]], #[[2]], #[[3]]}]]]->{#3, #2[[3]], nodes}
		]
		})&,
		{elem, econn, Range[elem//Length]}
	]//Flatten;

	taken = ConstantArray[False, elem//Length];
	quad = Map[
		If[ 
			Or@@taken[[#[[2, {1, 2}]]]]  || #[[1]]>0.8 || marker[[#[[2, 1]]]]  =!= marker[[#[[2, 2]]]],
			Nothing,
			taken[[#[[2, {1, 2}]]]] = True;{#[[2, 3]], marker[[#[[2, 1]]]]}
		]&,
		Sort[dist]
	]//Transpose;
	
	dist = Null;
	triag = {Extract[elem, #], Extract[marker, #]}& @ Position[taken, False];
	edge = DeleteDuplicates[
		Join[
			Flatten[Map[{#[[{1, 2}]]//Sort, #[[{2, 3}]]//Sort, #[[{3, 4}]]//Sort, #[[{4, 1}]]//Sort} &, quad[[1]]], 1],
			Flatten[Map[{#[[{1, 2}]]//Sort, #[[{2, 3}]]//Sort, #[[{3, 1}]]//Sort} &, triag[[1]]], 1]
		]
	];
	
	ncoor = coor//Length;
	nc = Dispatch[Flatten[Map[(ncoor++;{#->ncoor, #[[{2, 1}]]->ncoor})&, edge]]];
	
	allquads = MapThread[
		(ncoor++; 
		{Total[coor[[#]]]/4,
			{
			{#[[1]], #[[{1, 2}]]/.nc, ncoor, #[[{4, 1}]]/.nc},
			{#[[{1, 2}]]/.nc, #[[2]], #[[{2, 3}]]/.nc, ncoor},
			{ncoor, #[[{2, 3}]]/.nc, #[[3]], #[[{3, 4}]]/.nc},
			{#[[{4, 1}]]/.nc, ncoor, #[[{3, 4}]]/.nc, #[[4]]}
			},
		{#2, #2, #2, #2}
		})& ,
		quad
	];
	
	alltriag = MapThread[
		(ncoor++;
		{Total[coor[[#]]]/3,
			{
			{#[[1]], #[[{1, 2}]]/.nc, ncoor, #[[{3, 1}]]/.nc},
			{#[[{1, 2}]]/.nc, #[[2]], #[[{2, 3}]]/.nc, ncoor},
			{ncoor, #[[{2, 3}]]/.nc, #[[3]], #[[{3, 1}]]/.nc}
			},
		{#2, #2, #2}})&, 
		triag
	];

	mesh = ToElementMesh[
				"Coordinates" -> Join[
					coor,
					Map[(coor[[#[[1]]]]+coor[[#[[2]]]])/2&, edge],
					allquads[[All, 1]], alltriag[[All, 1]]
				], 
				"MeshElements" -> If[ markerQ,
					{QuadElement[Flatten[Join[
						allquads[[All, 2]],
						alltriag[[All, 2]]], 1],
						Flatten[{allquads[[All, 3]], alltriag[[All, 3]]}]]},
					{QuadElement[Flatten[Join[
						allquads[[All, 2]], alltriag[[All, 2]]], 1]]}
				],
				"RegionHoles" -> meshIn["RegionHoles"]
			];

	If[ !ElementMeshQ[mesh], Return[ $Failed, Module]];

	If[ increaseOrderQ, mesh = MeshOrderAlteration[mesh, meshIn["MeshOrder"]]];

	mesh
];


(* ::Subsection::Closed:: *)
(*Mesh measurements*)


(* ::Subsubsection::Closed:: *)
(*MeshElementMeasure*)


elementMeasure[LineElement[connectivity_,___],crds_,order_]:=Module[
	{elementByCrds},
	
	elementByCrds=Partition[
		N[crds][[Flatten@Part[connectivity,All,{1,2}]]],
		2
	];
	
	(* Additional partition is used to match the output of mesh["MeshElementMeasure"]. *)
	Partition[
		Abs[Subtract@@@Part[elementByCrds,All,All,1]],
		1
	]
];

elementMeasure[element_,crds_,order_]:=Module[
	{noNodes,noDim,type,nodes,igCrds,igWgts,shapeDerivative,jacobian,pts,volumeFun,elementByCrds},
	
	noNodes=Last@Dimensions[ElementIncidents@element];
	noDim=Last@Dimensions[crds];
	type=Head@element;
	
	igCrds=ElementIntegrationPoints[type,order];
	igWgts=ElementIntegrationWeights[type,order];
	shapeDerivative=ElementShapeFunctionDerivative[type,order];
	(* First argument name of GetElement has to match Compile function argument name. *)
	nodes=Table[Compile`GetElement[pts,i,j],{i,1,noNodes},{j,1,noDim}];
	
	(* Calculate symbolic function for numerical integration and pass it to Compile. *)
	jacobian=Function[
		Evaluate@Det[(shapeDerivative@@Take[{#1,#2,#3},noDim]).nodes]
	];
	
	volumeFun=With[
		{code=(jacobian@@@igCrds).igWgts},
		Compile[
			{{pts,_Real,2}},
			code,
			RuntimeAttributes->{Listable},
			Parallelization->True
		]
	];
	(* A very fast method to get element by coordinates list. *)
	elementByCrds=Partition[
		crds[[Flatten@ElementIncidents@element]],
		noNodes
	];
	
	volumeFun[elementByCrds]
];


(* This function gives the same result as asking for the property mesh["MeshElementMeasure"] *)
MeshElementMeasure::usage="MeshElementMeasure[mesh] gives the measure of each mesh element.";
MeshElementMeasure::meshelements="Given ElementMesh doesn't contain any \"MeshElements\".";

MeshElementMeasure//SyntaxInformation={"ArgumentsPattern"->{_}};

MeshElementMeasure[mesh_ElementMesh]:=Module[
	{order,elements,crds},
	
	order=mesh["MeshOrder"];
	elements=mesh["MeshElements"];
	crds=mesh["Coordinates"];
	
	If[
		Not@ListQ[elements],
		Message[MeshElementMeasure::meshelements];Return[$Failed,Module]
	];
		
	elementMeasure[#,crds,order]&/@elements
];


(* ::Subsubsection::Closed:: *)
(*BoundaryElementMeasure*)


boundaryMeasure[element_,crds_,order_]:=Module[
	{noNodes,noDim,type,nodes,igCrds,igWgts,shapeDerivative,area,pts,areaFun,elementByCrds},
	
	noNodes=Last@Dimensions[ElementIncidents@element];
	noDim=Last@Dimensions[crds];
	type=Head@element;
	
	igCrds=ElementIntegrationPoints[type,order];
	igWgts=ElementIntegrationWeights[type,order];
	shapeDerivative=ElementShapeFunctionDerivative[type,order];
	(* First argument name of GetElement has to match Compile function argument name. *)
	nodes=Table[Compile`GetElement[pts,i,j],{i,1,noNodes},{j,1,noDim}];
	
	(* Calculate symbolic function for numerical integration and pass it to Compile. *)
	area=Function[
		Evaluate@Norm[
			Cross@@(shapeDerivative[#1,#2,#3].nodes)
		]
	];
	
	areaFun=With[
		{code=(area@@@igCrds).igWgts},
		Compile[
			{{pts,_Real,2}},
			code,
			RuntimeAttributes->{Listable},
			Parallelization->True
		]
	];
	(* A very fast method to get element by coordinates list. *)
	elementByCrds=Partition[
		crds[[Flatten@ElementIncidents@element]],
		noNodes
	];
	
	areaFun[elementByCrds]
];


(* This function returns the surface of boundary elements in 3D embedding and length of 
boundary elements in 2D embedding. *)
BoundaryElementMeasure::usage="BoundaryElementMeasure[mesh] gives the measure of each boundary element.";

BoundaryElementMeasure//SyntaxInformation={"ArgumentsPattern"->{_}};

BoundaryElementMeasure[mesh_ElementMesh]:=Module[
	{order,elements,crds},
	
	order=mesh["MeshOrder"];
	elements=mesh["BoundaryElements"];
	crds=mesh["Coordinates"];
	
	boundaryMeasure[#,crds,order]&/@elements
];


(* ::Subsection::Closed:: *)
(*Mesh visualization*)


(* 
Function to visualize 2D second order FEM mesh. 
Copied from: WTC 2017 talk "Finite Element Method: How to talk to it and make it your friend" by Paritosh Mokhasi 
*)
ElementMeshCurvedWireframe::usage="ElementMeshCurvedWireframe[mesh] plots second order mesh with curved edges.";

ElementMeshCurvedWireframe//SyntaxInformation={"ArgumentsPattern"->{_}};

ElementMeshCurvedWireframe[mesh_ElementMesh]:=Module[
	{triIncidentsOrder,quadIncidentsOrder,interpolatingQuadBezierCurve,
	interpolatingQuadBezierCurveComplex,triEdges,triIncidents,coordinates},

	triIncidentsOrder={1,4,2,5,3,6};
	quadIncidentsOrder={1,5,2,6,3,7,4,8};

	interpolatingQuadBezierCurve[pts_List]/;Length[pts]==3:=
		BezierCurve[{pts[[1]],(1/2)(-pts[[1]]+4 pts[[2]]-pts[[3]]),pts[[3]]}];
	interpolatingQuadBezierCurve[pts_List,"ControlPoints"]/;(Length[pts]==3):=
		{pts[[1]],(1/2)(-pts[[1]]+4 pts[[2]]-pts[[3]]),pts[[3]]};
	interpolatingQuadBezierCurve[ptslist_List]:=interpolatingQuadBezierCurve/@ptslist;

	interpolatingQuadBezierCurveComplex[coords_,indices_]:=interpolatingQuadBezierCurve[
		Map[coords[[#]]&,indices]
	];

	triEdges=Partition[triIncidentsOrder, 3, 2, {1,1}];
	triIncidents=Join@@ElementIncidents[mesh["MeshElements"]];
	coordinates=mesh["Coordinates"];
	
	Graphics[{
		interpolatingQuadBezierCurveComplex[coordinates,triIncidents[[All, #]]]&/@triEdges
	}]
];


(* ::Subsection::Closed:: *)
(*Structured mesh*)


getElementConnectivity[nx_,ny_]:=Flatten[
	Table[{
		j+(i-1)(ny+1),
		j+i(ny+1),
		j+i(ny+1)+1,
		j+(i-1)(ny+1)+1
		},
		{i,1,nx},
		{j,1,ny}
	],
	1
];

getElementConnectivity[nx_,ny_,nz_]:=Flatten[
	Table[{
		k+(j-1)(nz+1)+(i-1)(nz+1)(ny+1),
		k+(j-1)(nz+1)+i(nz+1)(ny+1),
		k+j(nz+1)+i(nz+1)(ny+1),
		k+j(nz+1)+(i-1)(nz+1)(ny+1),
		k+(j-1)(nz+1)+(i-1)(nz+1)(ny+1)+1,
		k+(j-1)(nz+1)+i(nz+1)(ny+1)+1,
		k+j(nz+1)+i(nz+1)(ny+1)+1,
		k+j(nz+1)+(i-1)(nz+1)(ny+1)+1
		},
		{i,1,nx},
		{j,1,ny},
		{k,1,nz}
	],
	2
];


internalNodesQuad[{nx_,ny_},int_,side_]:=Module[
	{perm,p},
	perm=Switch[side,
		Bottom,Flatten@Table[4*(i-1)+{1,3,4,2},{i,nx}],
		Left,Flatten@Table[4*(i-1)+{1,2,4,3},{i,ny}],
		Top,Flatten@Table[4*(i-1)+{1,3,4,2},{i,nx}],
		Right,Flatten@Table[4*(i-1)+{1,2,4,3},{i,ny}]
	];
	p=Switch[side,
		Bottom,Flatten[Drop[Table[
			{i+1./(3*nx),j},
			{i,0,1-2./(3*nx),1./(3*nx)},
			{j,{0,1/(2*ny)}}
		],{3,-1,3}],1][[perm]],
		Left,Flatten[Drop[Table[
			{i,j+1/(3*ny)},
			{j,0,1-2./(3*ny),1./(3*ny)},
			{i,{0,1./(2*nx)}}
		],{3,-1,3}],1][[perm]],
		Top,Flatten[Drop[Table[
			{i+1./(3*nx),j},
			{i,0,1-2./(3*nx),1./(3*nx)},
			{j,{(ny-1)/ny+1./(2*ny),1}}
		],{3,-1,3}],1][[perm]],
		Right,Flatten[Drop[Table[
			{i,j+1./(3*ny)},
			{j,0,1-2./(3*ny),1./(3*ny)},
			{i,{(nx-1)/nx+1./(2*nx),1}}
		],{3,-1,3}],1][[perm]],
		None,{},
		_,Message[StructuredMesh::refinement];Return[$Failed,Module]
	];
	Through/@MapThread[int,Transpose@p]
];

generateCorner2D[{dx_,dy_},position_]:=Module[
	{coor},
	coor={
		{0*dx,0*dy},
		{1*dx,0*dy},
		{2*dx,0*dy},
		{0*dx,1*dy},
		{1*dx,1*dy},
		{2*dx,1*dy},
		{0*dx,2*dy},
		{1*dx,2*dy},
		{2*dx,2*dy}
	};
	Switch[position,
		1,Delete[coor,{{1},{9}}],
		2,Transpose[Transpose@Delete[coor,{{3},{7}}]+{1-2*dx,0}],
		3,Transpose[Transpose@Delete[coor,{{3},{7}}]+{0,1-2*dy}],
		4,Transpose[Transpose@Delete[coor,{{1},{9}}]+{1-2*dx,1-2*dy}]
	]
];

internalNodesQuadCorner[{nx_,ny_},int_,cornerElement_]:=Module[
	{p},
	p=Switch[
		cornerElement,
		(nx-1)*ny+1,generateCorner2D[{1/(3*nx),1/(3*ny)},2],
		nx*ny,generateCorner2D[{1/(3*nx),1/(3*ny)},4],
		ny,generateCorner2D[{1/(3*nx),1/(3*ny)},3],
		1,generateCorner2D[{1/(3*nx),1/(3*ny)},1]
	];
	Through/@MapThread[int,Transpose@p]
];


structuredMeshEdgeElements[{nx_Integer,ny_Integer},side_]:=Switch[
	side,
	Left,Range[1,ny,1],
	Right,Range[ny*(nx-1)+1,ny*nx,1],
	Top,Range[ny,ny*nx,ny],
	Bottom,Range[1,ny*(nx-1)+1,ny],
	None,{},
	_,Message[StructuredMesh::refinement];Return[$Failed,Module]
];

structuredMeshCornerElements[{nx_Integer,ny_Integer},sides_]:=Which[
	SubsetQ[sides,{Bottom,Right,Top,Left}],{1,1+(nx-1)*ny,nx*ny,ny},
	SubsetQ[sides,{Left,Bottom,Right}],{1,1+(nx-1)*ny},
	SubsetQ[sides,{Bottom,Right,Top}],{1+(nx-1)*ny,nx*ny},
	SubsetQ[sides,{Right,Top,Left}],{nx*ny,ny},
	SubsetQ[sides,{Top,Left,Bottom}],{1,ny},
	SubsetQ[sides,{Bottom,Right}],{1+(nx-1)*ny},
	SubsetQ[sides,{Top,Right}],{nx*ny},
	SubsetQ[sides,{Top,Left}],{ny},
	SubsetQ[sides,{Bottom,Left}],{1},
	True,{}
]


insertedElementNodes[nodes_,addedNodes_]:={
	{nodes[[1]],nodes[[2]],addedNodes[[2]],addedNodes[[1]]},
	{nodes[[2]],nodes[[3]],addedNodes[[3]],addedNodes[[2]]},
	{nodes[[3]],nodes[[4]],addedNodes[[4]],addedNodes[[3]]},
	{nodes[[4]],nodes[[1]],addedNodes[[1]],addedNodes[[4]]},
	addedNodes
};

insertedElementNodes[nodes_,addedNodes_,side_]:=Module[
	{index},
	index=Switch[side,
		Bottom,1,
		Right,2,
		Top,3,
		Left,4
	];
	Delete[insertedElementNodes[nodes,addedNodes],index]
];

insertedElementCornerNodes[nodes_,cornerElement_,{nx_Integer,ny_Integer},totalNodes_]:=Switch[
	cornerElement,
	(nx-1)*ny+1,{
		{nodes[[1]],totalNodes+1,totalNodes+3,nodes[[4]]},
		{totalNodes+1,totalNodes+2,totalNodes+4,totalNodes+3},
		{totalNodes+2,nodes[[2]],totalNodes+5,totalNodes+4},
		{totalNodes+3,totalNodes+4,totalNodes+6,nodes[[4]]},
		{totalNodes+4,totalNodes+5,totalNodes+7,totalNodes+6},
		{totalNodes+6,totalNodes+7,nodes[[3]],nodes[[4]]}
	},
	nx*ny,{
		{nodes[[1]],totalNodes+1,totalNodes+4,totalNodes+3},
		{nodes[[1]],nodes[[2]],totalNodes+2,totalNodes+1},
		{totalNodes+1,totalNodes+2,totalNodes+5,totalNodes+4},
		{nodes[[1]],totalNodes+3,totalNodes+6,nodes[[4]]},
		{totalNodes+3,totalNodes+4,totalNodes+7,totalNodes+6},
		{totalNodes+4,totalNodes+5,nodes[[3]],totalNodes+7}
	},
	ny,{
		{nodes[[1]],nodes[[2]],totalNodes+2,totalNodes+1},
		{totalNodes+1,totalNodes+2,totalNodes+4,totalNodes+3},
		{totalNodes+2,nodes[[2]],totalNodes+5,totalNodes+4},
		{totalNodes+3,totalNodes+4,totalNodes+6,nodes[[4]]},
		{totalNodes+4,totalNodes+5,totalNodes+7,totalNodes+6},
		{totalNodes+7,totalNodes+5,nodes[[2]],nodes[[3]]}
	},
	1,{
		{nodes[[1]],totalNodes+1,totalNodes+4,totalNodes+3},
		{totalNodes+1,totalNodes+2,totalNodes+5,totalNodes+4},
		{totalNodes+2,nodes[[2]],nodes[[3]],totalNodes+5},
		{totalNodes+3,totalNodes+4,totalNodes+7,totalNodes+6},
		{totalNodes+4,totalNodes+5,nodes[[3]],totalNodes+7},
		{totalNodes+6,totalNodes+7,nodes[[3]],nodes[[4]]}
	}
];


structuredMeshRefinement[int_,{nx_,ny_},sides_,nodesList_,connectivityList_]:=Module[
	{rfNodes,edgeElements,rfElements,sideIndex,rfNodesIndices,
	cornerElements,pos,deletedIndices,nodes,connectivity,
	elementsToDelete},
	
	nodes=nodesList;
	connectivity=connectivityList;
	
	rfNodes={};
	edgeElements={};
	rfElements={};
	sideIndex={};
	
	Do[
		AppendTo[rfNodes,internalNodesQuad[{nx,ny},int,side]];
		AppendTo[edgeElements,structuredMeshEdgeElements[{nx,ny},side]];
		AppendTo[sideIndex,ConstantArray[side,Length@edgeElements[[-1]] ] ],
		{side,sides}
	];
	
	rfNodes=Flatten[rfNodes,1];
	sideIndex=Flatten@sideIndex;
	edgeElements=Flatten@edgeElements;
	
	rfNodesIndices=ArrayReshape[
		Range[(nx+1)*(ny+1)+1,(nx+1)*(ny+1)+1+Length@edgeElements*4],
		{Length@edgeElements,4}
	];
	
	cornerElements=structuredMeshCornerElements[{nx,ny},sides];
	pos={};
	Do[
		AppendTo[pos,Position[edgeElements,corner]],
		{corner,cornerElements}
	];
	pos=Flatten[pos,1];
	
	edgeElements=Delete[edgeElements,pos];
	sideIndex=Delete[sideIndex,pos];
	
	pos=Flatten@pos;
	
	deletedIndices=Flatten@rfNodesIndices[[pos]];
	deletedIndices=ArrayReshape[deletedIndices,{Length@deletedIndices,1}];
	
	rfNodes=Delete[rfNodes,deletedIndices-(nx+1)*(ny+1)];
	
	rfNodesIndices=ArrayReshape[
		Range[(nx+1)*(ny+1)+1,(nx+1)*(ny+1)+1+Length@edgeElements*4],
		{Length@edgeElements,4}
	];
	
	Do[
		AppendTo[rfElements,insertedElementNodes[
			connectivity[[edgeElements[[i]] ]],
			rfNodesIndices[[i]],
			sideIndex[[i]]
		]],
		{i,Length@edgeElements}
	];
	rfElements=Flatten[rfElements,1];
	
	nodes=Join[nodes,rfNodes];
	connectivity=Join[connectivity,rfElements];
	
	Do[
		connectivity=Join[
			connectivity,
			insertedElementCornerNodes[connectivity[[corner]],corner,{nx,ny},Length@nodes]
		];
		nodes=Join[nodes,internalNodesQuadCorner[{nx,ny},int,corner]],
		{corner,cornerElements}
	];
	
	elementsToDelete=Join[edgeElements,cornerElements];
	elementsToDelete=ArrayReshape[elementsToDelete,{Length@elementsToDelete,1}];
	
	connectivity=Delete[connectivity,elementsToDelete];
	
	{nodes,connectivity}
	
];


(* This function may cause shadowing with the same function in FEMAddOns paclet (https://github.com/WolframResearch/FEMAddOns). *)

StructuredMesh::usage=(
	"StructuredMesh[raster,{nx,ny}] creates structured mesh of quadrilaterals."<>"\n"<>
	"StructuredMesh[raster,{nx,ny,nz}] creates structured mesh of hexahedra."
);

StructuredMesh::array="Raster of input points must be full array of numbers with depth of `1`.";
StructuredMesh::refinement="\"Refinement\" option takes as argument {Left, Right, Top, Bottom, Front, Back, None}.";

StructuredMesh//Options={InterpolationOrder->1, "Refinement"->{}};

StructuredMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,OptionsPattern[]}};

StructuredMesh[raster_,{nx_,ny_},opts:OptionsPattern[]]:=Module[
	{order,dim,restructured,xInt,yInt,zInt,nodes,connectivity,
	rfNodes,rfNodesIndices,edgeElements,rfElements,sideIndex,
	cornerElements,pos,deletedIndices},
	If[
		Not@ArrayQ[raster,3,NumericQ],
		Message[StructuredMesh::array,3+1];Return[$Failed,Module]
	];
	
	order=OptionValue[InterpolationOrder]/.Automatic->1;
	dim=Last@Dimensions[raster];
	
	restructured=Transpose[raster,{3,2,1}];
	xInt=ListInterpolation[restructured[[1]],{{0,1},{0,1}},InterpolationOrder->order];
	yInt=ListInterpolation[restructured[[2]],{{0,1},{0,1}},InterpolationOrder->order];
	
	nodes=Join@@If[
		dim==3,
		zInt=ListInterpolation[restructured[[3]],{{0,1},{0,1}},InterpolationOrder->order];
		Table[{xInt[i,j],yInt[i,j],zInt[i,j]},{i,0,1,1./nx},{j,0,1,1./ny}],
		Table[{xInt[i,j],yInt[i,j]},{i,0,1,1./nx},{j,0,1,1./ny}]
	];
	connectivity=getElementConnectivity[nx,ny];
	
	Switch[dim,
	2,If[OptionValue["Refinement"]!={},
		{nodes,connectivity}=structuredMeshRefinement[
			{xInt,yInt},{nx,ny},
			OptionValue["Refinement"],
			nodes,connectivity
		]
	],
	3,If[OptionValue["Refinement"]!={},
		{nodes,connectivity}=structuredMeshRefinement[
			{xInt,yInt,zInt},{nx,ny},
			OptionValue["Refinement"],
			nodes,connectivity
		]
	]];
	
	
	If[
		dim==3,
		ToBoundaryMesh[
			"Coordinates"->nodes,
			"BoundaryElements"->{QuadElement[connectivity]}
		],
		ToElementMesh[
			"Coordinates"->nodes,
			"MeshElements"->{QuadElement[connectivity]}
		]
	]
];

StructuredMesh[raster_,{nx_,ny_,nz_},opts:OptionsPattern[]]:=Module[
	{order,restructured,xInt,yInt,zInt,nodes,connectivity},
	
	If[
		Not@ArrayQ[raster,4,NumericQ],
		Message[StructuredMesh::array,4+1];Return[$Failed,Module]
	];
	
	order=OptionValue[InterpolationOrder]/.Automatic->1;
	
	restructured=Transpose[raster,{4, 3, 2, 1}];
	xInt=ListInterpolation[restructured[[1]],{{0,1},{0,1},{0,1}},InterpolationOrder->order];
	yInt=ListInterpolation[restructured[[2]],{{0,1},{0,1},{0,1}},InterpolationOrder->order];
	zInt=ListInterpolation[restructured[[3]],{{0,1},{0,1},{0,1}},InterpolationOrder->order];
	
	nodes=Flatten[
		Table[
			{xInt[i,j,k],yInt[i,j,k],zInt[i,j,k]},
			{i,0,1,1./nx},{j,0,1,1./ny},{k,0,1,1./nz}
		],
		2
	];
	
	connectivity=getElementConnectivity[nx,ny,nz];
	
	ToElementMesh[
		"Coordinates"->nodes,
		"MeshElements"->{HexahedronElement[connectivity]}
	]
];


(* ::Subsection::Closed:: *)
(*Named meshes 2D*)


(* ::Subsubsection::Closed:: *)
(*RectangleMesh*)


RectangleMesh::usage="RectangleMesh[{xMin, yMin},{xMax, yMax},{nx, ny}] creates structured mesh 
on axis-aligned Rectangle with corners {xMin,yMin} and {xMax,yMax}.";

RectangleMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_}};

RectangleMesh[n_Integer]:=RectangleMesh[{0,0},{1,1},{n,n}];

RectangleMesh[{x1_,y1_},{x2_,y2_},{nx_Integer,ny_Integer}]:=StructuredMesh[
	{{{x1,y1},{x2,y1}},{{x1,y2},{x2,y2}}},
	{nx,ny}
];


(* ::Subsubsection::Closed:: *)
(*TriangleMesh*)


unitTriangleToQuads[n_Integer]:=Module[
	{n1,n2,n3,x,connectivity},
	
	{n1,n2,n3}={{0,0},{1,0},{0,1}};
	
	x=Join[{n1,n2,n3},Mean/@{{n1,n2},{n2,n3},{n3,n1},{n1,n2,n3}}];
	connectivity={
		{{x[[1]],x[[4]]},{x[[6]],x[[7]]}},
		{{x[[4]],x[[2]]},{x[[7]],x[[5]]}},
		{{x[[6]],x[[7]]},{x[[3]],x[[5]]}}
	};
	MergeMesh@Map[
		StructuredMesh[#,{n,n}]&,
		connectivity
	]
];


unitTriangleToTriangles[n_Integer]:=Module[
	{},
	SelectElements[
		QuadToTriangleMesh[
			RectangleMesh[{0,0},{1,1},{n,n}],
			"SplitDirection"->Right
		],
		#1+#2<=1&
	]
];


TriangleMesh::usage="TriangleMesh[{p1, p2, p3}, n] creates triangular mesh on Triangle with corners p1, p2 and p3.";
TriangleMesh::quadelms="Only even number of elements per edge is allowed for \"MeshElementType\"->QuadElement.";
TriangleMesh::badtype="Unknown option value for \"MeshElementType\"->`1`.";

TriangleMesh//Options={"MeshElementType"->QuadElement};

TriangleMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,OptionsPattern[]}};

TriangleMesh[n_Integer?Positive,opts:OptionsPattern[]]:=TriangleMesh[{{0,0},{1,0},{0,1}},n,opts];

TriangleMesh[{p1_,p2_,p3_},n_Integer?Positive,opts:OptionsPattern[]]:=Module[
	{type,unitMesh,tf},
	
	type=OptionValue["MeshElementType"];

	If[
		(type===QuadElement)&&OddQ[n],
		Message[TriangleMesh::quadelms];Return[$Failed,Module]
	];
	
	unitMesh=Switch[type,
		TriangleElement,unitTriangleToTriangles[n],
		QuadElement,unitTriangleToQuads[n/2],
		_,Message[TriangleMesh::badtype,type];Return[$Failed,Module]
	];
	
	tf=Threshold/@Last@FindGeometricTransform[
		{p1,p2,p3},
		{{0,0},{1,0},{0,1}},
		Method->"Linear"
	];
	
	TransformMesh[unitMesh,tf]
];


(* ::Subsubsection::Closed:: *)
(*DiskMesh*)


diskMeshProjection[{x_,y_},r_,n_Integer/;(n>=2)]:=Module[
	{square,rescale,coordinates},
	
	rescale=(Max[Abs@#]*Normalize[#])&;
	(* This special raster makes all element edges on disk edge of the same length. *)
	square=With[
		{pts=r*N@Tan@Subdivide[-Pi/4,Pi/4,n]},
		StructuredMesh[Outer[Reverse@*List,pts,pts],{n,n}]
	];
	
	coordinates=Transpose[Transpose[rescale/@square["Coordinates"]]+{x,y}];
	
	ToElementMesh[
		"Coordinates" ->coordinates,
		"MeshElements" -> square["MeshElements"]
	]
];


diskMeshBlock[{x_,y_},r_,n_Integer/;(n>=2)]:=Module[
	{square,sideMesh,d,raster,rotations},
	
	(* Size of internal square. Optimial minimal element quality is obtained at 0.2.
	Value of 0.3 creates nicer element size distribution. *)
	d=0.3*r;
	square=RectangleMesh[{x-d,y-d},{x+d,y+d},{n,n}];

	raster={
		Thread[{x+Subdivide[-d,d,90],y+d}],
		N@Table[{x+r*Cos[fi],y+r*Sin[fi]},{fi,3*Pi/4,Pi/4,-Pi/180}]
	};
	sideMesh=StructuredMesh[raster,{n,n}];
	rotations=RotationTransform[#,{x,y}]&/@(Range[4]*Pi/2);
	
	MergeMesh@Join[{square},TransformMesh[sideMesh,#]&/@rotations]	
];


DiskMesh::usage="DiskMesh[{x, y}, r, n] creates structured mesh with n elements on Disk of radius r centered at {x,y}.";
DiskMesh::method="Method \"`1`\" is not supported.";
DiskMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 2.";

DiskMesh//Options={Method->Automatic};

DiskMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

DiskMesh[n_Integer,opts:OptionsPattern[]]:=DiskMesh[{0,0},1,n,opts];

DiskMesh[{x_,y_},r_,n_Integer,opts:OptionsPattern[]]:=Module[
	{method},
	
	If[
		Not@TrueQ[n>=2&&IntegerQ[n]],
		Message[DiskMesh::noelems,n];Return[$Failed]
	];
	
	method=OptionValue[Method]/.Automatic->"Block";
		
	Switch[method,
		"Block",diskMeshBlock[{x,y},r,n],
		"Projection",diskMeshProjection[{x,y},r,n],
		_,Message[DiskMesh::method,method];Return[$Failed]
	]
];


(* ::Subsubsection::Closed:: *)
(*AnnulusMesh*)


AnnulusMesh::usage=
	"AnnulusMesh[{x, y}, {rIn, rOut}, {n\[Phi], nr}] creates mesh on Annulus with n\[Phi] elements in circumferential 
and nr elements in radial direction."<>"\n"<>
	"AnnulusMesh[{x, y}, {rIn, rOut}, {\[Phi]1, \[Phi]2}, {n\[Phi], nr}] creates mesh on Annulus from angle \[Phi]1 to \[Phi]2.";

AnnulusMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,_.}};

AnnulusMesh[{nPhi_Integer,nr_Integer}]:=AnnulusMesh[{0,0},{1./2,1},{0,2*Pi},{nPhi,nr}];

AnnulusMesh[{x_,y_},{rIn_,rOut_},{nPhi_Integer,nr_Integer}]:=AnnulusMesh[{x,y},{rIn,rOut},{0,2*Pi},{nPhi,nr}];

AnnulusMesh[{x_,y_},{rIn_,rOut_},{phi1_,phi2_},{nPhi_Integer,nr_Integer}]:=Module[
	{raster},
	
	raster=N@{
		Table[rOut*{Cos[fi],Sin[fi]}+{x,y},{fi,phi1,phi2,(phi2-phi1)/nPhi}],
		Table[rIn*{Cos[fi],Sin[fi]}+{x,y},{fi,phi1,phi2,(phi2-phi1)/nPhi}]
	};
	StructuredMesh[raster,{nPhi,nr}]
];


(* ::Subsubsection::Closed:: *)
(*CircularVoidMesh*)


CircularVoidMesh::usage="CircularVoidMesh[{x,y}, r, s, n] creates a square mesh of size s with circular void of radius r centered at {x,y}.";
CircularVoidMesh::ratio="Radius `1` should be smaller than the half of square domain size `2`.";

CircularVoidMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,_}};

CircularVoidMesh[{cx_,cy_},radius_,size_,n_Integer?Positive]:=Module[
	{raster,quarter},
	(* This should also make sure that numerical quantities are compared. *)
	If[
		Not@TrueQ[radius<(size/2)],
		Message[CircularVoidMesh::ratio,radius,size];Return[$Failed]
	];
		
	raster=N@{
		Table[{size/2,y}+{cx,cy},{y,-size/2,size/2,size/n}],
		Table[radius*{Cos[fi],Sin[fi]}+{cx,cy},{fi,-Pi/4,Pi/4,(Pi/2)/n}]
	};
	quarter=StructuredMesh[raster,{n,Ceiling[(size/radius/8)*n]}];
	
	MergeMesh[{
		quarter,
		TransformMesh[quarter,RotationTransform[Pi/2,{cx,cy}]],
		TransformMesh[quarter,RotationTransform[Pi,{cx,cy}]],
		TransformMesh[quarter,RotationTransform[3*Pi/2,{cx,cy}]]
	}]
];


(* ::Subsection::Closed:: *)
(*Named meshes 3D*)


(* ::Subsubsection::Closed:: *)
(*CuboidMesh*)


CuboidMesh::usage="CuboidMesh[{x1, y1, z1}, {x2, y2, z2}, {nx, ny, nz}] creates structured mesh of hexahedra on Cuboid.";

CuboidMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_}};

CuboidMesh[n_Integer]:=CuboidMesh[{0,0,0},{1,1,1},{n,n,n}];

CuboidMesh[{x1_,y1_,z1_},{x2_,y2_,z2_},{nx_,ny_,nz_}]:=StructuredMesh[{
	{{{x1,y1,z1},{x2,y1,z1}},{{x1,y2,z1},{x2,y2,z1}}},
	{{{x1,y1,z2},{x2,y1,z2}},{{x1,y2,z2},{x2,y2,z2}}}
	},
	{nx,ny,nz}
];


(* ::Subsubsection::Closed:: *)
(*HexahedronMesh*)


HexahedronMesh::usage="HexahedronMesh[{p1, p2, ..., p8}, {nx, ny, nz}] creates structured mesh on Hexahedron.";
HexahedronMesh::ordering="Warning! Ordering of corners may be incorrect.";

HexahedronMesh//SyntaxInformation={"ArgumentsPattern"->{_,_.}};

HexahedronMesh[{nx_Integer,ny_Integer,nz_Integer}]:=HexahedronMesh[
	{{0,0,0},{1,0,0},{1,1,0},{0,1,0},{0,0,1},{1,0,1},{1,1,1},{0,1,1}},
	{nx,ny,nz}
];

HexahedronMesh[{p1_,p2_,p3_,p4_,p5_,p6_,p7_,p8_},{nx_Integer,ny_Integer,nz_Integer}]:=Module[
	{mesh},
	Check[
		mesh=StructuredMesh[
			{{{p1,p2},{p4,p3}},{{p5,p6},{p8,p7}}},
			{nx,ny,nz}
		],
		Message[HexahedronMesh::ordering];mesh
	]
];


(* ::Subsubsection::Closed:: *)
(*CylinderMesh*)


CylinderMesh::usage="CylinderMesh[{{x1, y1, z1}, {x2, y2, z2}}, r, {nr,nz}] creates structured mesh on Cylinder.";
CylinderMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 2.";

CylinderMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

CylinderMesh[{nr_Integer,nz_Integer},opts:OptionsPattern[]]:=CylinderMesh[{{0,0,-1},{0,0,1}},1,{nr,nz},opts];

CylinderMesh[{{x1_,y1_,z1_},{x2_,y2_,z2_}},r_,{nr_Integer,nz_Integer},opts:OptionsPattern[]]:=Module[
	{diskMesh,length,alignedCylinder,tf},
	If[
		TrueQ[nr<2]||Not@IntegerQ[nr],
		Message[CylinderMesh::noelems,nr];Return[$Failed]
	];

	length=Norm[{x2,y2,z2}-{x1,y1,z1}];
	diskMesh=DiskMesh[{0,0},r,nr,FilterRules[{opts},Options@DiskMesh]];
	alignedCylinder=ExtrudeMesh[diskMesh,length,nz];
	tf=Last@FindGeometricTransform[{{x1,y1,z1},{x2,y2,z2}},{{0,0,0},{0,0,length}}];
	
	ToElementMesh[
		"Coordinates" -> (tf@alignedCylinder["Coordinates"]),
		"MeshElements" -> alignedCylinder["MeshElements"]
	]
];


(* ::Subsubsection::Closed:: *)
(*SphereMesh*)


(* 
Some key ideas for this code come from the answer by "Michael E2" on topic: 
https://mathematica.stackexchange.com/questions/85592/how-to-create-an-elementmesh-of-a-sphere
*)
SphereMesh::usage="SphereMesh[{x, y, z}, r, n] creates structured mesh with n elements on Sphere of radius r centered at {x,y,z}.";
SphereMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 2.";

SphereMesh//Options={"MeshOrder"->Automatic};

SphereMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

SphereMesh[n_Integer,opts:OptionsPattern[]]:=SphereMesh[{0,0,0},1,n,opts];

SphereMesh[{x_,y_,z_},r_,n_Integer,opts:OptionsPattern[]]:=Module[
	{order,rescale,cuboid,cuboidShell,coordinates},
	If[TrueQ[n<2]||Not@IntegerQ[n],Message[SphereMesh::noelems,n];Return[$Failed]];
	order=OptionValue["MeshOrder"]/.Automatic->1;
	If[Not@MatchQ[order,1|2],Message[ToElementMesh::femmonv,order,1];Return[$Failed]];
	
	rescale=(Max[Abs@#]*Normalize[#])&;
	(* This special raster makes all element edges on sphere edge of the same length. *)
	cuboid=With[
		{pts=r*N@Tan@Subdivide[-Pi/4,Pi/4,n]},
		StructuredMesh[Outer[Reverse@*List,pts,pts,pts],{n,n,n}]
	];
	(* If we do order alteration (more than 1st order) before projection, then geometry is
	more accurate and elements have curved edges. *)
	cuboidShell=MeshOrderAlteration[
		ToBoundaryMesh[cuboid],
		order
	];
	
	coordinates=Transpose[Transpose[rescale/@cuboidShell["Coordinates"]]+{x,y,z}];
	
	ToBoundaryMesh[
		"Coordinates" -> coordinates,
		"BoundaryElements" -> cuboidShell["BoundaryElements"]
	]
];


(* ::Subsubsection::Closed:: *)
(*SphericalShellMesh*)


SphericalShellMesh::usage="SphericalShellMesh[{x, y, z}, {rIn, rOut}, {n\[Phi], nr}] creates structured mesh on SphericalShell, 
with nPhi elements in circumferential and nr elements in radial direction.";

SphericalShellMesh//Options={"MeshOrder"->Automatic};

SphericalShellMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

SphericalShellMesh[{nPhi_Integer,nr_Integer},opts:OptionsPattern[]]:=SphericalShellMesh[{0,0,0},{1./2,1},{nPhi,nr},opts];

SphericalShellMesh[{x_,y_,z_},{rIn_,rOut_},{nPhi_Integer,nr_Integer},opts:OptionsPattern[]]:=Module[
	{order,rescale,innerRaster,outerRaster,rotations,flatMesh,curvedMesh},
	
	order=OptionValue["MeshOrder"]/.Automatic->1;
	If[
		Not@MatchQ[order,1|2],
		Message[ToElementMesh::femmonv,order,1];Return[$Failed]
	];
	
	rescale=(Max[Abs@#]*Normalize[#])&;
	
	(* This special raster makes all element edges on disk edge of the same length. *)
	innerRaster=With[
		{pts=rIn*N@Tan@Subdivide[-Pi/4,Pi/4,nPhi]},
		Map[Append[rIn], Outer[Reverse@*List,pts,pts], {2}]
	];
	outerRaster=With[
		{pts=rOut*N@Tan@Subdivide[-Pi/4,Pi/4,nPhi]},
		Map[Append[rOut], Outer[Reverse@*List,pts,pts], {2}]
	];
	
	flatMesh=MeshOrderAlteration[
		StructuredMesh[{innerRaster,outerRaster},{nPhi,nPhi,nr}],
		order
	];
	curvedMesh=ToElementMesh[
		"Coordinates" ->Transpose[Transpose[rescale/@flatMesh["Coordinates"]]+{x,y,z}],
		"MeshElements" -> flatMesh["MeshElements"]
	];
	
	rotations=Join[
		RotationTransform[#,{1,0,0},{x,y,z}]&/@(Range[4]*Pi/2),
		RotationTransform[#,{0,1,0},{x,y,z}]&/@{Pi/2,3*Pi/2}
	];
		
	MergeMesh@(TransformMesh[curvedMesh,#]&/@rotations)
];


(* ::Subsubsection::Closed:: *)
(*BallMesh*)


ballMeshBlock[{x_,y_,z_},r_,n_Integer/;(n>=1)]:=Module[
	{rescale,bottomRaster,topRaster,cubeMesh,sideMesh,d,rotations,unitBall},
	
	(* Size of internal square. Optimial minimal element quality is obtained at 0.2.
	Value of 0.3 creates nicer element size distribution. *)
	d=0.3;
	rescale=(Max[Abs@#]*Normalize[#])&;
	
	bottomRaster=With[
		{pts=d*N@Subdivide[-1,1,n]},
		Map[Append[d], Outer[Reverse@*List,pts,pts], {2}]
	];
	
	(* This special raster makes all element edges on disk edge of the same length. *)
	topRaster=With[
		{pts=N@Tan@Subdivide[-Pi/4,Pi/4,n]},
		Map[rescale@*Append[1.], Outer[Reverse@*List,pts,pts], {2}]
	];
	
	cubeMesh=CuboidMesh[-d*{1,1,1},d*{1,1,1},{n,n,n}];
	(* TODO: Figure out how many elements should be in radial direction. *)
	sideMesh=StructuredMesh[{bottomRaster,topRaster},{n,n,n}];
	
	rotations=Join[
		RotationTransform[#,{0,1,0}]&/@({0,1,2,3}*Pi/2),
		RotationTransform[#,{1,0,0}]&/@({1,3}*Pi/2)
	];
	
	unitBall=MergeMesh@Join[{cubeMesh},TransformMesh[sideMesh,#]&/@rotations];
	
	ToElementMesh[
		"Coordinates" ->Transpose[Transpose[r*unitBall["Coordinates"]]+{x,y,z}],
		"MeshElements" -> unitBall["MeshElements"]
	]
];


(* Some key ideas come from: https://mathematica.stackexchange.com/a/103468 *)

ballMeshProjection[{x_,y_,z_},r_,n_Integer/;(n>=1)]:=Module[
	{rescale,cuboidMesh,coordinates},
	
	rescale=(Max[Abs@#]*Normalize[#])&;
	(* This special raster makes all element edges on sphere edge of the same length. *)
	cuboidMesh=With[
		{pts=N@Tan@Subdivide[-Pi/4,Pi/4,n]},
		StructuredMesh[Outer[Reverse@*List,pts,pts,pts],{n,n,n}]
	];
	(* If we do order alteration (more than 1st order) before projection, then geometry is
	more accurate and elements have curved edges. *)
	(*cuboidMesh=MeshOrderAlteration[cuboidMesh,order];*)
	
	coordinates=Transpose[Transpose[r*(rescale/@cuboidMesh["Coordinates"])]+{x,y,z}];
	
	ToElementMesh[
		"Coordinates" -> coordinates,
		"MeshElements" -> cuboidMesh["MeshElements"]
	]
];


BallMesh::usage="BallMesh[{x, y, z}, r, n] creates structured mesh with n elements on Ball of radius r centered at {x,y,z}.";
BallMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 1.";
BallMesh::method="Method \"`1`\" is not supported.";

BallMesh//Options={Method->Automatic};

BallMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

BallMesh[n_Integer,opts:OptionsPattern[]]:=BallMesh[{0,0,0},1,n,opts];

BallMesh[{x_,y_,z_},r_,n_Integer,opts:OptionsPattern[]]:=Module[
	{method},
	
	If[
		Not@(TrueQ[n>=1]&&IntegerQ[n]),
		Message[BallMesh::noelems,n];Return[$Failed]
	];
	
	method=OptionValue[Method]/.Automatic->"Block";
	
	Switch[method,
		"Block",ballMeshBlock[{x,y,z},r,n],
		"Projection",ballMeshProjection[{x,y,z},r,n],
		_,Message[BallMesh::method,method];$Failed
	]
];


(* ::Subsubsection::Closed:: *)
(*SphericalVoidMesh*)


SphericalVoidMesh::usage="SphericalVoidMesh[voidRadius, cuboidSize, noElements] creates a mesh with spherical void in cuboid domain.";

SphericalVoidMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

SphericalVoidMesh[voidRadius_,cuboidSize_,noElements_Integer,opts:OptionsPattern[]]:=Module[
	{r,s,n,rescale,outerRaster,innerRaster,basicMesh,rt},
	
	rescale=(Max[Abs@#]*Normalize[#])&;
	
	s=cuboidSize;
	r=Clip[voidRadius,{0.01,Infinity}];
	n=noElements;
	
	outerRaster=With[
		{pts=s*N@Subdivide[0,1,n]},
		Map[Append[s], Outer[Reverse@*List,pts,pts], {2}]
	];
	(* This special raster makes all element edges on sphere edge of the same length. *)
	innerRaster=With[
		{pts=r*N@Tan@Subdivide[0,Pi/4,n]},
		Map[rescale@*Append[r], Outer[Reverse@*List,pts,pts], {2}]
	];
	(* In "thickness" direction the number of elements is estimated so that their
	edges have similar length. *)
	basicMesh=StructuredMesh[{innerRaster,outerRaster},{n,n,Ceiling[n*(s/r/2)]}];

	rt=RotationTransform[#*(2*Pi/3),{1,1,1},{0,0,0}]&/@{0,1,2};
	MergeMesh[TransformMesh[basicMesh,#]&/@rt]
];


(* ::Subsubsection::Closed:: *)
(*EllipsoidVoidMesh*)


(* ::Text:: *)
(*This is a quick prototype, made for analysis of voids inside material (forging).*)


(* Projection (with RegionNearest) of points from one side of the cube to a sphere. 
Argument f should be a Function to specify on which side of cube points are created (e.g. {1,#1,#2}& ). 
It is assumed size of domain is 1. *)

voidRaster[f_Function,semiAxis:{_,_,_}]:=Module[
	{n=30,pts,sidePts,innerPts,middlePts},
	pts=N@Subdivide[0,1,n-1];
	sidePts=Flatten[Outer[f,pts,pts],1];
	innerPts=RegionNearest[Ellipsoid[{0,0,0},semiAxis],sidePts];
	middlePts=RegionNearest[Ellipsoid[{0,0,0},semiAxis*2],sidePts];
	{Partition[sidePts,n],Partition[middlePts,n],Partition[innerPts,n]}
];


EllipsoidVoidMesh::usage=
	"EllipsoidVoidMesh[radius, noElements] creates a mesh with spherical void."<>"\n"<>
	"EllipsoidVoidMesh[{r1, r2, r3}, noElements] creates a mesh with ellipsoid void with semi-axis radii r1, r2 and r3.";

EllipsoidVoidMesh//SyntaxInformation={"ArgumentsPattern"->{_,_}};

EllipsoidVoidMesh[{r1_,r2_,r3_},noElements_Integer]:=With[{
	dim=Clip[#,{0.01,0.8}]&/@{r1,r2,r3},
	n=Round@Clip[noElements,{1,100}]
	},
	Fold[
		MergeMesh[#1,#2]&,
		{
		StructuredMesh[voidRaster[{1,#1,#2}&,dim],n{1,1,2}],
		StructuredMesh[voidRaster[{#2,1,#1}&,dim],n{1,1,2}],
		StructuredMesh[voidRaster[{#1,#2,1}&,dim],n{1,1,2}]
		}
	]
];

(* Implementation for spherical void is faster, because (costly) StructuredMesh is generated
only once and then rotated. *)
EllipsoidVoidMesh[voidRadius_,noElements_Integer]:=Module[{
	dim=Clip[voidRadius,{0.01,0.8}]*{1,1,1},
	n=Round@Clip[noElements,{1,100}],
	rotations=RotationTransform[#,{1,1,1},{0,0,0}]&/@{0,2*Pi/3,4*Pi/3},
	basicMesh
	},
	basicMesh=StructuredMesh[voidRaster[{1,#1,#2}&,dim],n{1,1,2}];
	Fold[
		MergeMesh[#1,#2]&,
		TransformMesh[basicMesh,#]&/@rotations
	]
];


(* ::Subsubsection::Closed:: *)
(*TetrahedronMesh*)


unitTetrahedronToHexahedra[n_Integer]:=Module[
	{n1,n2,n3,n4,x,connectivity},

	{n1,n2,n3,n4}={{0,0,0},{1,0,0},{0,1,0},{0,0,1}};
	
	x=Join[{n1,n2,n3,n4},Mean/@{
		{n1,n2},{n2,n3},{n3,n1},{n2,n4},{n3,n4},{n1,n4},
		{n4,n3,n2},{n4,n1,n3},{n4,n2,n1},{n1,n2,n3},
		{n1,n2,n3,n4}
		}
	];
	
	connectivity={
		{
			{{x[[1]],x[[5]]},{x[[7]],x[[14]]}},
			{{x[[10]],x[[13]]},{x[[12]],x[[15]]}}
		},
		{
			{{x[[5]],x[[2]]},{x[[14]],x[[6]]}},
			{{x[[13]],x[[8]]},{x[[15]],x[[11]]}}
		},
		{
			{{x[[7]],x[[14]]},{x[[3]],x[[6]]}},
			{{x[[12]],x[[15]]},{x[[9]],x[[11]]}}
		},
		{
			{{x[[10]],x[[13]]},{x[[12]],x[[15]]}},
			{{x[[4]],x[[8]]},{x[[9]],x[[11]]}}
		}
	};
	MergeMesh@Map[
		StructuredMesh[#,{n,n,n}]&,
		connectivity
	]
];


unitTetrahedronToTetrahedra[n_Integer]:=Module[
	{},

	SelectElements[
		HexToTetrahedronMesh[
			CuboidMesh[{0,0,0},{1,1,1},{n,n,n}]
		],
		#1+#2+#3<=1&
	]
];


TetrahedronMesh::usage="TetrahedronMesh[{p1,p2,p3,p4}, n] creates tetrahedral mesh on Tetrahedron with corners p1, p2, p3 and p4.";
TetrahedronMesh::hexelms="Only even number of elements per edge is allowed for hexahedral mesh.";
TetrahedronMesh::badtype="Unknown value `1` for option \"MeshElementType\".";

TetrahedronMesh//Options={"MeshElementType"->HexahedronElement};

TetrahedronMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,OptionsPattern[]}};

TetrahedronMesh[n_Integer?Positive,opts:OptionsPattern[]]:=TetrahedronMesh[{{0,0,0},{1,0,0},{0,1,0},{0,0,1}},n,opts];

TetrahedronMesh[{p1_,p2_,p3_,p4_},n_Integer?Positive,opts:OptionsPattern[]]:=Module[
	{type,unitMesh,tf},
	
	type=OptionValue["MeshElementType"];
	
	If[
		(type===HexahedronElement)&&OddQ[n],
		Message[TetrahedronMesh::hexelms];Return[$Failed,Module]
	];
	
	unitMesh=Switch[type,
		TetrahedronElement,unitTetrahedronToTetrahedra[n],
		HexahedronElement,unitTetrahedronToHexahedra[n/2],
		_,Message[TetrahedronMesh::badtype,type];Return[$Failed,Module]
	];
	
	tf=Threshold/@Last@FindGeometricTransform[
		{p1,p2,p3,p4},
		{{0,0,0},{1,0,0},{0,1,0},{0,0,1}},
		Method->"Linear"
	];
	
	TransformMesh[unitMesh,tf]
];


(* ::Subsubsection::Closed:: *)
(*PrismMesh*)


(* Helper function to determine if points given as Prism corners need reordering. *)
reorientPrismQ[pts_]:=With[{
	a=pts[[2]]-pts[[1]],
	b=pts[[3]]-pts[[1]],
	c=pts[[4]]-pts[[1]]
	},
	Negative[Cross[a,b].c]
];


PrismMesh::usage="PrismMesh[{p1, ..., p6},{n1, n2}] creates structured mesh on Prism, with n1 and n2 elements per edge.";
PrismMesh::noelems="Specificaton of elements `1` must be even integer equal or larger than 2.";
PrismMesh::alignerr="Warning! Corner alignment error `1` is larger than tolerance.";

PrismMesh//SyntaxInformation={"ArgumentsPattern"->{_,_}};

PrismMesh[{n1_Integer,n2_Integer}]:=PrismMesh[{{0,0,0},{1,0,0},{0,1,0},{0,0,1},{1,0,1},{0,1,1}},{n1,n2}];

PrismMesh[corners_List,{n1_Integer,n2_Integer}]:=Module[
	{pts,error,triangleMesh,standardPrism,tf},
	If[
		Not@(TrueQ[n1>=2]&&EvenQ[n1]),
		Message[PrismMesh::noelems,n1];Return[$Failed]
	];
	
	pts=If[
		reorientPrismQ[corners],
		Join@@Reverse@TakeDrop[corners,3],
		corners
	];
	(* Smoothing TriangleMesh before extrusion seems to even worsen the quality of hex mesh. *)
	triangleMesh=TriangleMesh[{{0,0},{1,0},{0,1}},n1,"MeshElementType"->QuadElement];
	standardPrism=ExtrudeMesh[triangleMesh,1,n2];
	(* Find TransformationFunction between standard and arbitrary prism. "Linear" method
	is much faster than others. *)
	{error,tf}=FindGeometricTransform[
		pts,
		{{0,0,0},{1,0,0},{0,1,0},{0,0,1},{1,0,1},{0,1,1}},
		Method->"Linear"
	];
	(* Alignment error is non-zero in case of non-homogenous transformation. 
	Currently the tolerance is chosen arbitrarily. *)
	If[error>10^-4,Message[PrismMesh::alignerr,ScientificForm[error,4]]];
		
	ToElementMesh[
		"Coordinates" ->(tf@standardPrism["Coordinates"]),
		"MeshElements" -> standardPrism["MeshElements"]
	]
];


(* ::Section::Closed:: *)
(*End package*)


End[]; (* "`Private`" *)


EndPackage[];
