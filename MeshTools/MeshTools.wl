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
RevolveMesh;

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
(*RevolveMesh*)


RevolveMesh::usage=
	"RevolveMesh[mesh, {fi1, fi2}, layers] revolves 2D quadrilateral mesh to 3D hexahedron mesh around Y axis.";
RevolveMesh::order="Only first order mesh is supported.";
RevolveMesh::eltype="Only mesh with QuadElement(s) is supported.";
RevolveMesh::angle="Angle limits for body of revolution must be distinct.";
RevolveMesh::division="There should be more than one element for each Pi/2 sector.";
RevolveMesh::axis="Non-positive first coordinate (X) of nodes causes self-intersecting elements.";

RevolveMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_}};

RevolveMesh[mesh_ElementMesh,{fi1_,fi2_},layers_Integer?Positive]:=Module[
	{min,max,n,nodes2D,nodes3DInitial,nodes3D,elements2D,elements3D,markers2D,markers3D},

	If[
		mesh["MeshOrder"]=!=1,
		Message[RevolveMesh::order];Return[$Failed,Module]
	];
	(* Mesh "EmbeddingDimension" is already checked with correct "MeshElements" type. *)
	If[
		(Head/@mesh["MeshElements"])=!={QuadElement},
		Message[RevolveMesh::eltype];Return[$Failed,Module]
	];
	(* Sorting angle by size solves potential problems with inverted elements. *)
	{min,max}=MinMax[{fi1,fi2}];
	If[
		N[min]==N[max],
		Message[RevolveMesh::angle];Return[$Failed,Module]
	];
	max=Clip[max,{min,min+2*Pi}];
	(* It makes no sense if element division in angular direction is too small. *)
	If[
		(max-min)/layers>Pi/2,
		Message[RevolveMesh::division];Return[$Failed,Module]
	];
	(* If some nodes lie in non-positive half plane elements get self-intersected. *)
	nodes2D=mesh["Coordinates"];
	If[
		Min[nodes2D[[All,1]]]<=0.,
		Message[RevolveMesh::axis];Return[$Failed,Module]
	];

	elements2D=First@ElementIncidents[mesh["MeshElements"]];
	markers2D=First@ElementMarkers[mesh["MeshElements"]];
	n=Length[nodes2D];

	nodes3DInitial=Transpose@Join[Transpose[nodes2D],{ConstantArray[0.,n]}];
	nodes3D=Flatten[
		Map[
			(RotationTransform[#,{0,1,0}]@nodes3DInitial)&,
			Reverse@Subdivide[min,max,layers]
		],
		1
	];

	elements3D=Flatten[
		Table[
			Map[Join[n*(i-1)+#,n*i+#]&,elements2D],
			{i,layers}
		],
		1
	];

	markers3D=Flatten@ConstantArray[markers2D,layers];

	ToElementMesh[
		"Coordinates"->nodes3D,
		"MeshElements"->{HexahedronElement[elements3D,markers3D]},
		"CheckIntersections"->False
	]
];


(* ::Subsubsection::Closed:: *)
(*Mesh smoothing*)


mkFindConnectedNodes[mesh_ElementMesh] := Block[
	{vec, vecTvec, nzv, nzp, neigbours, temp2, sa}, 

	vec = mesh["VertexElementConnectivity"];
	vecTvec = vec.Transpose[vec];
	nzv = vecTvec["NonzeroValues"];
	nzp = vecTvec["NonzeroPositions"];
	neigbours = Join @@ Position[nzv, 2];
	temp2 = Transpose[nzp[[neigbours]]];
	sa = SparseArray[
			Transpose[
				Developer`ToPackedArray[{temp2[[1]],Range[Length[temp2[[1]]]]}]
			] -> 1,
			{Length[mesh["Coordinates"]], Length[neigbours]}
	];

	With[
		{lookup = sa, ids = temp2[[2]]},
		Function[theseNodes, 
			ids[[Flatten[lookup[[#]]["NonzeroPositions"]]]] & /@ theseNodes
		]
	]
];


findInteriorNodes[mesh_ElementMesh] := With[{
	allNodes = Range[Length[mesh["Coordinates"]]],
	boundaryNodes = Join @@ (Flatten /@ ElementIncidents[mesh["BoundaryElements"]])
	},
	Complement[allNodes, boundaryNodes]
];


$defaultMaxIter = 100;

iterativeMeshSmoothing//Options = {MaxIterations -> $defaultMaxIter};

(* Source of this fuction is https://mathematica.stackexchange.com/a/156669
and https://github.com/WolframResearch/FEMAddOns *)
iterativeMeshSmoothing[mesh_ElementMesh,opts:OptionsPattern[iterativeMeshSmoothing]]:=Block[
	{inodes, connectedNodes, temp, crds, movedCoords, iter},

	iter = OptionValue["MaxIterations"];
	If[ Not@(IntegerQ[iter] && iter > 0), iter = $defaultMaxIter];
  
	inodes = findInteriorNodes[mesh];
	(* Compute connectivity only once. *)
	connectedNodes = mkFindConnectedNodes[mesh];
	temp = connectedNodes[inodes];
	crds = mesh["Coordinates"];
	Do[
		movedCoords = Developer`ToPackedArray[Mean[crds[[#]]] & /@ temp];
		crds[[inodes]] = movedCoords,
		{iter}
	];

	ToElementMesh[
		"Coordinates" -> crds,
		"MeshElements" -> mesh["MeshElements"],
		"BoundaryElements" -> mesh["BoundaryElements"],
		"PointElements" -> mesh["PointElements"],
		"RegionHoles" -> mesh["RegionHoles"]
	]
];


(* This implements Laplacian mesh smoothing. Source: https://mathematica.stackexchange.com/a/156669 *)
directMeshSmoothing[mesh_ElementMesh,opts:OptionsPattern[]]:=Module[
	{n,vec,mat,adjacencyMatrix,mass,bndVertices,interiorVertices,stiffness,load,newCoords},
	
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
	interiorVertices = Complement[Range[1, n], bndVertices];
	
	stiffness[[bndVertices]] = IdentityMatrix[n, SparseArray][[bndVertices]];
	
	load = ConstantArray[0., {n, mesh["EmbeddingDimension"]}];
	load[[bndVertices]] = mesh["Coordinates"][[bndVertices]];
	newCoords = LinearSolve[stiffness, load];
	
	ToElementMesh[
		"Coordinates" -> newCoords,
		"MeshElements" -> mesh["MeshElements"],
		"BoundaryElements" -> mesh["BoundaryElements"],
		"PointElements" -> mesh["PointElements"],
		"CheckIntersections" -> False,
		"DeleteDuplicateCoordinates" -> False,
		"RegionHoles" -> mesh["RegionHoles"]
	]
];


SmoothenMesh::usage="SmoothenMesh[mesh] improves the quality of 2D mesh.";
SmoothenMesh::badmtd="Values for option Method should be \"Direct\", \"Iterative\" or Automatic.";
SmoothenMesh::fail="Failure in mesh smoothing. Possible solution is Method->{\"Iterative\",MaxIterations->1}.";

SmoothenMesh//Options={Method->Automatic};

SmoothenMesh//SyntaxInformation={"ArgumentsPattern"->{_,OptionsPattern[]}};

SmoothenMesh[mesh_ElementMesh,opts:OptionsPattern[]]:=Module[
	{method,fun,subOpts,result},
	
	method=Flatten[{OptionValue[Method]/.Automatic->"Direct"}];
	Switch[
		First[method],
		"Iterative",fun=iterativeMeshSmoothing,
		"Direct",fun=directMeshSmoothing,
		_,Message[SmoothenMesh::badmtd];Return[$Failed,Module]			
	 ];

	subOpts = {};
	If[ Length[method] > 1, subOpts=Rest@method ];
	Check[
		result=fun[mesh,FilterRules[subOpts,Options@fun]],
		Message[SmoothenMesh::fail];result
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


(* Based on https://mathematica.stackexchange.com/questions/176296 *)
ElementMeshCurvedWireframe::usage="ElementMeshCurvedWireframe[mesh] plots second order mesh with curved edges.";
ElementMeshCurvedWireframe::type="Only 2D ElementMesh objects with \"MeshOrder\"==2 are supported.";

ElementMeshCurvedWireframe//SyntaxInformation={"ArgumentsPattern"->{_,OptionsPattern[]}};

ElementMeshCurvedWireframe//Options=Join[{PlotStyle->Automatic},Options[Graphics]];

ElementMeshCurvedWireframe[mesh_ElementMesh,opts:OptionsPattern[]]:=Module[
	{triEdges,quadEdges,getEdges,interpolatingCurve,interpolatingCurveComplex,gr},
	
	If[
		Or[
			mesh["EmbeddingDimension"]=!=2,
			mesh["MeshOrder"]=!=2
		],
		Message[ElementMeshCurvedWireframe::type];Return[$Failed,Module]
	];

	triEdges=MeshElementBaseFaceIncidents[TriangleElement,2][[All,{1,3,2}]];
	quadEdges=MeshElementBaseFaceIncidents[QuadElement,2][[All,{1,3,2}]];
	
	getEdges[ele_TriangleElement]:=Join@@(ElementIncidents[ele][[All,#]]&/@triEdges);
	getEdges[ele_QuadElement]:=Join@@(ElementIncidents[ele][[All,#]]&/@quadEdges);
	getEdges[ele_List]:=getEdges/@ele;
	
	interpolatingCurve[pts_List]/;Length[pts]==3:=
		BezierCurve[{pts[[1]],1/2 (-pts[[1]]+4 pts[[2]]-pts[[3]]),pts[[3]]}];
	interpolatingCurve[ptslist_List]:=interpolatingCurve/@ptslist;
	
	interpolatingCurveComplex[coords_,indices_]:=interpolatingCurve[
		Map[coords[[#]]&,indices]
	];
	gr=Graphics[{
		(OptionValue[PlotStyle]/.Automatic->Black),
		Map[
			interpolatingCurveComplex[mesh["Coordinates"],#]&,
			Join@@getEdges[mesh["MeshElements"]]
		]
		},
		FilterRules[{opts},Options@Graphics]
	];
	(* Explicity clear local symbols with DownValues to avoid memory leaks. *)
	ClearAll[getEdges,interpolatingCurve,interpolatingCurveComplex];
	gr
];


(* ::Subsection::Closed:: *)
(*Structured mesh*)


(* ::Subsubsection::Closed:: *)
(*Connectivity*)


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


(* ::Subsubsection::Closed:: *)
(*Unit structured mesh*)


unitStructuredMesh[nx_,ny_]:=With[{
	(* Vectorized (the fastest) way to get coordinates of unit square. *)
	nodes=Flatten[
		Outer[List,Subdivide[0.,1.,nx],Subdivide[0.,1.,ny]],
		1
	],
	connectivity=getElementConnectivity[nx,ny]
	},
	(* We can disable all error checks for this mesh, because we know it is correct. *)
	ToElementMesh[
		"Coordinates"->nodes,
		"MeshElements"->{QuadElement[connectivity]},
		"CheckIntersections"->False,
		"CheckQuality"->False,
		"DeleteDuplicateCoordinates"->False
	]
];

unitStructuredMesh[nx_,ny_,nz_]:=With[{
	nodes=Flatten[
		Outer[List,Subdivide[0.,1.,nx],Subdivide[0.,1.,ny],Subdivide[0.,1.,nz]],
		2
	],
	connectivity=getElementConnectivity[nx,ny,nz]
	},
	ToElementMesh[
		"Coordinates"->nodes,
		"MeshElements"->{HexahedronElement[connectivity]},
		"CheckIntersections"->False,
		"CheckQuality"->False,
		"DeleteDuplicateCoordinates"->False
	]
];


(* ::Subsubsection::Closed:: *)
(*Refinement functions*)


refinementNodeMaker[{nx_,ny_}]:=Flatten[
	Drop[
		Outer[List,
			Subdivide[0.,0.5/nx,1],
			Subdivide[0.,1.,3.ny]
		],
		None,{1,-1,3}
	],
	1
];

refinementNodeMaker[{nx_,ny_,nz_}]:=Module[
	{data,i,j},
	data=Outer[List,
		Subdivide[0.,0.5/nx,1],		
		Subdivide[0.,1.,3.ny],
		Subdivide[0.,1.,3.nz]
	];
	i=Join[Range[2,Length@data[[1]],3],Range[3,Length@data[[1]],3]];
	j=Join[Range[2,Length@data[[1,1]],3],Range[3,Length@data[[1,1]],3]];
	data[[All,1;;-1;;3]]=Drop[data[[All,1;;-1;;3]],None,None,{1,-1,3}];
	data[[2,i,j,1]]*=0.5;
	Flatten[data,2]
];


refinementSingleElement[nds_,add_]:={
	{nds[[1]],add[[3]],add[[4]],add[[1]],nds[[5]],add[[15]],add[[16]],add[[13]]},
	{add[[1]],add[[4]],add[[5]],add[[2]],add[[13]],add[[16]],add[[17]],add[[14]]},
	{add[[2]],add[[5]],add[[6]],nds[[2]],add[[14]],add[[17]],add[[18]],nds[[6]]},
	{add[[3]],add[[7]],add[[8]],add[[4]],add[[15]],add[[19]],add[[20]],add[[16]]},
	{add[[4]],add[[8]],add[[9]],add[[5]],add[[16]],add[[20]],add[[21]],add[[17]]},
	{add[[5]],add[[9]],add[[10]],add[[6]],add[[17]],add[[21]],add[[22]],add[[18]]},
	{add[[7]],nds[[3]],add[[11]],add[[8]],add[[19]],nds[[7]],add[[23]],add[[20]]},
	{add[[8]],add[[11]],add[[12]],add[[9]],add[[20]],add[[23]],add[[24]],add[[21]]},
	{add[[9]],add[[12]],nds[[4]],add[[10]],add[[21]],add[[24]],nds[[8]],add[[22]]},
	{add[[13]],add[[16]],add[[17]],add[[14]],nds[[5]],add[[15]],add[[18]],nds[[6]]},
	{add[[16]],add[[20]],add[[21]],add[[17]],add[[15]],add[[19]],add[[22]],add[[18]]},
	{add[[20]],add[[23]],add[[24]],add[[21]],add[[19]],nds[[7]],nds[[8]],add[[22]]},
	{add[[15]],add[[19]],add[[22]],add[[18]],nds[[5]],nds[[7]],nds[[8]],nds[[6]]}
};


refinementElementMaker[{nx_,ny_}]:=Module[
	{n},
	n=(nx+1)*(ny+1);
	Flatten[
		Table[
			{
				{j,ny+1+j,n+2ny+2j-1,n+2j-1},
				{ny+1+j,ny+1+j+1,n+2ny+2j,n+2ny+2j-1},
				{ny+1+j+1,j+1,n+2j,n+2ny+2j},
				n+{2j-1,2ny+2j-1,2ny+2j,2j}
			},
		{j,ny}
		],
		1
	]
];

refinementElementMaker[{nx_,ny_,nz_}]:=Module[
	{nodes,addedNodes,dummy,arr1,arr2,arr3,arr4},
	nodes=getElementConnectivity[1,ny,nz];
	dummy=ArrayReshape[Range[3*nz-1],{nz,2}];
	arr1=Flatten[Table[dummy+(i-1)*2*(4*nz+1),{i,ny}],1];
	dummy=2*nz+Table[{3*i-2,3*i-1,3*i,3*i+1},{i,nz}];
	arr2=Flatten[Table[dummy+(i-1)*2*(4*nz+1),{i,ny}],1];
	arr3=arr2+3*nz+1;
	arr4=arr1+2*nz+2*(3*nz+1);
	addedNodes=Join[arr1,arr2,arr3,arr4,2];
	addedNodes=Join[addedNodes,Max[addedNodes]+addedNodes,2];
	addedNodes=(nx+1)*(ny+1)*(nz+1)+addedNodes;
	Flatten[
		MapThread[refinementSingleElement[Sort[#1],#2]&,{nodes,addedNodes}],
		1
	]
];


refinedUnitStructuredMesh[{nx_,ny_},refinement_]:=Module[
	{mesh,nodes,elements,n,rfNodes,rfElements,phi},
	
	(* Create unit mesh *)
	If[refinement===None,Return@unitStructuredMesh[nx,ny]];
	n=If[MemberQ[{Left,Right},refinement],
		{nx,ny},
		{ny,nx}
	];
	mesh=unitStructuredMesh@@n;
	
	(* Store mesh data *)
	nodes=mesh["Coordinates"];
	elements=First@ElementIncidents@mesh["MeshElements"];
	
	(* New nodes and elements on left edge *)
	rfNodes=refinementNodeMaker@n;
	rfElements=refinementElementMaker@n;
	
	(* Delete elements on left edge *)
	elements=Drop[elements,n[[2]]];
	
	(* Create refined mesh data *)
	nodes=Join[nodes,rfNodes];
	elements=Join[elements,rfElements];
	
	(* Rotate coordinates *)
	phi=Switch[refinement,
		Left,0,
		Bottom,0.5*Pi,
		Right,Pi,
		Top,1.5*Pi
	];
	nodes=RotationTransform[phi,{0.5,0.5}]/@nodes;
	
	ToElementMesh[
		"Coordinates"->nodes,
		"MeshElements"->{QuadElement[elements]},
		"CheckIntersections"->False,
		"CheckQuality"->False,
		"DeleteDuplicateCoordinates"->False
	]
];

refinedUnitStructuredMesh[{nx_,ny_,nz_},refinement_]:=Module[
	{mesh,nodes,elements,n,rfNodes,rfElements,phi,theta},
	
	(* Create unit mesh *)
	If[refinement===None,Return@unitStructuredMesh[nx,ny,nz]];
	n=Switch[refinement,
		Left,{nx,ny,nz},
		Right,{nx,ny,nz},
		Bottom,{nz,ny,nx},
		Top,{nz,ny,nx},
		Front,{ny,nx,nz},
		Back,{ny,nx,nz}
	];
	mesh=unitStructuredMesh@@n;
	
	(* Store mesh data *)
	nodes=mesh["Coordinates"];
	elements=First@ElementIncidents@mesh["MeshElements"];
	
	(* New nodes and elements on left edge *)
	rfNodes=refinementNodeMaker@n;
	rfElements=refinementElementMaker@n;
	
	(* Delete elements on left edge *)
	elements=Drop[elements,n[[2]]*n[[3]]];
	
	(* Create refined mesh data *)
	nodes=Join[nodes,rfNodes];
	elements=Join[elements,rfElements];
	
	(* Rotate coordinates *)
	{phi,theta}=Switch[refinement,
		Left,{0,0},
		Front,{0.5*Pi,0},
		Bottom,{0,1.5*Pi},
		Right,{Pi,0},
		Back,{1.5*Pi,0},
		Top,{0,0.5*Pi}	
	];
	nodes=RotationTransform[phi,{0,0,1},{0.5,0.5,0.5}]/@nodes;
	nodes=RotationTransform[theta,{0,1,0},{0.5,0.5,0.5}]/@nodes;
		
	ToElementMesh[
		"Coordinates"->nodes,
		"MeshElements"->{HexahedronElement[elements]},
		"CheckIntersections"->False,
		"CheckQuality"->False,
		"DeleteDuplicateCoordinates"->False
	]
];


(* ::Subsubsection::Closed:: *)
(*The main function*)


(* We create structured mesh on unit square (or cube) and then reinterpolate nodal 
coordinates over given raster points. Underlying unit mesh can also have refinement on 
some edges and/or have "MeshOrder"\[Rule]2. *)

StructuredMesh::usage=(
	"StructuredMesh[raster,{nx,ny}] creates structured mesh of quadrilaterals.\n"<>
	"StructuredMesh[raster,{nx,ny,nz}] creates structured mesh of hexahedra."
);
StructuredMesh::array="Raster of input points must be a full (non-ragged) array of numbers with depth of `1`.";
StructuredMesh::refinement="Value of \"Refinement\" option should be one of `1`.";

StructuredMesh//Options={InterpolationOrder->1,"MeshOrder"->1,"Refinement"->None};

StructuredMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,OptionsPattern[]}};

StructuredMesh[raster_,{nx_Integer?Positive,ny_Integer?Positive},opts:OptionsPattern[]]:=Module[
	{refinement,order,dim,restructured,xInt,yInt,zInt,unitMesh,unitCrds,crds},
	
	If[
		Not@ArrayQ[raster,3,NumericQ],
		Message[StructuredMesh::array,3+1];Return[$Failed,Module]
	];
	
	refinement=OptionValue["Refinement"];
	If[
		Not@MemberQ[{Left,Right,Top,Bottom,None},refinement],
		Message[StructuredMesh::refinement,{Left,Right,Top,Bottom,None}];refinement=None
	];
	
	order=OptionValue[InterpolationOrder]/.Automatic->1;
	dim=Last@Dimensions[raster];	
	(* We make sure to work with machine precison numbers. *)
	restructured=Transpose[N[raster],{3,2,1}];
	xInt=ListInterpolation[restructured[[1]],{{0.,1.},{0.,1.}},InterpolationOrder->order];
	yInt=ListInterpolation[restructured[[2]],{{0.,1.},{0.,1.}},InterpolationOrder->order];
	
	unitMesh=MeshOrderAlteration[
		refinedUnitStructuredMesh[{nx,ny},refinement],
		OptionValue["MeshOrder"]/.(Except[1|2]->1)
	];
	
	unitCrds=unitMesh["Coordinates"];
	crds=If[
		dim==3,
		zInt=ListInterpolation[restructured[[3]],{{0.,1.},{0.,1.}},InterpolationOrder->order];
		Transpose@{xInt@@@unitCrds,yInt@@@unitCrds,zInt@@@unitCrds},
		Transpose@{xInt@@@unitCrds,yInt@@@unitCrds}
	];
	
	(* We still check for quality and duplicate nodes, because of possible mesh distortions. *)
	If[
		dim==3,
		ToBoundaryMesh[
			"Coordinates"->crds,
			"BoundaryElements"->unitMesh["MeshElements"]
		],
		ToElementMesh[
			"Coordinates"->crds,
			"MeshElements"->unitMesh["MeshElements"]
		]
	]
];

StructuredMesh[raster_,{nx_Integer?Positive,ny_Integer?Positive,nz_Integer?Positive},opts:OptionsPattern[]]:=Module[
	{refinement,order,restructured,xInt,yInt,zInt,unitMesh,unitCrds,crds},
	
	If[
		Not@ArrayQ[raster,4,NumericQ],
		Message[StructuredMesh::array,4+1];Return[$Failed,Module]
	];
	refinement=OptionValue["Refinement"];
	If[
		Not@MemberQ[{Left,Right,Top,Bottom,Front,Back,None},refinement],
		Message[StructuredMesh::refinement,{Left,Right,Top,Bottom,Front,Back,None}];refinement=None
	];
	
	
	order=OptionValue[InterpolationOrder]/.Automatic->1;
	
	restructured=Transpose[N[raster],{4, 3, 2, 1}];
	xInt=ListInterpolation[restructured[[1]],{{0.,1.},{0.,1.},{0.,1.}},InterpolationOrder->order];
	yInt=ListInterpolation[restructured[[2]],{{0.,1.},{0.,1.},{0.,1.}},InterpolationOrder->order];
	zInt=ListInterpolation[restructured[[3]],{{0.,1.},{0.,1.},{0.,1.}},InterpolationOrder->order];
	
	unitMesh=MeshOrderAlteration[
		refinedUnitStructuredMesh[{nx,ny,nz},refinement],
		OptionValue["MeshOrder"]/.(Except[1|2]->1)
	];
	
	unitCrds=unitMesh["Coordinates"];
	crds=Transpose@{xInt@@@unitCrds,yInt@@@unitCrds,zInt@@@unitCrds};
	
	ToElementMesh[
		"Coordinates"->crds,
		"MeshElements"->unitMesh["MeshElements"]
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


(* This function makes sense only for 2nd order mesh. It makes all boundary nodes 
lie on theoretical circle or sphere and improves geometry of boundary. It works 
also for 3D. *)
improveUnitBallBoundary[mesh_ElementMesh]:=Module[
	{boundaryNodes,correctedNodes},
	boundaryNodes=Union@Flatten@ElementIncidents@mesh["BoundaryElements"];
	correctedNodes=MapAt[
		Normalize,
		mesh["Coordinates"],
		Partition[boundaryNodes,1]
	];
	
	ToElementMesh[
		"Coordinates"->correctedNodes,
		"MeshElements"->mesh["MeshElements"],
		"BoundaryElements"->mesh["BoundaryElements"],
		"PointElements"->mesh["PointElements"],
		"CheckIntersections"->False,
		"CheckQuality"->False,
		"DeleteDuplicateCoordinates"->False
	]
];


unitMeshSquareMethod[n_]:=RectangleMesh[{-1, -1},{1, 1},{n, n}];


unitMeshPolygonMethod[n_,refinement_?BooleanQ]:=Module[
	{squareMesh,sideMesh,d,raster,rotations},
	(* Size of internal square. If mesh smoothing is used in the end, then this value is not important. *)
	d=N[1/2];
	squareMesh=RectangleMesh[d*{-1,-1},d*{1, 1},{n,n}];

	(* Division in radial direction is chosen heuristicaly to give nice element size
	distribution (after Laplacian mesh smoothing). *)
	sideMesh=StructuredMesh[
		{{{d,-d},{1,-1}},{{d,d},{1,1}}},
		{Ceiling[n/4],n},
		"Refinement"->(refinement/.{True->Right,False->None})
	];
	rotations=RotationTransform[#,{0.,0.}]&/@(Range[4.]*Pi/2);
	
	MergeMesh@Join[{squareMesh},TransformMesh[sideMesh,#]&/@rotations]	
];


DiskMesh::usage="DiskMesh[{x, y}, r, n] creates structured mesh with n elements on Disk of radius r centered at {x,y}.";
DiskMesh::method="Values for option Method should be \"Polygon\", \"Square\" or Automatic.";
DiskMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 2.";

DiskMesh//Options={"Refinement"->False,"MeshOrder"->1,Method->Automatic};

DiskMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

DiskMesh[n_Integer,opts:OptionsPattern[]]:=DiskMesh[{0,0},1,n,opts];

DiskMesh[{x_,y_},r_,n_Integer,opts:OptionsPattern[]]:=Module[
	{method,order,unitSquare,unitDisk,rescale,unitCrds,crds},
	
	If[
		Not@TrueQ[n>=2],
		Message[DiskMesh::noelems,n];Return[$Failed,Module]
	];
	
	method=OptionValue[Method]/.(Automatic->"Polygon");	
	unitSquare=Switch[method,
		"Polygon",unitMeshPolygonMethod[n,TrueQ@OptionValue["Refinement"]],
		"Square",unitMeshSquareMethod[n],
		_,Message[DiskMesh::method];Return[$Failed,Module]
	];
	
	rescale=(Max[Abs@#]*Normalize[#])&;
	(* Scaling with Tan causes equal length of edges after projection to disk. *)
	unitDisk=SmoothenMesh@ToElementMesh[
		"Coordinates"->rescale/@Tan[Pi/4*unitSquare["Coordinates"]],
		"MeshElements"->unitSquare["MeshElements"],
		"CheckIntersections"->False,
		"CheckQuality"->False,
		"DeleteDuplicateCoordinates"->False
	];
	(* In case of "MeshOrder"\[Rule]2 position of boundary nodes has to be fixed again,
	because mesh smoothing works only on 1st order mesh. *)
	If[
		OptionValue["MeshOrder"]===2,
		unitDisk=improveUnitBallBoundary@MeshOrderAlteration[unitDisk,2]
	];
	unitCrds=unitDisk["Coordinates"];
	crds=(N[r]*unitCrds)+ConstantArray[N@{x,y},Length@unitCrds];
	
	ToElementMesh[
		"Coordinates"->crds,
		"MeshElements"->unitDisk["MeshElements"],
		"CheckIntersections"->False,
		"DeleteDuplicateCoordinates"->False
	]
];


(* ::Subsubsection::Closed:: *)
(*AnnulusMesh*)


AnnulusMesh::usage=
	"AnnulusMesh[{x, y}, {rIn, rOut}, {n\[Phi], nr}] creates mesh on Annulus with n\[Phi] elements in circumferential 
and nr elements in radial direction."<>"\n"<>
	"AnnulusMesh[{x, y}, {rIn, rOut}, {\[Phi]1, \[Phi]2}, {n\[Phi], nr}] creates mesh on Annulus from angle \[Phi]1 to \[Phi]2.";
AnnulusMesh::angle="Angle limits for Annulus must be distinct.";
AnnulusMesh::division="There should be more than one element for each Pi/2 sector of Annulus.";

AnnulusMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,_.}};

AnnulusMesh[{nfi_Integer?Positive,nr_Integer?Positive}]:=AnnulusMesh[{0,0},{1/2,1},{0,2*Pi},{nfi,nr}];

AnnulusMesh[{x_,y_},{rIn_,rOut_},{nfi_Integer?Positive,nr_Integer?Positive}]:=
	AnnulusMesh[{x,y},{rIn,rOut},{0,2*Pi},{nfi,nr}];

AnnulusMesh[{x_,y_},{rIn_,rOut_},{fi1_,fi2_},{nfi_Integer?Positive,nr_Integer?Positive}]:=Module[
	{min,max,raster},
	
	(* Sorting angle by size solves potential problems with inverted elements. *)
	{min,max}=MinMax[{fi1,fi2}];
	If[
		N[min]==N[max],
		Message[AnnulusMesh::angle];Return[$Failed,Module]
	];
	max=Clip[max,{min,min+2*Pi}];
	
	(* It makes no sense if element division in angular direction is too small. *)
	If[
		(max-min)/nfi>Pi/2,
		Message[AnnulusMesh::division];Return[$Failed,Module]
	];
	
	raster=N@{
		Table[rOut*{Cos[i],Sin[i]}+{x,y},{i,min,max,(max-min)/nfi}],
		Table[rIn*{Cos[i],Sin[i]}+{x,y},{i,min,max,(max-min)/nfi}]
	};
	StructuredMesh[raster,{nfi,nr}]
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


(* Some key ideas come from: https://mathematica.stackexchange.com/questions/85592 *)
SphereMesh::usage="SphereMesh[{x, y, z}, r, n] creates structured mesh with n elements on Sphere of radius r centered at {x,y,z}.";
SphereMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 2.";

SphereMesh//Options={"MeshOrder"->1};

SphereMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

SphereMesh[n_Integer,opts:OptionsPattern[]]:=SphereMesh[{0,0,0},1,n,opts];

SphereMesh[{x_,y_,z_},r_,n_Integer,opts:OptionsPattern[]]:=Module[
	{order,rescale,cuboid,cuboidShell,crds},
	
	If[TrueQ[n<2],Message[SphereMesh::noelems,n];Return[$Failed,Module]];
	order=OptionValue["MeshOrder"]/.(Except[1|2]->1);
	
	cuboidShell=MeshOrderAlteration[
		ToBoundaryMesh@CuboidMesh[{-1,-1,-1},{1,1,1},{n,n,n}],
		order
	];
	(* This special rescaling makes all element edges on sphere edge of the same length. *)
	rescale=(Max[Abs@#]*Normalize[#])&;
	crds=cuboidShell["Coordinates"];
	crds=rescale/@(r*Tan[N[Pi/4]*crds])+ConstantArray[{x,y,z},Length[crds]];
	
	ToBoundaryMesh[
		"Coordinates"->crds,
		"BoundaryElements"->cuboidShell["BoundaryElements"],
		"CheckIncidentsCompleteness"->False,
		"CheckIntersections"->False,
		"CheckQuality"->False,
		"DeleteDuplicateCoordinates"->False
	]
];


(* ::Subsubsection::Closed:: *)
(*SphericalShellMesh*)


SphericalShellMesh::usage=
"SphericalShellMesh[{x, y, z}, {rIn, rOut}, {n\[Phi], nr}] creates structured mesh on SphericalShell, 
with n\[Phi] elements in circumferential and nr elements in radial direction.";

SphericalShellMesh//Options={"MeshOrder"->Automatic};

SphericalShellMesh//SyntaxInformation={"ArgumentsPattern"->{_,_,_,OptionsPattern[]}};

SphericalShellMesh[{nfi_Integer,nr_Integer},opts:OptionsPattern[]]:=SphericalShellMesh[{0,0,0},{1/2,1},{nfi,nr},opts];

SphericalShellMesh[{x_,y_,z_},{rIn_,rOut_},{nfi_Integer,nr_Integer},opts:OptionsPattern[]]:=Module[
	{order,rescale,innerRaster,outerRaster,rotations,flatMesh,curvedMesh},
	
	order=OptionValue["MeshOrder"]/.Automatic->1;
	If[
		Not@MatchQ[order,1|2],
		Message[ToElementMesh::femmonv,order,1];Return[$Failed]
	];
	
	rescale=(Max[Abs@#]*Normalize[#])&;
	
	(* This special raster makes all element edges on disk edge of the same length. *)
	innerRaster=With[
		{pts=rIn*N@Tan@Subdivide[-Pi/4,Pi/4,nfi]},
		Map[Append[rIn], Outer[Reverse@*List,pts,pts], {2}]
	];
	outerRaster=With[
		{pts=rOut*N@Tan@Subdivide[-Pi/4,Pi/4,nfi]},
		Map[Append[rOut], Outer[Reverse@*List,pts,pts], {2}]
	];
	
	flatMesh=MeshOrderAlteration[
		StructuredMesh[{innerRaster,outerRaster},{nfi,nfi,nr}],
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

PrismMesh//SyntaxInformation={"ArgumentsPattern"->{_,_}};

PrismMesh[{n1_Integer,n2_Integer}]:=PrismMesh[{{0,0,0},{1,0,0},{0,1,0},{0,0,1},{1,0,1},{0,1,1}},{n1,n2}];

PrismMesh[corners_List,{n1_Integer,n2_Integer}]:=Module[
	{c,m1,m2,m3},
	If[
		Not@(TrueQ[n1>=2]&&EvenQ[n1]),
		Message[PrismMesh::noelems,n1];Return[$Failed,Module]
	];
	
	c=If[
		reorientPrismQ[corners],
		Join@@Reverse@TakeDrop[corners,3],
		corners
	];
	(* We make 3 meshes on hexahedron and merge them. This way also prism with 
	non-coplanar faces can be created accurately. *)
	m1=StructuredMesh[{
		{{c[[1]],Mean@c[[{1,2}]]},{Mean@c[[{1,3}]],Mean@c[[{1,2,3}]]}},
		{{c[[4]],Mean@c[[{4,5}]]},{Mean@c[[{4,6}]],Mean@c[[{4,5,6}]]}}
		},
		{n1/2,n1/2,n2}
	];
	m2=StructuredMesh[{
		{{c[[2]],Mean@c[[{2,3}]]},{Mean@c[[{1,2}]],Mean@c[[{1,2,3}]]}},
		{{c[[5]],Mean@c[[{5,6}]]},{Mean@c[[{4,5}]],Mean@c[[{4,5,6}]]}}
		},
		{n1/2,n1/2,n2}
	];
	m3=StructuredMesh[{
		{{c[[3]],Mean@c[[{1,3}]]},{Mean@c[[{2,3}]],Mean@c[[{1,2,3}]]}},
		{{c[[6]],Mean@c[[{4,6}]]},{Mean@c[[{5,6}]],Mean@c[[{4,5,6}]]}}
		},
		{n1/2,n1/2,n2}
	];
	
	MergeMesh[{m1,m2,m3}]
];


(* ::Section::Closed:: *)
(*End package*)


End[]; (* "`Private`" *)


EndPackage[];
