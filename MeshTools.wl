(* ::Package:: *)

(* ::Section::Closed:: *)
(*Header comments*)


(* :Title: MeshTools *)
(* :Context: MeshTools` *)
(* :Author: Pintar M, C3M, Slovenia *)
(* :Summary: Utilities for generating and manipulating ElementMesh objects. *)
(* :Copyright: C3M d.o.o., 2018 *)

(* :Acknowledgements: *)


(* ::Section::Closed:: *)
(*Begin package*)


(* Mathematica FEM functionality (context "NDSolve`FEM`") is needed. *)
BeginPackage["MeshTools`",{"NDSolve`FEM`"}];


(* ::Subsection::Closed:: *)
(*Messages*)


AddMeshMarkers::usage="AddMeshMarkers[mesh, marker] adds integer marker to all mesh elements.";
SelectElementsByMarker::usage="SelectElementsByMarker[mesh, marker] creates ElementMesh made only out of elements with selected marker.";

MergeMesh::usage="MergeMesh[list] merges a list of ElementMesh objects with the same embedding dimension.";
TransformMesh::usage="TransformMesh[mesh, tfun] transforms ElementMesh mesh according to TransformationFunction tfun";
ExtrudeMesh::usage="ExtrudeMesh[mesh, thickness, layers] extrudes 2D quadrilateral mesh to 3D hexahedron mesh.";

SmoothenMesh::usage="SmoothenMesh[mesh] improves the quality of 2D mesh.";
ToTriangleMesh::usage="ToTriangleMesh[mesh] converts quadrilateral mesh to triangle mesh.";
ToTetrahedronMesh::usage="ToTetrahedronMesh[mesh] converts hexahedral mesh to tetrahedral mesh.";

ElementMeshCurvedWireframe::usage="ElementMeshCurvedWireframe[mesh] draws accurately second order 2D mesh.";

MeshElementMeasure::usage="MeshElementMeasure[mesh] gives the measure of each mesh element.";
BoundaryElementMeasure::usage="BoundaryElementMeasure[mesh] gives the measure of each boundary element.";

RectangleMesh::usage="RectangleMesh[{x1, y1},{x2, y2}, {nx, ny}] creates structured mesh on Rectangle.";
CuboidMesh::usage="CuboidMesh[{x1, y1, z1}, {x2, y2, z2}, {nx, ny, nz}] creates structured mesh of hexahedra on Cuboid.";
TriangleMesh::usage="Triangle[{p1, p2, p3}, n] creates triangular mesh on Triangle with corners p1, p2 and p3.";

DiskMesh::usage="DiskMesh[{x, y}, r, n] creates structured mesh with n elements on Disk of radius r centered at {x,y}.";
AnnulusMesh::usage="AnnulusMesh[{x, y}, {rIn, rOut}, {\[Phi]1, \[Phi]2}, {n\[Phi], nr}] creates mesh on Annulus with n\[Phi] elements in circumferential and nr elements in radial direction.";

SphereMesh::usage="SphereMesh[{x, y, z}, r, n] creates structured mesh with n elements on Sphere of radius r centered at {x,y,z}.";
SphericalShellMesh::usage="SphericalShellMesh[{x, y, z}, {rIn, rOut}, {n\[Phi], nr}]";
BallMesh::usage="BallMesh[{x, y, z}, r, n] creates structured mesh with n elements on Ball of radius r centered at {x,y,z}.";
TetrahedronMesh::usage="TetrahedronMesh[{p1,p2,p3,p4}, n] creates tetrahedral mesh on Tetrahedron with corners p1, p2, p3 and p4.";

DiskVoidMesh::usage="DiskVoidMesh[voidRadius, squareSize, noElements] creates a mesh with disk shaped void in square domain.";
SphericalVoidMesh::usage="SphericalVoidMesh[voidRadius, cuboidSize, noElements] creates a mesh with spherical void in cuboid domain.";

EllipsoidVoidMesh::usage="EllipsoidVoidMesh[radius, noElements] creates a mesh with spherical void.
EllipsoidVoidMesh[{r1, r2, r3}, noElements] creates a mesh with ellipsoid void with semi-axis radii r1, r2 and r3.";
RodriguesSpaceMesh::usage="RodriguesSpaceMesh[n] creates mesh for Rodrigues space used in metal texture analysis.";


(* ::Section::Closed:: *)
(*Code*)


(* Begin private context *)
Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Mesh operations*)


(* ::Subsubsection::Closed:: *)
(*AddMeshMarkers*)


AddMeshMarkers[mesh_ElementMesh,marker_Integer]:=Module[{
	elementType,head,elementTypes,elementIncidents,elementMarkers
	},
	
	{elementType,head}=If[
		mesh["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	elementTypes=Head/@mesh[elementType];
	elementIncidents=ElementIncidents@mesh[elementType];
	elementMarkers=ConstantArray[marker,#]&/@(Length/@elementIncidents);
	
	head[
		"Coordinates"->mesh["Coordinates"],
		elementType->MapThread[#1[#2,#3]&,{elementTypes,elementIncidents,elementMarkers}]
	]
]


(* ::Subsubsection::Closed:: *)
(*SelectElementsByMarker*)


SelectElementsByMarker::marker="Specified marker `1` does not exist and unmodified ElementMesh is returned.";

SelectElementsByMarker[mesh_ElementMesh,marker_Integer]:=Module[
	{elementType,head,elementHeads,connectivity,allMarkers,chosenElements,chosenNodeNum,renumbering},
	
		{elementType,head}=If[
		mesh["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	elementHeads=Head/@mesh[elementType];
	connectivity=ElementIncidents@mesh[elementType];
	allMarkers=ElementMarkers@mesh[elementType];
	
	If[
		Not@MemberQ[Union@Flatten@allMarkers,marker],
		Message[SelectElementsByMarker::marker,marker];Return[mesh]
	];
	
	chosenElements=MapThread[Pick[#1,#2,marker]&,{connectivity,allMarkers}];
	chosenNodeNum=Union@Flatten@chosenElements;
	renumbering=Thread[chosenNodeNum->Range@Length@chosenNodeNum];

	head[
		"Coordinates" -> Part[mesh["Coordinates"],chosenNodeNum],
		elementType -> MapThread[#1[#2]&,{elementHeads,chosenElements/.renumbering}]
	]
]


(* ::Subsubsection::Closed:: *)
(*TransformMesh*)


(* Argument must be an expression of the form ElementType[incidents,markers] *)
reorderNodes[elements_]:=Module[
	{head,conn,markers,length,numbering},
	head=Head[elements];
	conn=First[elements];
	markers=If[Length[elements]===2,elements[[2]],Nothing];
	length=Last@Dimensions[conn];
	numbering=Take[
		head/.{
			TriangleElement->{2,1,3,4,6,5},
			QuadElement->{2,1,4,3,5,8,7,6},
			TetrahedronElement->{3,2,1,4,6,5,7,8,10,9},
			HexahedronElement->{2,1,4,3,6,5,8,7,9,12,11,10,13,16,15,14,18,17,20,19}
			},
		length
	];
	
	head[Sequence@@{conn[[All,numbering]],markers}]
]


TransformMesh[mesh_ElementMesh,tfun_TransformationFunction]:=Module[{
	elementsType,head,elements,transformedElements,reflectionQ
	},
	
	{elementsType,head}=If[
		mesh["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	elements=mesh[elementsType];
	
	(* If TransformationFunction means reflection (or inversion), then reorder element incidents. *)
	transformedElements=If[
		Negative@Det[TransformationMatrix[tfun]],
		reorderNodes/@elements,
		elements
	];
	
	head[
		"Coordinates"->tfun/@mesh["Coordinates"],
		elementsType->transformedElements
	]
]


(* ::Subsubsection::Closed:: *)
(*MergeMesh*)


(* 
Code is adjusted after this source: 
https://mathematica.stackexchange.com/questions/156445/automatically-generating-boundary-meshes-for-region-intersections 
*)
MergeMesh::order="Meshes must have the same \"MeshOrder\".";
MergeMesh::dim="Meshes must have the same \"EmbeddingDimension\".";

MergeMesh[list_List/;Length[list]>=2]:=Fold[MergeMesh,list]

MergeMesh[mesh1_,mesh2_]:=Module[
	{elementType,head,c1,c2,newCrds,newElements,elementTypes,elementMarkers,inci1,inci2},
	
	If[mesh1["MeshOrder"]=!=mesh2["MeshOrder"],Message[MergeMesh::order];Return[$Failed]];
	If[mesh1["EmbeddingDimension"]=!=mesh2["EmbeddingDimension"],Message[MergeMesh::dim];Return[$Failed]];
		
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
		"DeleteDuplicateCoordinates"->True (* already a default option *)
	]
]


(* ::Subsubsection::Closed:: *)
(*ExtrudeMesh*)


ExtrudeMesh::badType="Only first order 2D quadrilateral mesh is supported.";

ExtrudeMesh[mesh_ElementMesh,thickness_/;thickness>0,layers_Integer?Positive]:=Module[{
	n,nodes2D,nodes3D,elements2D,elements3D,markers2D,markers3D
	},
	If[
		Or[mesh["MeshOrder"]=!=1,(Head/@mesh["MeshElements"])=!={QuadElement},mesh["EmbeddingDimension"]=!=2],
		Message[ExtrudeMesh::badType];Return[$Failed]
	];
		
	nodes2D=mesh["Coordinates"];
	elements2D=Join@@ElementIncidents[mesh["MeshElements"]];
	markers2D=Join@@ElementMarkers[mesh["MeshElements"]];
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
]


(* ::Subsubsection::Closed:: *)
(*Mesh smoothing*)


(* 
This implements Laplacian mesh smoothing method as described in 
https://mathematica.stackexchange.com/a/156669 
*)
SmoothenMesh::badType="Smoothing is only supported for first order 2D meshes.";

SmoothenMesh[mesh_ElementMesh]:=Block[
	{n,vec,mat,adjacencymatrix2,mass2,laplacian2,bndvertices2,interiorvertices,stiffness,load,newCoords},
	
	If[
		Or[mesh["MeshOrder"]=!=1,mesh["EmbeddingDimension"]=!=2],
		Message[SmoothenMesh::badType];Return[$Failed]
	];
	
	n=Length[mesh["Coordinates"]];
	vec=mesh["VertexElementConnectivity"];
	mat=Unitize[vec.Transpose[vec]];
	vec=Null;
	adjacencymatrix2=mat-DiagonalMatrix[Diagonal[mat]];
	mass2=DiagonalMatrix[SparseArray[Total[adjacencymatrix2,{2}]]];
	stiffness=N[mass2-adjacencymatrix2];
	adjacencymatrix2=Null;
	mass2=Null;
	bndvertices2=Flatten[Join@@ElementIncidents[mesh["PointElements"]]];
	interiorvertices=Complement[Range[1,n],bndvertices2];
	stiffness[[bndvertices2]]=IdentityMatrix[n,SparseArray][[bndvertices2]];
	load=ConstantArray[0.,{n,mesh["EmbeddingDimension"]}];
	load[[bndvertices2]]=mesh["Coordinates"][[bndvertices2]];
	newCoords=LinearSolve[stiffness,load(*,Method\[Rule]"Pardiso"*)];
	
	ToElementMesh[
		"Coordinates"->newCoords,
		"MeshElements"->mesh["MeshElements"],
		"BoundaryElements"->mesh["BoundaryElements"],
		"PointElements"->mesh["PointElements"],
		"CheckIncidentsCompletness"->False,
		"CheckIntersections"->False,
		"DeleteDuplicateCoordinates"->False
	]
]


(* ::Subsubsection::Closed:: *)
(*ToTriangleMesh*)


ToTriangleMesh::order="Only the first order mesh is currently supported.";

ToTriangleMesh[mesh_ElementMesh]:=Module[{
	elementType,head,conn,triangles
	},
	If[mesh["MeshOrder"]=!=1,Message[ToTriangleMesh::order];Return[$Failed]];
	
	{elementType,head}=If[
		mesh["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	conn=ElementIncidents@First@mesh[elementType];
	triangles=Join[conn[[All,{1,2,3}]],conn[[All,{1,3,4}]]];
	
	head[
		"Coordinates"->mesh["Coordinates"],
		elementType->{TriangleElement[triangles]}
	]
]


(* ::Subsubsection::Closed:: *)
(*ToTetrahedronMesh*)


ToTetrahedronMesh::type="ElementMesh should contain only hexadedral elements.";

ToTetrahedronMesh[mesh_ElementMesh]:=Module[{
	nodes,origElms,tetConnect,restructure,newElms
	},
	origElms=mesh["MeshElements"];
	
	If[Head@First[origElms]=!=HexahedronElement,Message[ToTetrahedronMesh::type];Return[$Failed]];
	
	tetConnect={
		{4, 1, 2, 5},
		{7, 5, 2, 6},
		{4, 2, 3, 7},
		{4, 5, 2, 7},
		{4, 5, 7, 8}
	};
	restructure=Function[{hexNodes},Part[hexNodes,#]&/@tetConnect];
	
	newElms=TetrahedronElement[
		Flatten[restructure/@First@ElementIncidents[origElms],1]
	];
	
	ToElementMesh[
		"Coordinates"->mesh["Coordinates"],
		"MeshElements"->{newElms}
	]
]


(* ::Subsubsection::Closed:: *)
(*ToQuadMesh*)


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

(* Currently ToQuadMesh function is kept in the private context to avoid shadowing with
the same function in FEMAddOns paclet from WRI. *)
ToQuadMesh::usage="ToQuadMesh[mesh] converts triangular mesh to quadrilateral mesh.";

ToQuadMesh::elmtype = "Only conversion of pure triangular meshes is supported."

distortion := distortion = Compile[
		{{n1, _Real, 1}, {n2, _Real, 1}, {n3, _Real, 1}, {n4, _Real, 1}},
		2/Pi * Max @ Map[
			Abs[Pi/2 - If[ # < 0, Pi - #, #] ]& @ 
				ArcTan[	#[[1, 1]] #[[2, 1]] + #[[1, 2]] #[[2, 2]],
						#[[1, 1]] #[[2, 2]] - #[[1, 2]] #[[2, 1]]
				]&,
			{{n2-n1, n4-n1}, {n3-n2, n1-n2}, {n4-n3, n2-n3}, {n1-n4, n3-n4}}
			]
	];

ToQuadMesh[meshIn_] /; ElementMeshQ[meshIn] && !BoundaryElementMeshQ[meshIn] &&
	meshIn["EmbeddingDimension"] === 2 :=
Module[
	{coor, econn, elem, dist, ncoor, taken, quad, triag, edge, nc, allquads, 
	alltriag, marker, nodes, markerQ, mesh, increaseOrderQ},

	mesh = meshIn;

	If[ mesh["MeshOrder"] =!= 1,
		mesh = MeshOrderAlteration[mesh, 1]];
		increaseOrderQ = True;

	If[ Union[Head /@ mesh["MeshElements"]] =!= {TriangleElement},
		Message[ToQuadMesh::elmtype]; Return[$Failed, Module]
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
]


(* ::Subsection::Closed:: *)
(*Mesh measurements*)


(* ::Subsubsection::Closed:: *)
(*MeshElementMeasure*)


Clear[elementMeasure]

(* Definition for multiple elements in a list. *)
elementMeasure[nodes_List/;(Depth[nodes]==4),type_,order_]:=elementMeasure[#,type,order]&/@nodes

(* Length of LineElement (as "MeshElement") is calculated differently. *)
elementMeasure[nodes_List/;(Depth[nodes]==3),LineElement,order_]:=Abs[Differences@Flatten@Take[nodes,2]]

elementMeasure[nodes_List/;(Depth[nodes]==3),type_,order_]:=Block[{
	igCrds=ElementIntegrationPoints[type,order],
	igWgts=ElementIntegrationWeights[type,order],
	shapeDerivative=ElementShapeFunctionDerivative[type,order],
	jacobian,r,s,t
	},
	
	jacobian=With[{
		vars=(type/.{
			TriangleElement|QuadElement->{r,s},
			TetrahedronElement|HexahedronElement->{r,s,t}
		})
		},
		Function[Evaluate@vars,Evaluate@Det[(shapeDerivative@@vars).nodes]]
	];
	
	(jacobian@@@igCrds).igWgts
]


(* This function gives the same result as asking for the property mesh["MeshElementMeasure"] *)
MeshElementMeasure[mesh_ElementMesh]:=Module[{
	order=mesh["MeshOrder"],
	elements=mesh["MeshElements"],
	nodes=mesh["Coordinates"],
	elementCoordinates,
	elementTypes
	},
	elementCoordinates=Map[Part[nodes,#]&,ElementIncidents@elements,{2}];
	elementTypes=Head/@elements;
	
	MapThread[
		elementMeasure[#1,#2,order]&,
		{elementCoordinates,elementTypes}
	]
]


(* ::Subsubsection::Closed:: *)
(*BoundaryElementMeasure*)


Clear[boundaryElementMeasure]

(* Boundary mesh measure for each submesh. *)
boundaryElementMeasure[
	nodes_List/;(Depth[nodes]==4),
	type_,
	order_,
	integrationOrder_]:=
	boundaryElementMeasure[#,type,order,integrationOrder]&/@nodes

(* Boundary mesh measure for each 1D element. *)
boundaryElementMeasure[
	nodes_List/;(Depth[nodes]==3),
	type_/;(type==LineElement),
	order_,
	integrationOrder_]:=Block[{
		f,\[Xi],
		igCrds=ElementIntegrationPoints[type,integrationOrder],
		igWgts=ElementIntegrationWeights[type,integrationOrder]
		},
		f=Function[{\[Xi]},Cross@@(ElementShapeFunctionDerivative[type,order][\[Xi]].nodes//Simplify )//Norm];
		igWgts.(f@@@igCrds)
	]

(* Boundary mesh measure for each 2D element. *)
boundaryElementMeasure[
	nodes_List/;(Depth[nodes]==3),
	type_,
	order_,
	integrationOrder_]:=Block[{
		f,\[Xi],\[Eta],
		igCrds=ElementIntegrationPoints[type,integrationOrder],
		igWgts=ElementIntegrationWeights[type,integrationOrder]
		},
		f=Function[{\[Xi],\[Eta]},Cross@@(ElementShapeFunctionDerivative[type,order][\[Xi],\[Eta]].nodes//Simplify )//Norm];
		igWgts.(f@@@igCrds)
	]


(*
This function returns the surface of boundary elements in 3D embedding and length of 
boundary elements in 2D embedding.
*)

BoundaryElementMeasure[mesh_ElementMesh,integrationOrder_:3]:=Module[
	{order=mesh["MeshOrder"],
	elements=mesh["BoundaryElements"],
	nodes=mesh["Coordinates"],
	elementCoordinates,
	elementTypes
	},
	elementCoordinates=Map[Part[nodes,#]&,ElementIncidents@elements,{2}];
	elementTypes=Head/@mesh["BoundaryElements"];
	MapThread[
		boundaryElementMeasure[#1,#2,order,integrationOrder]&,
		{elementCoordinates,elementTypes}]
	]


(* ::Subsection::Closed:: *)
(*Mesh visualization*)


(* 
Function to visualize 2D second order FEM mesh. 
Copied from: WTC 2017 talk "Finite Element Method: How to talk to it and make it your friend" by Paritosh Mokhasi 
*)

ElementMeshCurvedWireframe[mesh_ElementMesh]:=Block[
	{triIncidentsOrder,quadIncidentsOrder,interpolatingQuadBezierCurve,interpolatingQuadBezierCurveComplex,
	triEdges,triIncidents,coordinates},

	triIncidentsOrder={1,4,2,5,3,6};
	quadIncidentsOrder={1,5,2,6,3,7,4,8};

	interpolatingQuadBezierCurve[pts_List]/;Length[pts]==3:=
		BezierCurve[{pts[[1]],(1/2)(-pts[[1]]+4 pts[[2]]-pts[[3]]),pts[[3]]}];
	interpolatingQuadBezierCurve[pts_List,"ControlPoints"]/;(Length[pts]==3):=
		{pts[[1]],(1/2)(-pts[[1]]+4 pts[[2]]-pts[[3]]),pts[[3]]};
	interpolatingQuadBezierCurve[ptslist_List]:=interpolatingQuadBezierCurve/@ptslist;

	interpolatingQuadBezierCurveComplex[coords_,indices_]:=interpolatingQuadBezierCurve[Map[coords[[#]]&,indices]];

	triEdges=Partition[triIncidentsOrder, 3, 2, {1,1}];
	triIncidents=Join@@ElementIncidents[mesh["MeshElements"]];
	coordinates=mesh["Coordinates"];
	
	Graphics[{
		interpolatingQuadBezierCurveComplex[coordinates,triIncidents[[All, #]]]&/@triEdges
	}]
]


(* ::Subsection::Closed:: *)
(*Structured mesh*)


getElementConnectivity[nx_,ny_]:=Flatten[
	Table[{
		i+(j-1)(nx+1),
		i+(j-1)(nx+1)+1,
		i+j(nx+1)+1,
		i+j(nx+1)
		},
		{j,1,ny},
        {i,1,nx}
   ],
   1
]

(* =====================================================================\[Equal] *)

getElementConnectivity[nx_,ny_,nz_]:=Flatten[
	Table[{
		i+(j-1)(nx+1)+(k-1)(nx+1)(ny+1),
		i+(j-1)(nx+1)+(k-1)(nx+1)(ny+1)+1,
		i+j(nx+1)+(k-1)(nx+1)(ny+1)+1,
		i+j(nx+1)+(k-1)(nx+1)(ny+1),
		i+(j-1)(nx+1)+k(nx+1)(ny+1),
		i+(j-1)(nx+1)+k(nx+1)(ny+1)+1,
		i+j(nx+1)+k(nx+1)(ny+1)+1,
		i+j(nx+1)+k(nx+1)(ny+1)
        },
        {k,1,nz},
        {j,1,ny},
        {i,1,nx}
    ],
    2
]


(* This function is kept in private context to avoid shadowing with the same function in
FEMAddOns paclet (https://github.com/WolframResearch/FEMAddOns). *)
StructuredMesh::usage="StructuredMesh[raster,{nx,ny}] creates structured mesh of quadrilaterals.
StructuredMesh[raster,{nx,ny,nz}] creates structured mesh of hexahedra.";

StructuredMesh::array="Raster of input points must be full array of numbers with depth of `1`.";

StructuredMesh//Options={InterpolationOrder->1};
StructuredMesh[raster_,{nx_,ny_},opts:OptionsPattern[]]:=Module[
    {order,dim,restructured,xInt,yInt,zInt,nodes,connectivity},
    If[Not@ArrayQ[raster,3,NumericQ],Message[StructuredMesh::array,3+1];Return[$Failed]];

    order=OptionValue[InterpolationOrder]/.Automatic->1;
    dim=Last@Dimensions[raster];

    restructured=Transpose[raster,{3,2,1}];
    xInt=ListInterpolation[restructured[[1]],{{0,1},{0,1}},InterpolationOrder->order];
    yInt=ListInterpolation[restructured[[2]],{{0,1},{0,1}},InterpolationOrder->order];

    nodes=Flatten[#,1]&@If[dim==3,
        zInt=ListInterpolation[restructured[[3]],{{0,1},{0,1}},InterpolationOrder->order];
        Table[{xInt[i,j],yInt[i,j],zInt[i,j]},{j,0,1,1./ny},{i,0,1,1./nx}]
        ,
        Table[{xInt[i,j],yInt[i,j]},{j,0,1,1./ny},{i,0,1,1./nx}]
    ];

    connectivity=getElementConnectivity[nx,ny];

    If[dim==3,
        ToBoundaryMesh["Coordinates"->nodes,"BoundaryElements"->{QuadElement[connectivity]}],
        ToElementMesh["Coordinates"->nodes,"MeshElements"->{QuadElement[connectivity]}]
    ]
]

(* ===================================================================================== *)

StructuredMesh[raster_,{nx_,ny_,nz_},opts:OptionsPattern[]]:=Module[
    {order,restructured,xInt,yInt,zInt,nodes,connectivity},
    If[Not@ArrayQ[raster,4,NumericQ],Message[StructuredMesh::array,4+1];Return[$Failed]];

    order=OptionValue[InterpolationOrder]/.Automatic->1;
       
    restructured=Transpose[raster,{4, 3, 2, 1}];
    xInt=ListInterpolation[restructured[[1]],{{0,1},{0,1},{0,1}},InterpolationOrder->order];
    yInt=ListInterpolation[restructured[[2]],{{0,1},{0,1},{0,1}},InterpolationOrder->order];
    zInt=ListInterpolation[restructured[[3]],{{0,1},{0,1},{0,1}},InterpolationOrder->order];
    
    nodes=Flatten[
       Table[
          {xInt[i,j,k],yInt[i,j,k],zInt[i,j,k]},
          {k,0,1,1./nz},{j,0,1,1./ny},{i,0,1,1./nx}
       ],
       2
    ];

    connectivity=getElementConnectivity[nx,ny,nz];
    
    ToElementMesh["Coordinates"->nodes,"MeshElements"->{HexahedronElement[connectivity]}]
]


(* ::Subsection::Closed:: *)
(*Shape meshes 2D*)


(* ::Subsubsection::Closed:: *)
(*RectangleMesh*)


RectangleMesh[{x1_,y1_},{x2_,y2_},{nx_,ny_}]:=StructuredMesh[{
	{{x1,y1},{x2,y1}},{{x1,y2},{x2,y2}}},
	{nx,ny}
];


(* ::Subsubsection::Closed:: *)
(*TriangleMesh*)


(* From documentation page on Triangle *)
symmetricSubdivision[Triangle[pl_],k_]/;0<=k<2^Length[pl]:=Module[
	{n=Length[pl]-1,i0,bl,pos},
	
	i0=DigitCount[k,2,1]; 
	bl=IntegerDigits[k,2,n];
	pos=FoldList[If[#2==0,#1+{0,1},#1+{1,0}]&,{0,i0},Reverse[bl]];
	Triangle@Map[Mean,Extract[pl,#]&/@Map[{#}&,pos+1,{2}]] 
]


(* From documentation page on Triangle *)
nestedSymmetricSubdivision[Triangle[pl_],level_Integer]/;level==0:=Triangle[pl]

nestedSymmetricSubdivision[Triangle[pl_],level_Integer]/;level>0:=Flatten[
	nestedSymmetricSubdivision[symmetricSubdivision[Triangle[pl],#],level-1]&/@Range[0,3],
	1
]


(* Helper function to check the orientation of nodes used in Triangle *)
reorientQ[{a_,b_,c_}]:=Negative@Det[{a-c,b-c}]


TriangleMesh::noelms="Currently only 2, 4, 8, 16 or 32 elements per edge are allowed.";

(* TODO: It would be really nice if Triangle could be split to arbitrary 
number of elements per edge.*)
TriangleMesh[pts_,n_Integer]:=Module[
	{divisions,f,allCrds,nodes,connectivity},
	
	If[Not@MemberQ[{2,4,8,16,32},n],Message[TriangleMesh::noelms];Return[$Failed]];
	
	divisions=Log[2,n];
	f=If[reorientQ[#],#[[{1,3,2}]],#]&;
	allCrds=f/@(Join@@List@@@nestedSymmetricSubdivision[Triangle[pts],divisions]);
	nodes=DeleteDuplicates@Flatten[allCrds,1];
	connectivity=With[
		{rules=Thread[nodes->Range@Length[nodes]]},
		Replace[allCrds,rules,{2}]
	];
	
	ToElementMesh[
		"Coordinates"->nodes,
		"MeshElements"->{TriangleElement[connectivity]}
	]
]


(* ::Subsubsection::Closed:: *)
(*DiskMesh*)


diskMeshProjection[{x_,y_},r_,n_Integer/;(n>=2)]:=Module[{
	square,rescale,coordinates
	},
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
]


squareMesh[{x_,y_},r_,n_]:=StructuredMesh[
	{{{x-r,y-r},{x+r,y-r}},{{x-r,y+r},{x+r,y+r}}},
	{n,n}
]


diskMeshBlock[{x_,y_},r_,n_Integer/;(n>=2)]:=Module[
	{square,sideMesh,d,raster,rotations},
	(* Size of internal square. Value is chosen to optimize minimal element quality. *)
	d=0.2*r;
	square=squareMesh[{x,y},d,n];
	
	raster={
		Thread[{x+Subdivide[-d,d,90],y+d}],
		N@Table[{x+r*Cos[fi],y+r*Sin[fi]},{fi,3Pi/4,Pi/4,-Pi/180}]
	};
	sideMesh=StructuredMesh[raster,{n,n}];
	rotations=RotationTransform[#,{x,y}]&/@(Range[4]*Pi/2);
	
	MergeMesh@Join[{square},TransformMesh[sideMesh,#]&/@rotations]	
]


DiskMesh::method="Method \"`1`\" is not supported.";
DiskMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 2.";

DiskMesh//Options={Method->Automatic};

DiskMesh[n_,opts:OptionsPattern[]]:=DiskMesh[{0,0},1,n,opts]

DiskMesh[{x_,y_},r_,n_,opts:OptionsPattern[]]/;If[TrueQ[n>=2&&IntegerQ[n]],True,Message[DiskMesh::noelems,n];False]:=Module[
	{squareMesh,order,method,mesh},
	
	method=OptionValue[Method]/.Automatic->"Block";
		
	If[
		Not@MemberQ[{"Block","Projection"},method],
		Message[DiskMesh::method,method];Return[$Failed]
	];
	
	mesh=Switch[method,
		"Block",diskMeshBlock[{x,y},r,n],
		"Projection",diskMeshProjection[{x,y},r,n]
	];
	mesh
]


(* ::Subsubsection::Closed:: *)
(*AnnulusMesh*)


AnnulusMesh[{n\[Phi]_Integer,nr_Integer}]:=AnnulusMesh[{0,0},{1/2,1},{0,2Pi},{n\[Phi],nr}]

AnnulusMesh[{x_,y_},{rIn_,rOut_},{\[Phi]1_,\[Phi]2_},{n\[Phi]_Integer,nr_Integer}]:=Module[
	{raster},
	raster=N@{
		Table[rOut*{Cos[fi],Sin[fi]}+{x,y},{fi,\[Phi]1,\[Phi]2,(\[Phi]2-\[Phi]1)/n\[Phi]}],
		Table[rIn*{Cos[fi],Sin[fi]}+{x,y},{fi,\[Phi]1,\[Phi]2,(\[Phi]2-\[Phi]1)/n\[Phi]}]
	};
	StructuredMesh[raster,{n\[Phi],nr}]
]


(* ::Subsubsection::Closed:: *)
(*DiskVoidMesh*)


DiskVoidMesh[voidRadius_,squareSize_,noElements_Integer,opts:OptionsPattern[]]:=Module[
	{r,s,n,raster,half},
		
	s=squareSize;
	r=Clip[voidRadius,{0.01,Infinity}];
	n=noElements;
	
	raster=N@{
		Table[{s,y},{y,0,s,s/n}],
		Table[r*{Cos[fi],Sin[fi]},{fi,0,Pi/4,(Pi/4)/n}]
	};
	half=StructuredMesh[raster,{n,Ceiling[(s/r/2)*n]}];
	
	MergeMesh[{
		half,
		TransformMesh[half,ReflectionTransform[{-1,1},{0,0}]]
	}]
]


(* ::Subsection::Closed:: *)
(*Shape meshes 3D*)


(* ::Subsubsection::Closed:: *)
(*CuboidMesh*)


CuboidMesh[{x1_,y1_,z1_},{x2_,y2_,z2_},{nx_,ny_,nz_}]:=StructuredMesh[{
	{{{x1,y1,z1},{x2,y1,z1}},{{x1,y2,z1},{x2,y2,z1}}},
	{{{x1,y1,z2},{x2,y1,z2}},{{x1,y2,z2},{x2,y2,z2}}}
	},
	{nx,ny,nz}
];


(* ::Subsubsection::Closed:: *)
(*SphereMesh*)


sphereMeshBlock[{x_,y_,z_},r_,n_Integer/;(n>=2)]:=Module[
	{rescale,bottomRaster,topRaster,cubeMesh,sideMesh,d,rotations,unitCube},
	(* Size of internal square. Size is determined to optimize the minimal quality of all elements. *)
	d=0.2;
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
	sideMesh=StructuredMesh[{bottomRaster,topRaster},{n,n,n}];
	
	rotations=Join[
		RotationTransform[#,{1,0,0}]&/@(Range[4]*Pi/2),
		RotationTransform[#,{0,1,0}]&/@{Pi/2,3Pi/2}
	];
	
	unitCube=MergeMesh@Join[{cubeMesh},TransformMesh[sideMesh,#]&/@rotations];
	
	ToElementMesh[
		"Coordinates" ->Transpose[Transpose[r*unitCube["Coordinates"]]+{x,y,z}],
		"MeshElements" -> unitCube["MeshElements"]
	]
]


(* 
Some key ideas for this code come from the answer by "Michael E2" on topic: 
https://mathematica.stackexchange.com/questions/85592/how-to-create-an-elementmesh-of-a-sphere
*)
sphereMeshProjection[{x_,y_,z_},r_,n_Integer/;(n>=2)]:=Module[{
	rescale,cuboidMesh,coordinates
	},
	rescale=(Max[Abs@#]*Normalize[#])&;
	(* This special raster makes all element edges on sphere edge of the same length. *)
	cuboidMesh=With[
		{pts=r*N@Tan@Subdivide[-Pi/4,Pi/4,n]},
		StructuredMesh[Outer[Reverse@*List,pts,pts,pts],{n,n,n}]
	];
	(* If we do order alteration (more than 1st order) before projection, then geometry is
	more accurate and elements have curved edges. *)
	(*cuboidMesh=MeshOrderAlteration[cuboidMesh,order];*)
	
	coordinates=Transpose[Transpose[rescale/@cuboidMesh["Coordinates"]]+{x,y,z}];
	
	ToElementMesh[
		"Coordinates" -> coordinates,
		"MeshElements" -> cuboidMesh["MeshElements"]
	]
]


SphereMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 2.";
SphereMesh::method="Method \"`1`\" is not supported.";
SphereMesh//Options={Method->Automatic};

SphereMesh[n_,opts:OptionsPattern[]]:=SphereMesh[{0,0,0},1,n,opts]

SphereMesh[{x_,y_,z_},r_,n_,opts:OptionsPattern[]]:=Module[
	{order,method,mesh},
	If[TrueQ[n<2]||Not@IntegerQ[n],Message[SphereMesh::noelems,n];Return[$Failed]];
	
	method=OptionValue[Method]/.Automatic->"Block";
		
	If[
		Not@MemberQ[{"Block","Projection"},method],
		Message[SphereMesh::method,method];Return[$Failed]
	];
	
	mesh=Switch[method,
		"Block",sphereMeshBlock[{x,y,z},r,n],
		"Projection",sphereMeshProjection[{x,y,z},r,n]
	];
	mesh
]


(* ::Subsubsection:: *)
(*SphericalShellMesh*)


(* ::Subsubsection::Closed:: *)
(*BallMesh*)


ballMeshBlock//Clear;
ballMeshBlock[{x_,y_,z_},r_,n_Integer/;(n>=1)]:=Module[
	{rescale,bottomRaster,topRaster,cubeMesh,sideMesh,d,rotations,firstOctant},
	(* Size of internal square. Size is determined to optimize the minimal quality of all elements. *)
	d=0.2;
	rescale=(Max[Abs@#]*Normalize[#])&;
	
	bottomRaster=With[
		{pts=d*N@Subdivide[0,1,n]},
		Map[Append[d], Outer[Reverse@*List,pts,pts], {2}]
	];
	
	(* This special raster makes all element edges on disk edge of the same length. *)
	topRaster=With[
		{pts=N@Tan@Subdivide[0,Pi/4,n]},
		Map[rescale@*Append[1.], Outer[Reverse@*List,pts,pts], {2}]
	];
	
	cubeMesh=CuboidMesh[{0,0,0},d*{1,1,1},{n,n,n}];
	sideMesh=StructuredMesh[{bottomRaster,topRaster},{n,n,2*n}];
	
	rotations=RotationTransform[#,{1,1,1}]&/@({0,1,2}*2Pi/3);
	
	firstOctant=MergeMesh@Join[{cubeMesh},TransformMesh[sideMesh,#]&/@rotations];
	
	ToElementMesh[
		"Coordinates" ->Transpose[Transpose[r*firstOctant["Coordinates"]]+{x,y,z}],
		"MeshElements" -> firstOctant["MeshElements"]
	]
]


(* 
Some key ideas for this code come from the answer by "Michael E2" on topic: 
https://mathematica.stackexchange.com/questions/85592/how-to-create-an-elementmesh-of-a-sphere
*)
ballMeshProjection//Clear;
ballMeshProjection[{x_,y_,z_},r_,n_Integer/;(n>=1)]:=Module[{
	rescale,cuboidMesh,coordinates
	},
	rescale=(Max[Abs@#]*Normalize[#])&;
	(* This special raster makes all element edges on sphere edge of the same length. *)
	cuboidMesh=With[
		{pts=N@Tan@Subdivide[0,Pi/4,n]},
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
]


BallMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 1.";
BallMesh::method="Method \"`1`\" is not supported.";
BallMesh//Options={Method->Automatic,"BasicUnit"->False};

BallMesh[n_,opts:OptionsPattern[]]:=BallMesh[{0,0,0},1,n,opts]

BallMesh[{x_,y_,z_},r_,n_,opts:OptionsPattern[]]:=Module[
	{order,method,mesh,rotations},
	If[Not@(TrueQ[n>=1]&&IntegerQ[n]),Message[BallMesh::noelems,n];Return[$Failed]];
	
	method=OptionValue[Method]/.Automatic->"Block";
		
	If[
		Not@MemberQ[{"Block","Projection"},method],
		Message[BallMesh::method,method];Return[$Failed]
	];
	(* Mesh is created only in the first octant and returned according to given Option. *)
	mesh=Switch[method,
		"Block",ballMeshBlock[{x,y,z},r,n],
		"Projection",ballMeshProjection[{x,y,z},r,n]
	];
	
	If[TrueQ@OptionValue["BasicUnit"],Return[mesh]];
	
	(* 8 rotations, 4 of them are double (composition). *)
	rotations=Flatten@Map[
		{#,#@*RotationTransform[Pi/2,{0,0,1}]}&,
		(RotationTransform[#,{1,0,0}]&/@({0,1,2,3}*Pi/2))
	];
	MergeMesh[TransformMesh[mesh,#]&/@rotations]
]


(* ::Subsubsection::Closed:: *)
(*SphericalVoidMesh*)


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

	rt=RotationTransform[#*(2Pi/3),{1,1,1},{0,0,0}]&/@{0,1,2};
	MergeMesh[TransformMesh[basicMesh,#]&/@rt]
]


(* ::Subsubsection::Closed:: *)
(*EllipsoidVoidMesh*)


(* ::Text:: *)
(*This is a quick prototype, made for analysis of voids inside material (forging).*)


(* 
Projection (with RegionNearest) of points from one side of the cube to a sphere. 
Argument f should be a Function to specify on which side of cube points are created (e.g. {1,#1,#2}& ). 
It is assumed size of domain is 1.
*)
Clear[voidRaster]
voidRaster[f_Function,semiAxis:{_,_,_}]:=Module[
	{n=30,pts,sidePts,innerPts,middlePts},
	pts=N@Subdivide[0,1,n-1];
	sidePts=Flatten[Outer[f,pts,pts],1];
	innerPts=RegionNearest[Ellipsoid[{0,0,0},semiAxis],sidePts];
	middlePts=RegionNearest[Ellipsoid[{0,0,0},semiAxis*2],sidePts];
	{Partition[sidePts,n],Partition[middlePts,n],Partition[innerPts,n]}
]


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
]

(* Implementation for spherical void is faster, because (costly) StructuredMesh is generated
only once and then rotated. *)
EllipsoidVoidMesh[voidRadius_,noElements_Integer]:=Module[{
	dim=Clip[voidRadius,{0.01,0.8}]*{1,1,1},
	n=Round@Clip[noElements,{1,100}],
	rotations=RotationTransform[#,{1,1,1},{0,0,0}]&/@{0,2Pi/3,4Pi/3},
	basicMesh
	},
	basicMesh=StructuredMesh[voidRaster[{1,#1,#2}&,dim],n{1,1,2}];
	Fold[
		MergeMesh[#1,#2]&,
		TransformMesh[basicMesh,#]&/@rotations
	]
]


(* ::Subsubsection::Closed:: *)
(*TetrahedronMesh*)


(* From documentation page on Tetrahedron *)
symmetricSubdivision[Tetrahedron[pl_],k_]/;0<=k<2^Length[pl]:=Module[
	{n=Length[pl]-1,i0,bl,pos},
	
	i0=DigitCount[k,2,1]; 
	bl=IntegerDigits[k,2,n];
	pos=FoldList[If[#2==0,#1+{0,1},#1+{1,0}]&,{0,i0},Reverse[bl]];
	Tetrahedron@Map[Mean,Extract[pl,#]&/@Map[{#}&,pos+1,{2}]] 
]


(* From documentation page on Tetrahedron *)
nestedSymmetricSubdivision[Tetrahedron[pl_],level_Integer]/;level==0:=Tetrahedron[pl]

nestedSymmetricSubdivision[Tetrahedron[pl_],level_Integer]/;level>0:=Flatten[
	nestedSymmetricSubdivision[symmetricSubdivision[Tetrahedron[pl],#],level-1]&/@Range[0,7]
]


(* Helper function to check the orientation of nodes used in TetrahedronElement *)
reorientQ[{a_,b_,c_,d_}]:=Positive@Det[{a-d,b-d,c-d}]


TetrahedronMesh::noelms="Currently only 2, 4, 8, 16 or 32 elements per edge are allowed.";

(* TODO: It would be really nice if tetrahedron could be split to arbitrary 
number of elements per edge.*)
TetrahedronMesh[pts_,n_Integer]:=Module[
	{divisions,f,allCrds,nodes,connectivity},
	
	If[Not@MemberQ[{2,4,8,16,32},n],Message[TetrahedronMesh::noelms];Return[$Failed]];
	
	divisions=Log[2,n];
	f=If[reorientQ[#],#[[{1,2,4,3}]],#]&;
	allCrds=f/@(Join@@List@@@nestedSymmetricSubdivision[Tetrahedron[pts],divisions]);
	nodes=DeleteDuplicates@Flatten[allCrds,1];
	connectivity=With[
		{rules=Thread[nodes->Range@Length[nodes]]},
		Replace[allCrds,rules,{2}]
	];
	
	ToElementMesh[
		"Coordinates"->nodes,
		"MeshElements"->{TetrahedronElement[connectivity]}
	]
]


(* ::Subsubsection::Closed:: *)
(*RodriguesSpaceMesh*)


RodriguesSpaceMesh::noelms="Currently only 2, 4, 8 or 16 elements per edge are allowed.";
RodriguesSpaceMesh[n_Integer]:=Module[
	{a,b,divisions,sideBasicMesh,basicRotations,sideMesh,sideRotations,allSidesMesh,cornerMesh,cornerRotations,allCornersMesh},
	a=N@Tan[Pi/8];
	b=N@(1-2a);
	
	If[Not@MemberQ[{2,4,8,16},n],Message[RodriguesSpaceMesh::noelms];Return[$Failed]];
	divisions=Log[2,n];
	sideBasicMesh=TetrahedronMesh[{{a,0,0},{a,b,a},{a,-b,a},{0,0,0}},divisions];
	
	basicRotations=N@RotationTransform[#,{1,0,0}]&/@(Most@Subdivide[0,2Pi,8]);
	sideMesh=Fold[
		MergeMesh[#1,#2]&,
		TransformMesh[sideBasicMesh,#]&/@basicRotations
	];
	
	sideRotations=N@Join[
		RotationTransform[#,{0,0,1}]&/@{0,Pi/2,Pi,3Pi/2},
		RotationTransform[#,{0,1,0}]&/@{Pi/2,3Pi/2}
	];
	allSidesMesh=Fold[
		MergeMesh[#1,#2]&,
		TransformMesh[sideMesh,#]&/@sideRotations
	];
	
	cornerMesh=TetrahedronMesh[{{a,a,b},{b,a,a},{a,b,a},{0,0,0}},divisions];
	cornerRotations=N@Join[
		RotationTransform[#,{0,0,1}]&/@{0,Pi/2,Pi,3Pi/2},
		(RotationTransform[#,{0,0,1}]@*RotationTransform[Pi,{1,1,0}])&/@{0,Pi/2,Pi,3Pi/2}
	];
	allCornersMesh=Fold[
		MergeMesh[#1,#2]&,
		TransformMesh[cornerMesh,#]&/@cornerRotations
	];
	
	MergeMesh[allSidesMesh,allCornersMesh]
]


(* ::Section::Closed:: *)
(*End package*)


End[]; (* "`Private`" *)


EndPackage[];
