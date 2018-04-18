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


MergeMesh::usage="MergeMesh[mesh1, mesh2] merges two ElementMesh objects.";
TransformMesh::usage="TransformMesh[mesh, tfun] transforms ElementMesh mesh according to TransformationFunction tfun";
ExtrudeMesh::usage="ExtrudeMesh[mesh, thickness, layers] extrudes 2D quadrilateral mesh to 3D hexahedron mesh.";
SmoothenMesh::usage"SmoothenMesh[mesh] improves the quality of 2D mesh.";

MeshElementMeasure::usage="MeshElementMeasure[mesh_ElementMesh] gives the measure of each mesh element.";
BoundaryElementMeasure::usage="BoundaryElementMeasure[mesh_ElementMesh] gives the measure of each boundary element.";

RectangleMesh::usage="RectangleMesh[{x1,y1},{x2,y2},{nx,ny}] creates structured mesh on Rectangle.";
CuboidMesh::usage="CuboidMesh[{x1,y1,z1},{x2,y2,z2},{nx,ny,nz}] creates structured mesh of hexahedra on Cuboid.";
DiskMesh::usage="DiskMesh[n] created structured mesh on Disk.";
SphereMesh::usage="SphereMesh[n] creates structured mesh of sphere.";
StructuredMesh::usage="StructuredMesh[raster,{nx,ny}] creates structured mesh of quadrilaterals.
StructuredMesh[raster,{nx,ny,nz}] creates structured mesh of hexahedra.";

EllipsoidVoidMesh::usage="EllipsoidVoidMesh[radius, noElements] creates a mesh with spherical void.
EllipsoidVoidMesh[{r1,r2,r3}, noElements] creates a mesh with ellipsoid void with semi-axis radii r1, r2 and r3.";


(* ::Section::Closed:: *)
(*Code*)


(* Begin private context *)
Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Mesh operations*)


(* ::Subsubsection::Closed:: *)
(*TransformMesh*)


reflectionQ[tfun_TransformationFunction]:=Negative@Det[TransformationMatrix[tfun]]


reorderElements[elements_,1,2]:=Map[Reverse,elements,{3}]
reorderElements[elements_,2,2]:=Map[Join@@(Reverse/@TakeDrop[#,Length[#]/2])&,elements,{3}]
reorderElements[elements_,order_,dim_]:=elements


(* This function doesn't work for ReflectionTransform because it breaks the node order in elements. *)
TransformMesh[mesh_ElementMesh,tfun_TransformationFunction]:=Module[{
	elements=mesh["MeshElements"],transformedElements
	},
	transformedElements=If[
		reflectionQ[tfun],
		reorderElements[elements,mesh["MeshOrder"],mesh["EmbeddingDimension"]],
		elements
	];
	
	ToElementMesh[
		"Coordinates"->tfun/@mesh["Coordinates"],
		"MeshElements"->transformedElements
	]
]


(* ::Subsubsection::Closed:: *)
(*MergeMesh*)


(* Code is adjusted after this source: 
https://mathematica.stackexchange.com/questions/156445/automatically-generating-boundary-meshes-for-region-intersections 
*)
MergeMesh[mesh1_,mesh2_]:=Module[
	{c1,c2,nc1,newCrds,newElements,eleType,inci1,inci2},
	c1=mesh1["Coordinates"];
	c2=mesh2["Coordinates"];
	nc1=Length[c1];
	newCrds=Join[c1,c2];
	eleType=Join[Head/@mesh1["MeshElements"],Head/@mesh2["MeshElements"]];
	inci1=ElementIncidents[mesh1["MeshElements"]];
	inci2=ElementIncidents[mesh2["MeshElements"]]+nc1;
	(* If all elements are of the same type, then this type is specified only once. *)
	newElements=If[
		SameQ@@eleType,
		{First[eleType][Join[Join@@inci1,Join@@inci2]]},
		MapThread[#1[#2]&,{eleType,Join[inci1,inci2]}]
	];
	ToElementMesh[
		"Coordinates"->newCrds,
		"MeshElements"->newElements,
		"DeleteDuplicateCoordinates"->True (* already a default option *)
	]
]


(* ::Subsubsection::Closed:: *)
(*ExtrudeMesh*)


(* Basics of this function are taken from AceFEM help. *)
ExtrudeMesh::badType="Only first order 2D quadrilateral mesh is supported.";
ExtrudeMesh[mesh_ElementMesh,thickness_/;thickness>0,layers_Integer?Positive]:=Module[{
	fi=0,stretch=0,rot,n2D,nodes2D,nodes3D,elements2D,elements3D
	},
	If[
		Or[mesh["MeshOrder"]=!=1,(Head/@mesh["MeshElements"])=!={QuadElement},mesh["EmbeddingDimension"]=!=2],
		Message[ExtrudeMesh::badType];Return[$Failed]
	];
	
	
	nodes2D=mesh["Coordinates"];
	elements2D=Join@@ElementIncidents[mesh["MeshElements"]];
	n2D=Length@nodes2D;
	nodes3D=With[{
		dz=thickness/layers,dfi=fi/layers,dstretch=stretch/layers
		},
		Flatten[
			Table[
				rot=RotationTransform[(l-1)dfi];
				Map[Join[(1+(l-1) dstretch) rot[#],{(l-1)dz}]&,nodes2D],
				{l,layers+1}
			],
		1]
	];
	
	elements3D=Flatten[
		Table[
			Map[Join[n2D (l-1)+#,n2D l+#]&,elements2D],
			{l,layers}
		],
	1];
		
	ToElementMesh[
		"Coordinates"->nodes3D,
		"MeshElements"->{HexahedronElement[elements3D]}
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


(*
This function returns the surface of boundary elements in 3D embedding and length of 
boundary elements in 2D embedding.
*)

BoundaryElementMeasure[mesh_ElementMesh]:=Module[
	{},
	Print["Placeholder"];
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


StructuredMesh::array="Raster of input points must be full array of numbers with depth of `1`.";

StructuredMesh//Options={InterpolationOrder->1};
StructuredMesh[raster_,{nx_,ny_},opts:OptionsPattern[]]:=Module[
    {order,dim,restructured,xInt,yInt,zInt,nodes,connectivity},
    If[Not@ArrayQ[raster,3,NumericQ],Message[StructuredMesh::array,3];Return[$Failed]];

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
    If[Not@ArrayQ[raster,4,NumericQ],Message[StructuredMesh::array,4];Return[$Failed]];

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
(*Shape meshes*)


(* ::Subsubsection::Closed:: *)
(*RectangleMesh*)


RectangleMesh[{x1_,y1_},{x2_,y2_},{nx_,ny_}]:=StructuredMesh[{
	{{x1,y1},{x2,y1}},{{x1,y2},{x2,y2}}},
	{nx,ny}
];


(* ::Subsubsection::Closed:: *)
(*CuboidMesh*)


CuboidMesh[{x1_,y1_,z1_},{x2_,y2_,z2_},{nx_,ny_,nz_}]:=StructuredMesh[{
	{{{x1,y1,z1},{x2,y1,z1}},{{x1,y2,z1},{x2,y2,z1}}},
	{{{x1,y1,z2},{x2,y1,z2}},{{x1,y2,z2},{x2,y2,z2}}}
	},
	{nx,ny,nz}
];


(* ::Subsubsection::Closed:: *)
(*DiskMesh*)


diskMeshProjection[n_Integer/;(n>=2),order_Integer]:=Module[
	{squareMesh,d=1},
	squareMesh=StructuredMesh[
		{{{-d,-d},{d,-d}},{{-d,d},{d,d}}}/2,
		{n,n}
	];
	
	squareMesh=MeshOrderAlteration[squareMesh,order];
	
	ToElementMesh[
		"Coordinates" -> rescale/@squareMesh["Coordinates"],
		"MeshElements" -> squareMesh["MeshElements"]
	]
]


diskMeshBlock[n_Integer/;(n>=2)]:=Module[
	{squareMesh,sideMesh,d,r,raster,rotations},
	r=1;
	d=0.33*r;
	
	squareMesh=StructuredMesh[
		{{{-d,-d},{d,-d}},{{-d,d},{d,d}}},
		{n,n}
	];
	
	raster={
		Thread[{Subdivide[-d,d,90],d}],
		N@Table[{r*Cos[fi],r*Sin[fi]},{fi,3Pi/4,Pi/4,-Pi/180}]
	};
	sideMesh=StructuredMesh[raster,{n,n}];
	rotations=RotationTransform/@(Range[4]*Pi/2);
	
	Fold[
		MergeMesh[#1,#2]&,
		squareMesh,
		TransformMesh[sideMesh,#]&/@rotations
	]
]


DiskMesh::method="Method \"`1`\" is not supported.";
DiskMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 2.";

DiskMesh//Options={"MeshOrder"->Automatic,Method->Automatic};

DiskMesh[n_,opts:OptionsPattern[]]/;If[TrueQ[n>=2&&IntegerQ[n]],True,Message[DiskMesh::noelems,n];False]:=Module[
	{squareMesh,order,method},
	order=OptionValue["MeshOrder"]/.(Except[1|2]->1);
	method=OptionValue[Method]/.Automatic->"Block";
	
	Switch[method,
		"Block",diskMeshBlock[n],
		"Projection",diskMeshProjection[n,order],
		_,Message[DiskMesh::method,method];$Failed
	]
]


(* ::Subsubsection::Closed:: *)
(*Sphere mesh*)


rescale[v_] := Max[Abs@v]*Normalize[v]


(* Source of this code is the answer from "Michael E2" on topic: 
https://mathematica.stackexchange.com/questions/85592/how-to-create-an-elementmesh-of-a-sphere
*)

SphereMesh//Options={"MeshOrder"->Automatic};
SphereMesh[n_Integer/;(n>=2),opts:OptionsPattern[]]:=Module[
	{cuboidMesh,order,d=2},
	order=OptionValue["MeshOrder"]/.(Except[1|2]->2);
	cuboidMesh=CuboidMesh[
		-{d,d,d}/2,
		{d,d,d}/2,
		{n,n,n}
	];
	
	
	cuboidMesh=MeshOrderAlteration[cuboidMesh,order];
	
	ToElementMesh[
		"Coordinates" -> rescale /@ cuboidMesh["Coordinates"],
		"MeshElements" -> cuboidMesh["MeshElements"]
	]
]


(* ::Subsubsection::Closed:: *)
(*Hollow cube mesh*)


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


(* ::Section::Closed:: *)
(*End package*)


End[]; (* "`Private`" *)


EndPackage[];
