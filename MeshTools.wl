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

RectangleMesh::usage="RectangleMesh[{x1,y1},{x2,y2},{nx,ny}] creates structured mesh on Rectangle.";
CuboidMesh::usage="CuboidMesh[{x1,y1,z1},{x2,y2,z2},{nx,ny,nz}] creates structured mesh of hexahedra on Cuboid.";
DiskMesh::usage="DiskMesh[n] created structured mesh on Disk.";
SphereMesh::usage="SphereMesh[n] creates structured mesh of sphere.";
StructuredMesh::usage="StructuredMesh[raster,{nx,ny}] creates structured mesh of quadrilaterals.
StructuredMesh[raster,{nx,ny,nz}] creates structured mesh of hexahedra.";


(* ::Section::Closed:: *)
(*Code*)


(* Begin private context *)
Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Mesh operations*)


(* ::Subsubsection::Closed:: *)
(*TransformMesh*)


(* This function doesn't work for ReflectionTransform because it breaks the node order in elements. *)
TransformMesh[mesh_ElementMesh,tfun_TransformationFunction]:=ToElementMesh[
	"Coordinates"->tfun/@mesh["Coordinates"],
	"MeshElements"->mesh["MeshElements"]
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


(* ::Section::Closed:: *)
(*End package*)


End[]; (* "`Private`" *)


EndPackage[];
