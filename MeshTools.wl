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
MergeMesh::usage="MergeMesh[list] merges a list of ElementMesh objects with the same embedding dimension.";
TransformMesh::usage="TransformMesh[mesh, tfun] transforms ElementMesh mesh according to TransformationFunction tfun";
ExtrudeMesh::usage="ExtrudeMesh[mesh, thickness, layers] extrudes 2D quadrilateral mesh to 3D hexahedron mesh.";
SmoothenMesh::usage"SmoothenMesh[mesh] improves the quality of 2D mesh.";

MeshElementMeasure::usage="MeshElementMeasure[mesh_ElementMesh] gives the measure of each mesh element.";
BoundaryElementMeasure::usage="BoundaryElementMeasure[mesh_ElementMesh] gives the measure of each boundary element.";

RectangleMesh::usage="RectangleMesh[{x1,y1},{x2,y2},{nx,ny}] creates structured mesh on Rectangle.";
CuboidMesh::usage="CuboidMesh[{x1,y1,z1},{x2,y2,z2},{nx,ny,nz}] creates structured mesh of hexahedra on Cuboid.";

DiskMesh::usage="DiskMesh[{x,y},r,n] creates structured mesh with n elements on Disk of radius r centered at {x,y}.";
SphereMesh::usage="SphereMesh[{x,y,z}, r, n] creates structured mesh with n elements on Sphere of radius r centered at {x,y,z}.";

StructuredMesh::usage="StructuredMesh[raster,{nx,ny}] creates structured mesh of quadrilaterals.
StructuredMesh[raster,{nx,ny,nz}] creates structured mesh of hexahedra.";

EllipsoidVoidMesh::usage="EllipsoidVoidMesh[radius, noElements] creates a mesh with spherical void.
EllipsoidVoidMesh[{r1,r2,r3}, noElements] creates a mesh with ellipsoid void with semi-axis radii r1, r2 and r3.";
RodriguesSpaceMesh::usage="RodriguesSpaceMesh[n] creates mesh for Rodrigues space used in metal texture analysis.";


(* ::Section:: *)
(*Code*)


(* Begin private context *)
Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Mesh operations*)


(* ::Subsubsection::Closed:: *)
(*AddMeshMarkers*)


AddMeshMarkers[mesh_ElementMesh,marker_Integer]:=Module[{
	meshType,head,elementTypes,elementIncidents,elementMarkers
	},
	
	{meshType,head}=If[
		mesh["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	elementTypes=Head/@mesh[meshType];
	elementIncidents=ElementIncidents@mesh[meshType];
	elementMarkers=ConstantArray[marker,#]&/@(Length/@elementIncidents);
	
	head[
		"Coordinates"->mesh["Coordinates"],
		meshType->MapThread[#1[#2,#3]&,{elementTypes,elementIncidents,elementMarkers}]
	]
]


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


(* 
Code is adjusted after this source: 
https://mathematica.stackexchange.com/questions/156445/automatically-generating-boundary-meshes-for-region-intersections 
*)
MergeMesh::order="Meshes must have the same \"MeshOrder\".";
MergeMesh::dim="Meshes must have the same \"EmbeddingDimension\".";

MergeMesh[list_List/;Length[list]>=2]:=Fold[MergeMesh,list]

MergeMesh[mesh1_,mesh2_]:=Module[
	{meshType,head,c1,c2,newCrds,newElements,elementTypes,elementMarkers,inci1,inci2},
	
	If[mesh1["MeshOrder"]=!=mesh2["MeshOrder"],Message[MergeMesh::order];Return[$Failed]];
	If[mesh1["EmbeddingDimension"]=!=mesh2["EmbeddingDimension"],Message[MergeMesh::dim];Return[$Failed]];
		
	{meshType,head}=If[
		mesh1["MeshElements"]===Automatic,
		{"BoundaryElements",ToBoundaryMesh},
		{"MeshElements",ToElementMesh}
	];
	
	c1=mesh1["Coordinates"];
	c2=mesh2["Coordinates"];
	newCrds=Join[c1,c2];
	elementTypes=Join[Head/@mesh1[meshType],Head/@mesh2[meshType]];
	elementMarkers=ElementMarkers/@{mesh1[meshType],mesh2[meshType]};
	
	inci1=ElementIncidents[mesh1[meshType]];
	inci2=ElementIncidents[mesh2[meshType]]+Length[c1];
	(* If all elements are of the same type, then this type is specified only once. *)
	newElements=If[
		SameQ@@elementTypes,
		{First[elementTypes][Join[Join@@inci1,Join@@inci2],Flatten[elementMarkers]]},
		MapThread[#1[#2,#3]&,{elementTypes,Join[inci1,inci2],Join@@elementMarkers}]
	];
	
	head[
		"Coordinates"->newCrds,
		meshType->newElements,
		"DeleteDuplicateCoordinates"->True (* already a default option *)
	]
]


(* ::Subsubsection::Closed:: *)
(*ExtrudeMesh*)


(* Basics of this function are taken from AceFEM help. *)
ExtrudeMesh::badType="Only first order 2D quadrilateral mesh is supported.";
ExtrudeMesh[mesh_ElementMesh,thickness_/;thickness>0,layers_Integer?Positive]:=Module[{
	fi=0,stretch=0,rot,n2D,nodes2D,nodes3D,elements2D,elements3D,markers2D,markers3D
	},
	If[
		Or[mesh["MeshOrder"]=!=1,(Head/@mesh["MeshElements"])=!={QuadElement},mesh["EmbeddingDimension"]=!=2],
		Message[ExtrudeMesh::badType];Return[$Failed]
	];
		
	nodes2D=mesh["Coordinates"];
	elements2D=Join@@ElementIncidents[mesh["MeshElements"]];
	markers2D=Join@@ElementMarkers[mesh["MeshElements"]];
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
			Map[Join[n2D*(l-1)+#,n2D*l+#]&,elements2D],
			{l,layers}
		],
	1];
	
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


(* ::Subsection:: *)
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


(* ::Subsubsection:: *)
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
	(* Size of internal square. *)
	(* TODO: Find the best value for this size. *)
	d=0.33*r;
	square=squareMesh[{x,y},d,n];
	
	raster={
		Thread[{x+Subdivide[-d,d,90],y+d}],
		N@Table[{x+r*Cos[fi],y+r*Sin[fi]},{fi,3Pi/4,Pi/4,-Pi/180}]
	};
	sideMesh=StructuredMesh[raster,{n,n}];
	rotations=RotationTransform[#,{x,y}]&/@(Range[4]*Pi/2);
	
	Fold[
		MergeMesh[#1,#2]&,
		square,
		TransformMesh[sideMesh,#]&/@rotations
	]
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


(* ::Subsubsection:: *)
(*SphereMesh*)


sphereMeshBlock[{x_,y_,z_},r_,n_Integer/;(n>=2)]:=Module[
	{rescale,bottomRaster,topRaster,cubeMesh,sideMesh,d,rotations,unitCube},
	(* Size of internal square. *)

	d=0.33;
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
SphereMesh::noelems="Specificaton of elements `1` must be an integer equal or larger than 2.";
SphereMesh//Options={"MeshOrder"->Automatic};

SphereMesh[n_,opts:OptionsPattern[]]:=SphereMesh[{0,0,0},1,n,opts]

SphereMesh[{x_,y_,z_},r_,n_,opts:OptionsPattern[]]:=Module[
	{rescale,cuboidMesh,order,coordinates},
	If[TrueQ[n<2]&&Not@IntegerQ[n],Message[SphereMesh::noelems,n];$Failed];
	
	rescale=(Max[Abs@#]*Normalize[#])&;
	order=OptionValue["MeshOrder"]/.(Except[1|2]->2);
		
	(* This special raster makes all element edges on sphere edge of the same length. *)
	cuboidMesh=With[
		{pts=r*N@Tan@Subdivide[-Pi/4,Pi/4,n]},
		StructuredMesh[Outer[Reverse@*List,pts,pts,pts],{n,n,n}]
	];
	(* If we do order alteration (more than 1st order) before projection, then geometry is
	more accurate and elements have curved edges. *)
	cuboidMesh=MeshOrderAlteration[cuboidMesh,order];
	
	coordinates=Transpose[Transpose[rescale/@cuboidMesh["Coordinates"]]+{x,y,z}];
	
	ToElementMesh[
		"Coordinates" -> coordinates,
		"MeshElements" -> cuboidMesh["MeshElements"]
	]
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
(*RodriguesSpaceMesh*)


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


(* TODO: It would be really nice if tetrahedron could be split to arbitrary 
number of elements per edge.*)
tetrahedronSubMesh[pts_,n_Integer]:=Module[
	{f,allCrds,nodes,connectivity},
	
	f=If[reorientQ[#],#[[{1,2,4,3}]],#]&;
	allCrds=f/@(Join@@List@@@nestedSymmetricSubdivision[Tetrahedron[pts],n]);
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


RodriguesSpaceMesh::noelms="Currently only 2, 4, 8 or 16 elements per edge are allowed.";
RodriguesSpaceMesh[n_Integer]:=Module[
	{a,b,divisions,sideBasicMesh,basicRotations,sideMesh,sideRotations,allSidesMesh,cornerMesh,cornerRotations,allCornersMesh},
	a=N@Tan[Pi/8];
	b=N@(1-2a);
	
	If[Not@MemberQ[{2,4,8,16},n],Message[RodriguesSpaceMesh::noelms];Return[$Failed]];
	divisions=Log[2,n];
	sideBasicMesh=tetrahedronSubMesh[{{a,0,0},{a,b,a},{a,-b,a},{0,0,0}},divisions];
	
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
	
	cornerMesh=tetrahedronSubMesh[{{a,a,b},{b,a,a},{a,b,a},{0,0,0}},divisions];
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
