#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Text:: *)
(*This script builds the MeshTools-X.Y.Z.paclet. The creation is two step process. First the source code and other files need to be copied alongside the documentation and then the paclet file is created. Previously documentation should be generated separately with Workbench as outlined in the README.md file.*)


(* ::Subsection:: *)
(*Git functions*)


getGitRevision[dir_]:=Module[{description},
	SetDirectory[dir];
	(* For meaning of this see: https://git-scm.com/docs/git-describe *)
	description=First[
		ReadList["!git describe --long --tags --dirty --always",String],
		Return[$Failed]
	];
	ResetDirectory[];
	description
];


getGitCommitCount[dir_]:=Module[{value},
	SetDirectory[dir];
	(* For meaning of this see: https://git-scm.com/docs/git-describe *)
	value=First[
		ReadList["!git rev-list --count HEAD",Number],
		Return[$Failed]
	];
	ResetDirectory[];
	value
];


(* ::Subsection:: *)
(*Build procedure*)


(* ::Subsubsection:: *)
(*Initialization*)


$name="MeshTools";
(* Get the distribution directory by using this notebook directory. *)
$root=If[$Notebooks,NotebookDirectory[],Directory[]];
(* Directory of main source code. *)
$source=FileNameJoin[{$root,$name}];
(* Construct the target directory from this notebooks base directory. *)
$target=FileNameJoin[{$root,"build",$name}];


$helpMessage=("
  This script packs package source code to .paclet file.
  Script options:
    --help                       Output usage information
    --release                    Build public release
    --install                    Install just created paclet
");


Print[$helpMessage];

If[$VersionNumber<10.0, Print["Mathematica 10.0 or later required."];Quit[1]];


(* This should support text based interface with "wolframscript" mechanism and 
"wolfram -script" mechanism. See tutorial/WolframLanguageScripts for more info. *)
$args=Join[$ScriptCommandLine,$CommandLine];


(* Verify that we have git *)
With[
	{rg=RunProcess[{"git","--version"}]},
	If[
		rg===$Failed||rg["ExitCode"]!=0,
		Print["git software is not available."];Quit[1]
	]
];


If[
	Not@DirectoryQ[$target],
	CreateDirectory[$target,CreateIntermediateDirectories->True]
];


(* ::Subsubsection:: *)
(*Copy files*)


Module[
	{docDir,docFiles,lastModTime},
	(* Copy package files from the source to the target directory. *)
	Map[
		CopyFile[
			FileNameJoin[{$source,#}],
			FileNameJoin[{$target,#}],
			OverwriteTarget->True
		]&,
		{"MeshTools.wl","PacletInfo.m"}
	];

	(* Delete old and copy the new source directories. *)
	Map[
		(
		If[
			FileExistsQ[FileNameJoin[{$target,#}]],
			DeleteDirectory[FileNameJoin[{$target,#}],DeleteContents->True]
		];
		CopyDirectory[FileNameJoin[{$source,#}],FileNameJoin[{$target,#}]]
		)&,
		{"FrontEnd","Kernel"}
	];

	(* Documentation should be built with a separate procedure before. *)
	docDir=FileNameJoin[{$target,"Documentation"}];
	docFiles=FileNames["*.nb",docDir,Infinity];
	If[
		docFiles=!={},
		lastModTime=Min[FileDate[#,"Modification"]&/@docFiles];
		(* Check if all notebooks have been created at least one hour ago. *)
		If[
			(AbsoluteTime@Now-AbsoluteTime@lastModTime)/3600 > 1.,
			Print["  Warning! Documentation notebooks may be outdated."];
			Print["  Last modification date is: ",DateString@lastModTime]
		],
		Print["  Warning! Documentation notebooks are missing in build folder."]
	];
];


Module[
	{original,modified,noCommits,revision,publicQ,version},

	original=List@@Import[FileNameJoin[{$source,"PacletInfo.m"}]];
	noCommits=getGitCommitCount[$root];
	revision=getGitRevision[$root];
	publicQ=MemberQ[$args,"--release"];

	(* Description of git repository (git describe ...) returns a string that cannot 
	be used as BuildNumber because only integers are accepted. 
	Otherwise PackPaclet returns $Failed and procedure fails. Instead commit count is used. 
	Version cannot be appended with build number, because otherwise we break procedure
	of loading development version with PacletDirectoryAdd. *)
	modified=Paclet@@Normal[Merge[{
		Association@@original,
		<|Internal->Not@publicQ,BuildNumber->noCommits,"Hash"->revision|>
		},
		Last
	]];

	If[
		StringContainsQ[revision,"dirty"],
		Print["  Warning! Repository contains uncommited changes."]
	];
	Export[
		FileNameJoin[{$target,"PacletInfo.m"}],
		modified,
		"Comments"->{"Paclet Info File","Created on "<>DateString[]}
	];
];


(* ::Subsubsection:: *)
(*Create .paclet file*)


Module[
	{path,newPaclet,buildNo,successQ},

	buildNo=getGitCommitCount[$root];
	Print["  Build number: ",buildNo];

	Needs["PacletManager`"];
	(* Change into the build directory and create the paclet. *)	
	SetDirectory@FileNameJoin[{$root,"build"}];
	path=PackPaclet[$name];
	ResetDirectory[];
	(* We check if the paclet was created less than 1 second ago. *)
	successQ=TrueQ[(AbsoluteTime@Now-AbsoluteTime@FileDate@path)<1.];

	If[
		successQ,
		Print["  ",FileNameTake[path]," built succesfully."],
		Print["  Paclet build failed!"];Quit[1]
	];

	If[
		MemberQ[$args,"--install"],
		(* Uninstall older versions first. *)
		If[PacletFind[$name]=!={},PacletUninstall[$name]];
		newPaclet=PacletInstall[path];
		RebuildPacletData[];
		Print["  Paclet is installed to $UserBasePacletsDirectory."];
	];

	If[successQ,Quit[0],Quit[1]]
]
