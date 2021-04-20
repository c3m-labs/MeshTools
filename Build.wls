#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Build script*)


(* ::Text:: *)
(*This script builds the (installation) paclet file from source code.*)


(* ::Subsection:: *)
(*Git functions*)


(* Get human readable description of current state of repository.
For more info see: https://git-scm.com/docs/git-describe *)
getGitRevision[dir_]:=Module[
	{description},
	SetDirectory[dir];
	description=First[
		ReadList["!git describe --long --tags --dirty --always",String],
		Return[$Failed]
	];
	ResetDirectory[];
	description
];


(* Get repository commit count. It is useful to mark "build number". *)
getGitCommitCount[dir_]:=Module[
	{value},
	SetDirectory[dir];
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

(* This should support text based interface with "wolframscript" mechanism and 
"wolfram -script" mechanism. See tutorial/WolframLanguageScripts for more info. *)
$args=Join[$ScriptCommandLine,$CommandLine];


$helpMessage=("
  This script converts package source code to installation .paclet file.
  Script options:
    --install                    Install just created paclet
");


(* Help message is printed every time. *)
Print[$helpMessage];

If[$VersionNumber<10.0, Print["Mathematica 10.0 or later required."];Quit[1]];


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

	(* Delete old and copy the new source directories. Documentation is build separately. *)
	Map[
		(If[
			FileExistsQ[FileNameJoin[{$target,#}]],
			DeleteDirectory[FileNameJoin[{$target,#}],DeleteContents->True]
		];
		CopyDirectory[
				FileNameJoin[{$source,#}],
				FileNameJoin[{$target,#}]
		])&,
		{"FrontEnd","Kernel","LibraryResources"}
	];

	(* Documentation should be built with a separate procedure before. *)
	docDir=FileNameJoin[{$target,"Documentation"}];
	docFiles=FileNames["*.nb",docDir,Infinity];
	If[
		docFiles=!={},
		lastModTime=Min[FileDate[#,"Modification"]&/@docFiles];
		(* Check if all notebooks have been created at least one hour ago. *)
		If[
			(AbsoluteTime@Now-AbsoluteTime@lastModTime)/3600. > 1.,
			Print["  Warning! Documentation notebooks may be outdated. Earliest change ",DateString@lastModTime]
		],
		Print["  Warning! Documentation directory is missing in build folder."]
	];
];


Module[
	{original,modified,noCommits,revision},

	original=List@@Import[FileNameJoin[{$source,"PacletInfo.m"}]];
	revision=getGitRevision[$root];

	(* Number of commits cannot be appended to version number because otherwise loading
	from development directory with PacletDirectoryAdd procedure is broken. *)
	modified=Paclet@@Normal[
		Merge[{Association@@original,<|"UUID"->revision|>},Last]
	];

	If[
		StringContainsQ[revision,"dirty"],
		Print["  Warning! Repository contains uncommited changes."]
	];
	(* Overwrite "PacletInfo.m" with updated contents. *)
	Export[
		FileNameJoin[{$target,"PacletInfo.m"}],
		modified,
		"Comments"->{"Paclet Info File","Created on "<>DateString[]}
	];
];


(* ::Subsubsection:: *)
(*Create .paclet file*)


Module[
	{path,buildNo,successQ},

	buildNo=getGitCommitCount[$root];
	Print["  Build number: ",buildNo];

	(* Change into the build directory and create the paclet. *)	
	SetDirectory@FileNameJoin[{$root,"build"}];
	path=With[
		{createPaclet = If[$VersionNumber >= 12.1, CreatePacletArchive, PackPaclet]},
		createPaclet@$name
	];
	ResetDirectory[];
	successQ=TrueQ[(AbsoluteTime@Now-AbsoluteTime@FileDate@path)<1.];

	If[
		successQ&&MemberQ[$args,"--install"],
		(* Uninstall older versions first. *)
		Quiet@PacletUninstall[$name];
		(* Option ForceVersionInstall appears in 12.1 *)
		If[
			$VersionNumber >= 12.1,
			PacletInstall[path,ForceVersionInstall->True];PacletDataRebuild[],
			PacletInstall[path];RebuildPacletData[]
		];
	];

	If[
		successQ,
		Print["  ",FileNameTake[path]," built succesfully."];Quit[0],
		Print["  Paclet build failed!"];Quit[1]
	]
]
