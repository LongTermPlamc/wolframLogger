BeginPackage["Logger`"]

(* Declare your package's public symbols here. *)

Logger

Begin["`Private`"]

(* Define your public and private symbols here. *)

Logger[basePath_String : "logs/", maxSize_ : 1024*1024, mode_:"Single"] :=
 Module[
  {
    logLevels = {"Info", "Error", "Warning", "Debug"}, 
    logFiles, 
    logIndex = Association[], rotateFile, writeLog
    },
  (*Ensure the log directory exists*)
  If[
    !DirectoryQ[basePath],
    CreateDirectory[basePath, CreateIntermediateDirectories -> True]
    ];
  (*Initialize log file paths*)
  logFiles = If[MatchQ[mode,"Single"],
    
    Association[Map[# -> FileNameJoin[{basePath, "Combined.log"}] &, logLevels]],
    <|
      "Info" -> FileNameJoin[{basePath, "Info.log"}],
      "Warning" -> FileNameJoin[{basePath, "Info.log"}],  (* Same file as Info *)
      "Error" -> FileNameJoin[{basePath, "Error.log"}],
      "Debug" -> FileNameJoin[{basePath, "Debug.log"}]
    |>
  ];
  
  (*Rotate log file by renaming if too big*)
  rotateFile[level_] := Module[
    {file = logFiles[level], size, timeStamp, newName},
    If[
      FileExistsQ[file],
      size = FileByteCount[file];
      If[
       size >= maxSize,
       timeStamp = 
        DateString[{"Year", "Month", "Day", "_", "Hour", "Minute", 
          "Second"}];
       newName = 
        FileNameJoin[{basePath, level <> "_" <> timeStamp <> ".log"}];
       RenameFile[file, newName];
       ];
      ];
    ];
  
  (*Write a single message,opening/closing the stream each time*)
  writeLog[level_, message_] := Module[
    {file, timeStamp, logLine, stream},
    If[KeyExistsQ[logFiles, level],
      rotateFile[level];
      file = logFiles[level];
      stream = OpenAppend[file];
      timeStamp = 
       DateString[{"Year", "-", "Month", "-", "Day", " ", "Hour", ":",
          "Minute", ":", "Second"}];
      logLine = "[" <> level <> "] " <> timeStamp <> " - " <> message;
      WriteLine[stream, logLine];
      Close[stream];
      ];
    ];
  
  (*Return logger object*)
  <|
   "Log" -> Function[{level, msg}, writeLog[level, msg]],
   "Files" -> logFiles, 
   "MaxSize" -> maxSize,
   "Mode"-> mode
   |>
]


End[] (* End `Private` *)

EndPackage[]
