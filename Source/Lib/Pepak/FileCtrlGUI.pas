unit FileCtrlGUI;

interface

type
  TSelectDirOpt = (sdAllowCreate, sdPerformCreate, sdPrompt);
  TSelectDirOpts = set of TSelectDirOpt;

function SelectDirectory(var Directory: string; Options: TSelectDirOpts; HelpCtx: Longint): Boolean; 

implementation

uses
  Windows, ShlObj, ActiveX;

resourcestring
  SSelectDirectory = 'Select a directory';

function SelectDirectory(var Directory: string; Options: TSelectDirOpts; HelpCtx: Longint): Boolean;
var Info: TBrowseInfo;
    Path: array[0..MAX_PATH+1] of Char;
    Item: PItemIDList;
begin
  Result := False;
  CoInitialize(nil);
  try
    Path[0] := #0;
    Info.hwndOwner := 0;
    Info.pidlRoot := nil;
    Info.pszDisplayName := nil;
    Info.lpszTitle := PChar(SSelectDirectory);
    Info.ulFlags := 0;
    Info.lpfn := nil;
    Info.lParam := 0;
    Info.iImage := 0;
//    Info.ulFlags := BIF_RETURNONLYFSDIRS ;//+ BIF_EDITBOX + BIF_VALIDATE + $40 {BIF_NEWDIALOGSTYLE};
//    if not (sdAllowCreate in Options) then
//      Info.ulFlags := Info.ulFlags + $200 {BIF_NONEWFOLDERBUTTON};
    Item := {$IFDEF FPC} SHBrowseForFolder(@Info) {$ELSE} SHBrowseForFolder(Info) {$ENDIF};
    if (Item <> nil) then
      try
        if SHGetPathFromIDList(Item, @Path[0]) then
          begin
          Directory := Path;
          Result := True;
          end;
      finally
        CoTaskMemFree(Item);
        end;
  finally
    CoUninitialize;
    end;
end;

end.
