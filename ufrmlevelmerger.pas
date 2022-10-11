unit ufrmLevelMerger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uKSRoomView,
  uKSMapView, uKSLog, udlgInstalledLevelList, uKSRepresentations, udlgDuplicateRooms;

type

  { TForm1 }

  TKSLevels = array [1..2] of TKSLevel;
  TKSRoomViews = array [1..2] of TKSRoomView;
  TKSMapViews = array [1..2] of TKSMapView;

  TForm1 = class(TForm)
    btnOpenLevel2: TButton;
    btnSelLevel1Path: TButton;
    btnSelLevel2Path: TButton;
    btnSelKSDir: TButton;
    btnOpenLevel1: TButton;
    btnDupl: TButton;
    edLevel1Path: TEdit;
    edLevel2Path: TEdit;
    edKSPath: TEdit;
    mvLevel1: TKSMapView;
    mvLevel2: TKSMapView;
    rv1: TKSRoomView;
    rv2: TKSRoomView;
    mLog: TMemo;
    procedure btnDuplClick(Sender: TObject);
    procedure btnOpenLevel1Click(Sender: TObject);
    procedure btnOpenLevel2Click(Sender: TObject);
    procedure btnSelKSDirClick(Sender: TObject);
    procedure btnSelLevel1PathClick(Sender: TObject);
    procedure btnSelLevel2PathClick(Sender: TObject);
    procedure edKSPathChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mvLevel1GoToRoom(Sender: TObject; iX, iY: integer);
    procedure mvLevel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mvLevel2GoToRoom(Sender: TObject; iX, iY: integer);
    procedure mvLevel2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mvLevel2RoomSelectionChanged(Sender: TObject);
  private
    fKSDir: string;
    gLog: TKSLog;
    fLevels: TKSLevels;
    fMapViews: TKSMapViews;
    fRoomViews: TKSRoomViews;
    procedure OnLogUpdate(Sender: TObject);
    function SelectLevel(out ALevelPath: string): boolean;
    function OpenLevel(ALevelNo: integer; ALevelFile: string): boolean;
    procedure GoToRoom(LevelNo: integer; XPos: integer; YPos: integer);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnSelKSDirClick(Sender: TObject);
var
  dlgOpen: TOpenDialog;
begin
  dlgOpen := TOpenDialog.Create(Self);
  try
    dlgOpen.InitialDir := '.';
    dlgOpen.Title := 'Where is your Knytt Stories exe:';
    dlgOpen.Filter := 'Knytt Stories Executable|Knytt Stories.exe';
    if (dlgOpen.Execute()) then
    begin
      fKSDir := IncludeTrailingPathDelimiter(ExtractFilePath(dlgOpen.Filename));
      gLog.Log(LOG_INFO, 'Knytt Stories directory set to "' + fKSDir + '"');
    end
    else
    begin
      gLog.Log(LOG_WARNING, 'Knytt Stories directory not set by user');
    end;
  finally
    dlgOpen.Free();
  end;
  edKSPath.Text := fKSDir;
end;

procedure TForm1.btnOpenLevel1Click(Sender: TObject);
begin
  OpenLevel(1,IncludeTrailingPathDelimiter(edLevel1Path.Text) + 'Map.bin');
end;

procedure TForm1.btnOpenLevel2Click(Sender: TObject);
begin
  OpenLevel(2,IncludeTrailingPathDelimiter(edLevel2Path.Text) + 'Map.bin');
end;

procedure TForm1.btnDuplClick(Sender: TObject);
var
	dlg: TdlgDuplicateRooms;
	Sel: TList;
begin
	dlg := TdlgDuplicateRooms.Create(Application, fLevels[1], fLevels[2]);
	try
          Sel:=mvLevel1.Selection;
          dlg.MapView.GoToCoord(mvLevel1.XPos, mvLevel1.YPos);
          dlg.MapView.Selection.Assign(Sel);
          dlg.ShowModal();
	finally
          dlg.Free();
	end;
end;

procedure TForm1.btnSelLevel1PathClick(Sender: TObject);
var
  aPath: string;
begin

  if SelectLevel(aPath) then
  begin
    edLevel1Path.Text := aPath;
  end;
end;

procedure TForm1.btnSelLevel2PathClick(Sender: TObject);
var
  aPath: string;
begin
  if SelectLevel(aPath) then
  begin
    edLevel2Path.Text := aPath;
  end;
end;

procedure TForm1.edKSPathChange(Sender: TObject);
begin
  fKSDir := IncludeTrailingPathDelimiter(edKSPath.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  gLog := TKSLog.Create(LOG_INFO);
  gLog.OnUpdate.Add(@OnLogUpdate);

  fMapViews[1]:=mvLevel1;
  fMapViews[2]:=mvLevel2;

  fRoomViews[1]:=rv1;
  fRoomViews[2]:=rv2;

  edKSPathChange(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i:=Low(fLevels) to High(fLevels) do
  begin
    if Assigned(fLevels[i]) then
    begin
      fLevels[i].Free;
    end;
  end;
  gLog.Free;
end;

procedure TForm1.mvLevel1GoToRoom(Sender: TObject; iX, iY: integer);
begin
  GotoRoom(1, iX, iY);
end;

procedure TForm1.mvLevel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    mvLevel1.ClearSelection;
  end;
end;

procedure TForm1.mvLevel2GoToRoom(Sender: TObject; iX, iY: integer);
begin
  GotoRoom(2, iX, iY);
end;

procedure TForm1.mvLevel2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    mvLevel2.ClearSelection;
  end;
end;

procedure TForm1.mvLevel2RoomSelectionChanged(Sender: TObject);
begin

end;

procedure TForm1.OnLogUpdate(Sender: TObject);
begin
  mLog.Lines.AddStrings(gLog.Items);
end;

function TForm1.SelectLevel(out ALevelPath: string): boolean;
var
  dlg: TdlgInstalledLevelList;
begin
  // open initial level
  dlg := TdlgInstalledLevelList.Create(nil);
  dlg.KSDir := fKSDir;
  try
    dlg.Caption := 'Select level:';
    case dlg.ShowModal() of
      mrKSDirNotFound:
      begin
        gLog.Log(LOG_ERROR, 'KS folder not found.');
      end;

      mrKSDirNoWorlds, mrNoWorldFound:
      begin
        gLog.Log(LOG_ERROR, 'There are no levels installed in the KS folder.');
      end;

      mrCancel:
      begin
        gLog.Log(LOG_INFO, 'No level selected.');
      end;
    end;

    ALevelPath := dlg.Path;
  finally
    dlg.Release();
  end;
end;

function TForm1.OpenLevel(ALevelNo: integer; ALevelFile: string): boolean;
var
  lvl: TKSLevel;
  aLevel: TKSLevel;
begin
  uKSRepresentations.gKSDir:=fKSDir;
  aLevel := TKSLevel.Create(gLog);
  //InitProgress('Loading level...');
  //lvl.OnProgress := OnLoadProgress;
  try
    aLevel.LoadFromFile(ALevelFile);
  except
    aLevel.Free();
    //  FinitProgress();
    Exit;
  end;
  if Assigned(fLevels[ALevelNo]) then
  begin
       fLevels[ALevelNo].Free;
  end;
  fLevels[ALevelNo] := aLevel;
  fMapViews[ALevelNo].Level:= aLevel;

  if (aLevel.NumRooms > 0) then
  begin
    fMapViews[ALevelNo].GoToCoord(aLevel.StartRoomX, aLevel.StartRoomY);
    GotoRoom(ALevelNo, aLevel.StartRoomX, aLevel.StartRoomY);
  end;
end;

procedure TForm1.GoToRoom(LevelNo: integer; XPos: integer; YPos: integer);
var
  aLevel: TKSLevel;
  nr: TKSRoom;
begin
  aLevel := fLevels[LevelNo];

  nr := aLevel.GetRoom(XPos, YPos);
  if not (Assigned(nr)) then
  begin
    Exit;
  end;

  fRoomViews[LevelNo].Room := nr;
end;

end.

