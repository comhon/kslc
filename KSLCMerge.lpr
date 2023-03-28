program KSLCMerge;

{$mode objfpc}{$H+}

{$IFDEF UNIX}
{$DEFINE UseCThreads}
{$ENDIF}

uses
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrmLevelMerger, uKSRepresentations, udlgDuplicateRooms, udlgRoomView
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TdlgRoomView, dlgRoomView);
  Application.Run;
end.

