unit udlgRoomView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uKSRoomView;

type

  { TdlgRoomView }

  TdlgRoomView = class(TForm)
    rv1: TKSRoomView;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  dlgRoomView: TdlgRoomView;

implementation

{$R *.lfm}

{ TdlgRoomView }

procedure TdlgRoomView.FormCreate(Sender: TObject);
begin
  ClientWidth:=rv1.width-50;
  ClientHeight:=rv1.height-50;
  rv1.Left:=-25;
  rv1.Top:=-25;
end;

procedure TdlgRoomView.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction:=caNone;
  Hide;
end;

end.

