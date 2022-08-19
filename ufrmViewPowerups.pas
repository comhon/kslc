
unit ufrmViewPowerups;

{$MODE Delphi}

interface

uses
	Messages,
	SysUtils,
	Variants,
	Classes,
	Graphics,
	Controls,
	Forms,
	Dialogs,
	ComCtrls,
	uKSRepresentations;





type
	TfrmViewPowerups = class(TForm)
		lvPowerups: TListView;
    procedure lvPowerupsDblClick(Sender: TObject);
	public
		procedure UpdateFromLevel(iLevel: TKSLevel);
	end;





var
	frmViewPowerups: TfrmViewPowerups;


















implementation

uses
	uPowerupList,
	ufrmMain;





{$R *.lfm}





procedure TfrmViewPowerups.UpdateFromLevel(iLevel: TKSLevel);
var
	i: integer;
	pl: TPowerupList;
	p: TPowerup;
	li: TListItem;
begin
	pl := TPowerupList.Create();
	try
		pl.UpdateFromLevel(iLevel);
		lvPowerups.Items.BeginUpdate();
		try
			lvPowerups.Items.Clear();
			for i := 0 to pl.NumPowerups - 1 do
			begin
				p := pl.Powerup[i];
				li := lvPowerups.Items.Add();
				li.Caption := GetPowerupCaptionFromKind(p.Kind);
				li.ImageIndex := p.Kind - 3;
				li.SubItems.Add(IntToStr(p.RoomX));
				li.SubItems.Add(IntToStr(p.RoomY));
				li.SubItems.Add(IntToStr(p.X));
				li.SubItems.Add(IntToStr(p.X));
			end;
		finally
			lvPowerups.Items.EndUpdate();
		end;
	finally
		pl.Free();
	end;
end;





procedure TfrmViewPowerups.lvPowerupsDblClick(Sender: TObject);
var
	rx, ry: integer;
	// ix, iy: integer;
	li: TListItem;
	code: integer;
begin
	li := lvPowerups.ItemFocused;
	if not(Assigned(li)) then Exit;
	val(li.SubItems[0], rx, code);
	if (code <> 0) then Exit;
	val(li.SubItems[1], ry, code);
	if (code <> 0) then Exit;
	(*
	val(li.SubItems[2], rx, code);
	if (code <> 0) then Exit;
	val(li.SubItems[3], rx, code);
	if (code <> 0) then Exit;
	*)
	frmMain.GoToRoom(rx, ry);
end;





end.
