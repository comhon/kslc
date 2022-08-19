unit ufrmShiftList;

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
	uKSRepresentations, Menus;





type
	TfrmShiftList = class(TForm)
		lvShifts: TListView;
    pmShift: TPopupMenu;
    pmiGotoFrom: TMenuItem;
    pmiGotoTo: TMenuItem;
    procedure pmiGotoToClick(Sender: TObject);
    procedure pmiGotoFromClick(Sender: TObject);
	public
		procedure UpdateFromLevel(iLevel: TKSLevel);
	end;




	
var
  frmShiftList: TfrmShiftList;































implementation

uses
	ufrmMain;




{$R *.lfm}





procedure TfrmShiftList.UpdateFromLevel(iLevel: TKSLevel);
var
	i: integer;
	li: TListItem;
	s: TKSShift;
begin
	lvShifts.Items.BeginUpdate();
	try
		lvShifts.Items.Clear();
		for i := 0 to iLevel.ShiftList.NumShifts - 1 do
		begin
			s := iLevel.ShiftList.Shift[i];
			li := lvShifts.Items.Add();
			li.Caption := IntToStr(s.FromRoomX);
			li.SubItems.Add(IntToStr(s.FromRoomY));
			li.SubItems.Add(IntToStr(s.FromX));
			li.SubItems.Add(IntToStr(s.FromY));
			li.SubItems.Add(IntToStr(s.ToRoomX));
			li.SubItems.Add(IntToStr(s.ToRoomY));
			li.SubItems.Add(IntToStr(s.ToX));
			li.SubItems.Add(IntToStr(s.ToY));
			li.SubItems.Add(s.Kind);
		end;
	finally
		lvShifts.Items.EndUpdate();
	end;
end;





procedure TfrmShiftList.pmiGotoToClick(Sender: TObject);
var
	li: TListItem;
begin
	li := lvShifts.ItemFocused;
	if not(Assigned(li)) then
	begin
		Exit;
	end;
	frmMain.GotoRoom(StrToInt(li.SubItems[3]), StrToInt(li.SubItems[4]));
end;





procedure TfrmShiftList.pmiGotoFromClick(Sender: TObject);
var
	li: TListItem;
begin
	li := lvShifts.ItemFocused;
	if not(Assigned(li)) then
	begin
		Exit;
	end;
	frmMain.GotoRoom(StrToInt(li.Caption), StrToInt(li.SubItems[0]));
end;





end.
