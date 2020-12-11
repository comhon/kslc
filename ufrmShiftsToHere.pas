
// ufrmShiftsToHere

// implements the form that shows Shifts incomingto a single room

{
Implementation notes:
=====================
ShiftList member is queried from the level object and remains assigned until a new query or until form destruction.
This way we can use a TKSShift reference in TListItem's data.
}

unit ufrmShiftsToHere;

interface

uses
	Windows,
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
	TfrmShiftsToHere = class(TForm)
		lvShifts: TListView;
    procedure lvShiftsDblClick(Sender: TObject);
	private
		ShiftList: TKSShiftList;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy(); override;
		
		procedure SetRoom(iRoom: TKSRoom);
	end;





var
	frmShiftsToHere: TfrmShiftsToHere;


















implementation

uses ufrmMain;

{$R *.dfm}





constructor TfrmShiftsToHere.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	ShiftList := nil;
end;





destructor TfrmShiftsToHere.Destroy();
begin
	ShiftList.Free();
	inherited Destroy();
end;





procedure TfrmShiftsToHere.SetRoom(iRoom: TKSRoom);
var
	i: integer;
	li: TListItem;
	s: TKSShift;
begin
	ShiftList.Free();
	ShiftList := iRoom.Parent.ShiftList.ExtractSingleToRoom(iRoom.XPos, iRoom.YPos);
	lvShifts.Items.BeginUpdate();
	try
		lvShifts.Items.Clear();
		for i := 0 to ShiftList.NumShifts - 1 do
		begin
			s := ShiftList.Shift[i];
			li := lvShifts.Items.Add();
			li.Caption := IntToStr(s.FromRoomX);
			li.SubItems.Add(IntToStr(s.FromRoomY));
			li.SubItems.Add(IntToStr(s.FromX));
			li.SubItems.Add(IntToStr(s.FromY));
			li.SubItems.Add(s.Kind);
			li.Data := s;
		end;
	finally
		lvShifts.Items.EndUpdate();
	end;
end;





procedure TfrmShiftsToHere.lvShiftsDblClick(Sender: TObject);
var
	s: TKSShift;
begin
	if (not(Assigned(lvShifts.ItemFocused)) or not(Assigned(lvShifts.ItemFocused.Data))) then
	begin
		Exit;
	end;

	s := TKSShift(lvShifts.ItemFocused.Data);
	frmMain.GotoRoom(s.FromRoomX, s.FromRoomY);
end;





end.
