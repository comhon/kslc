
unit ufrmWallSwimList;

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
	uVectors,
	uKSRepresentations,
	uWallSwimChecker;





const
	NUM_COLUMNS = 6;





type
	TWSSortColumn = (wsscRoomX, wsscRoomY, wsscX1, wsscY1, wsscX2, wsscY2);





	TWSSortCriteria = record
		Criteria:  array[0..NUM_COLUMNS - 1] of TWSSortColumn;
		Ascending: array[0..NUM_COLUMNS - 1] of integer;
	end;




	
	TfrmWallSwimList = class(TForm)
		lvWallSwims: TListView;
		sbMain: TStatusBar;
		procedure lvWallSwimsColumnClick(Sender: TObject; Column: TListColumn);
		procedure lvWallSwimsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
		procedure FormCreate(Sender: TObject);
		procedure lvWallSwimsDblClick(Sender: TObject);
		procedure lvWallSwimsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
	public
		SortCriteria: TWSSortCriteria;
		CurrentLine: TVGLine;
		
		procedure UpdateFromChecker(iChecker: TWallSwimChecker);
		procedure ClearItems();
	end;





var
	frmWallSwimList: TfrmWallSwimList;





















	
implementation

uses
	ufrmMain;





{$R *.dfm}





procedure TfrmWallSwimList.UpdateFromChecker(iChecker: TWallSwimChecker);
var
	i: integer;
	li: TListItem;
	s: TWallSwim;
begin
	lvWallSwims.Items.BeginUpdate();
	try
		ClearItems();
		for i := 0 to iChecker.NumSwims - 1 do
		begin
			s := iChecker.Swim[i];
			li := lvWallSwims.Items.Add();
			li.Caption := IntToStr(s.RoomX);
			li.SubItems.Add(IntToStr(s.RoomY));
			li.SubItems.Add(IntToStr(s.X1));
			li.SubItems.Add(IntToStr(s.Y1));
			li.SubItems.Add(IntToStr(s.X2));
			li.SubItems.Add(IntToStr(s.Y2));
			li.Data := s.Duplicate();
		end;
	finally
		lvWallSwims.Items.EndUpdate();
	end;
	sbMain.SimpleText := 'WallSwims found: ' + IntToStr(iChecker.NumSwims);
	lvWallSwims.AlphaSort();
end;





procedure TfrmWallSwimList.ClearItems();
var
	i: integer;
	li: TListItem;
begin
	if Assigned(CurrentLine) then
	begin
		frmMain.RemVector(CurrentLine);
		CurrentLine.Free();
		CurrentLine := nil;
	end;
	
	for i := 0 to lvWallSwims.Items.Count - 1 do
	begin
		li := lvWallSwims.Items[i];
		TWallSwim(li.Data).Free();
	end;
	lvWallSwims.Items.Clear();
end;





procedure TfrmWallSwimList.lvWallSwimsColumnClick(Sender: TObject; Column: TListColumn);
var
	wssc: TWSSortColumn;
	i, u: integer;
begin
	wssc := TWSSortColumn(Column.Index);
	if (SortCriteria.Criteria[0] = wssc) then
	begin
		SortCriteria.Ascending[0] := -SortCriteria.Ascending[0];
	end
	else
	begin
		for i := 0 to NUM_COLUMNS - 1 do
		begin
			if (SortCriteria.Criteria[i] = wssc) then
			begin
				for u := i downto 1 do
				begin
					SortCriteria.Criteria[u] := SortCriteria.Criteria[u - 1];
				end;		// for u
				SortCriteria.Criteria[0] := wssc;
			end;
		end;		// for i
		SortCriteria.Ascending[0] := 1;
	end;
	lvWallSwims.AlphaSort();
end;





procedure TfrmWallSwimList.lvWallSwimsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
	ci, idx: integer;
	v1, v2: integer;
	code: integer;
begin
	idx := 0;
	repeat
		case SortCriteria.Criteria[idx] of
			wsscRoomX: ci := -1;
			wsscRoomY: ci := 0;
			wsscX1:    ci := 1;
			wsscY1:    ci := 2;
			wsscX2:    ci := 3;
			else       ci := 4;
		end;
		if (ci < 0) then
		begin
			val(Item1.Caption, v1, code);
			val(Item2.Caption, v2, code);
		end
		else
		begin
			val(Item1.SubItems[ci], v1, code);
			val(Item2.SubItems[ci], v2, code);
		end;
		Compare := SortCriteria.Ascending[idx] * (v1 - v2);
		idx := idx + 1;
	until (Compare <> 0) or (idx = NUM_COLUMNS);
end;





procedure TfrmWallSwimList.FormCreate(Sender: TObject);
var
	i: integer;
begin
	for i := 0 to NUM_COLUMNS - 1 do
	begin
		SortCriteria.Ascending[i] := 1;
		SortCriteria.Criteria[i] := TWSSortColumn(i);
	end;		// for i
end;





procedure TfrmWallSwimList.lvWallSwimsDblClick(Sender: TObject);
var
	ws: TWallSwim;
begin
	if not(Assigned(lvWallSwims.ItemFocused)) then Exit;
	ws := TWallSwim(lvWallSwims.ItemFocused.Data);
	frmMain.GotoRoom(ws.RoomX, ws.RoomY);
end;





procedure TfrmWallSwimList.lvWallSwimsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var
	ws: TWallSwim;
begin
	if (Change = ctState) then
	begin
		if not(Assigned(lvWallSwims.ItemFocused)) then Exit;
		ws := TWallSwim(lvWallSwims.ItemFocused.Data);
		if not(Assigned(CurrentLine)) then
		begin
			CurrentLine := TVGLine.Create(0, 0, 0, 0, $ff, 3);
			frmMain.RegVector(CurrentLine);
		end;
		CurrentLine.X1 := ws.X1 + 24;
		CurrentLine.Y1 := ws.Y1 + 24;
		CurrentLine.X2 := ws.X2 + 24;
		CurrentLine.Y2 := ws.Y2 + 24;
		CurrentLine.Data := ws.Room;
		CurrentLine.Room := ws.Room;
		CurrentLine.Changed();
	end;
end;





procedure TfrmWallSwimList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caHide;
	if Assigned(CurrentLine) then
	begin
		frmMain.RemVector(CurrentLine);
		CurrentLine.Free();
	end;
end;





end.
