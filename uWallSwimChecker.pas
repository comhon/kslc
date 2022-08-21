
unit uWallSwimChecker;

{$MODE Delphi}

interface

uses
	Classes,
	uKSLog,
	uKSObjPass,
	uKSRepresentations;




		
type
	TWallSwim = class
	public
		Room: TKSRoom;
		RoomX, RoomY: integer;
		X1, Y1: integer;
		X2, Y2: integer;

		constructor Create(iRoom: TKSRoom; iRoomX, iRoomY, iX1, iY1, iX2, iY2: integer);

		function Duplicate(): TWallSwim;
	end;





	TWallSwimChecker = class
	public
		CapSwims, NumSwims: integer;
		Swim: array of TWallSwim;
		Log: TKSLog;
		OnProgress: TKSProgressEvent;

		constructor Create(iLog: TKSLog);
		destructor Destroy(); override;
		procedure Clear();

		procedure ProcessLevel(iLevel: TKSLevel);

		procedure CompareVertical  (iRoom: TKSRoom; arr1, arr2: PPassArray; x1, x2: integer);
		procedure CompareHorizontal(iRoom: TKSRoom; arr1, arr2: PPassArray; y1, y2: integer);
		procedure AddSwim(iRoom: TKSRoom; X1, Y1, X2, Y2: integer);
	end;


























implementation

uses
	SysUtils;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TWallSwim:

constructor TWallSwim.Create(iRoom: TKSRoom; iRoomX, iRoomY, iX1, iY1, iX2, iY2: integer);
begin
	inherited Create();
	Room := iRoom;
	RoomX := iRoomX;
	RoomY := iRoomY;
	X1 := iX1;
	Y1 := iY1;
	X2 := iX2;
	Y2 := iY2;
end;





function TWallSwim.Duplicate(): TWallSwim;
begin
	Result := TWallSwim.Create(Room, RoomX, RoomY, X1, Y1, X2, Y2);
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TWallSwimChecker:

constructor TWallSwimChecker.Create(iLog: TKSLog);
begin
	inherited Create();
	Log := iLog;
	NumSwims := 0;
	CapSwims := 0;
end;





destructor TWallSwimChecker.Destroy();
begin
	Clear();
	inherited Destroy();
end;





procedure TWallSwimChecker.Clear();
var
	i: integer;
begin
	for i := 0 to NumSwims - 1 do
	begin
		Swim[i].Free();
	end;
	NumSwims := 0;
	CapSwims := 0;
	SetLength(Swim, 0);
end;





procedure TWallSwimChecker.ProcessLevel(iLevel: TKSLevel);
var
	i: integer;
	r: TKSRoom;
begin
	for i := 0 to iLevel.NumRooms - 1 do
	begin
		r := iLevel.Room[i];
		if Assigned(Log) then Log.Log(LOG_INFO, 'Processing room [' + IntToStr(r.XPos) + ', ' + IntToStr(r.YPos) + ']');
		if Assigned(OnProgress) then OnProgress(Self, i, iLevel.NumRooms, 'Searching for possible wallswims');

		r.UpdatePassable();
		if Assigned(r.RoomLeft) then
		begin
			r.RoomLeft.UpdatePassable();
			CompareVertical(r, r.Passable, r.RoomLeft.Passable, 0, 599);
		end;
		if Assigned(r.RoomRight) then
		begin
			r.RoomRight.UpdatePassable();
			CompareVertical(r, r.Passable, r.RoomRight.Passable, 599, 0);
		end;
		if Assigned(r.RoomUp) then
		begin
			r.RoomUp.UpdatePassable();
			CompareHorizontal(r, r.Passable, r.RoomUp.Passable, 0, 239);
		end;
		if Assigned(r.RoomDown) then
		begin
			r.RoomDown.UpdatePassable();
			CompareHorizontal(r, r.Passable, r.RoomDown.Passable, 239, 0);
		end;
		if (i >= 20) then
		begin
			iLevel.Room[i - 20].FreePassable();
		end;
	end;

	// TODO: filter by reachability
end;





procedure TWallSwimChecker.CompareVertical(iRoom: TKSRoom; arr1, arr2: PPassArray; x1, x2: integer);
var
	i: integer;
	LastSame: integer;
begin
	// Compare the arrays and add passability from zero arr1 to nonzero arr2 to the list:
	LastSame := -1;
	for i := 0 to 239 do
	begin
		if ((arr1^[x1, i] = 0) and (arr2^[x2, i] > 0)) then continue;

		if (i - LastSame > WALLSWIM_HEIGHT) then
		begin
			AddSwim(iRoom, x1, LastSame + 1, x1, i);
		end;
		LastSame := i;
	end;		// for i
	if (239 - LastSame > WALLSWIM_HEIGHT) then
	begin
		AddSwim(iRoom, x1, LastSame + 1, x1, 239);
	end;
end;





procedure TWallSwimChecker.CompareHorizontal(iRoom: TKSRoom; arr1, arr2: PPassArray; y1, y2: integer);
var
	i: integer;
	LastSame: integer;
begin
	// Compare the arrays and add passability from zero arr1 to nonzero arr2 to the list:
	LastSame := -1;
	for i := 0 to 599 do
	begin
		if ((arr1^[i, y1] = 0) and (arr2^[i, y2] > 0)) then continue;

		if (i - LastSame > WALLSWIM_WIDTH) then
		begin
			AddSwim(iRoom, LastSame + 1, y1, i, y1);
		end;
		LastSame := i;
	end;		// for i
	if (599 - LastSame > WALLSWIM_WIDTH) then
	begin
		AddSwim(iRoom, LastSame + 1, y1, 599, y1);
	end;
end;





procedure TWallSwimChecker.AddSwim(iRoom: TKSRoom; X1, Y1, X2, Y2: integer);
begin
	if (NumSwims >= CapSwims) then
	begin
		CapSwims := NumSwims + 16;
		SetLength(Swim, CapSwims);
	end;
	Swim[NumSwims] := TWallSwim.Create(iRoom, iRoom.XPos, iRoom.YPos, X1, Y1, X2, Y2);
	NumSwims := NumSwims + 1;
end;





end.
