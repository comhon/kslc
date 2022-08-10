
unit uReachCalc;

{$MODE Delphi}

interface

uses
	Classes,
	uKSRepresentations,
	uKSLog;





const
	REACHROOM_WIDTH = ROOM_WIDTH + MIN_WIDTH;
	REACHROOM_HEIGHT = ROOM_HEIGHT + MIN_HEIGHT;
	kspRun       = 1;
	kspWallclimb = 2;
	kspDblJump   = 4;
	kspHighJump  = 8;
	kspEye       = 16;
	kspDetector  = 32;
	kspUmbrella  = 64;
	kspHologram  = 128;
	possJump     = 256;
	possDblJump  = 512;
	possClimb    = 1024;




	
type
	TReachArray = array[0..REACHROOM_WIDTH - 1, 0..REACHROOM_HEIGHT - 1] of boolean;
	PReachArray = ^TReachArray;
	TReachVector = packed record
		x, y: byte;
	end;
	TPowerup = word;
	TReachVectorArray = array[0..REACHROOM_WIDTH - 1, 0..REACHROOM_HEIGHT - 1] of TReachVector;
	PReachVectorArray = ^TReachVectorArray;
	TReachPowerupArray = array[0..REACHROOM_WIDTH - 1, 0..REACHROOM_HEIGHT - 1] of TPowerup;

	TRoomReachRec = record
		Reach: TReachArray;
		Vector: TReachVectorArray;
		Powerup: TReachPowerupArray;
	end;

	PRoomReachRec = ^TRoomReachRec;

	TReachCalc = class;		// fwd

	TProgressEvent = procedure(Sender: TObject; iCur, iMax: integer) of object;





	TReachRoom = class
	private
		FReachRec: PRoomReachRec;
		FSwapFile: string;

		function  FGetReach(): PReachArray;
		function  FGetReachable   (iX, iY: integer): boolean;
		function  FGetReachVector (iX, iY: integer): TReachVector;
		function  FGetReachVectorX(iX, iY: integer): byte;
		function  FGetReachVectorY(iX, iY: integer): byte;
		function  FGetDblJump     (iX, iY: integer): boolean;
		function  FGetPowerup     (iX, iY: integer): TPowerup;
		procedure FSetReachable   (iX, iY: integer; iVal: boolean);
		procedure FSetReachVector (iX, iY: integer; iVal: TReachVector);
		procedure FSetReachVectorX(iX, iY: integer; iVal: byte);
		procedure FSetReachVectorY(iX, iY: integer; iVal: byte);
		procedure FSetDblJump     (iX, iY: integer; iVal: boolean);
		procedure FSetPowerup     (iX, iY: integer; iVal: TPowerup);

		procedure ProcessBounds(iX, iY: integer);		// checks if on bounds, if so, mark neighboring room dirty
	public
		Room: TKSRoom;
		IsDirty: boolean;
		Parent: TReachCalc;
		LastUseTime: cardinal;

		constructor Create(iRoom: TKSRoom; iParent: TReachCalc);
		destructor Destroy(); override;
		procedure Clear();

		procedure SwapIn();
		procedure SwapOut();

		procedure CalcFromPoint(iX, iY: integer; iPowerup: TPowerup);		// starts calculation from the specified point (used for start position)
		procedure CalcFromEdges();		// starts calculation from all four edges, if existing. (used for dirty-elimination)

		function  ReachFallDown (iX, iY: integer; iPowerup: TPowerup): integer;		// Calculates reach from [iX, iY] with iPowerup, updates ReachRec and returns lowest y-pos available
		procedure ReachWalkLeft (iX, iY: integer; iPowerup: TPowerup);
		procedure ReachWalkRight(iX, iY: integer; iPowerup: TPowerup);
		procedure ReachJump     (iX, iY: integer; iPowerup: TPowerup);

		procedure SetInitial();		// sets Powerups array based on the room topology (no climb, sticky etc)

		procedure OrPowerup     (iX, iY: integer; iVal: TPowerup);
		procedure OrPowerupRect (t, l, b, r: integer; iVal: TPowerup);
		procedure AndPowerup    (iX, iY: integer; iVal: TPowerup);
		procedure AndPowerupRect(t, l, b, r: integer; iVal: TPowerup);

		function CheckPos   (iX, iY: integer): boolean;
		function CheckBottom(iX, iY: integer): boolean;
		function CheckLeft  (iX, iY: integer): boolean;
		function CheckRight (iX, iY: integer): boolean;
		function CheckWallAhead(iX, iY: integer): integer;		// return the number of pixels required to climb up

		property Reach: PReachArray read FGetReach;
		property Reachable[x, y: integer]: boolean read FGetReachable write FSetReachable;
		property ReachVector[x, y: integer]: TReachVector read FGetReachVector write FSetReachVector;
		property ReachVectorX[x, y: integer]: byte read FGetReachVectorX write FSetReachVectorX;
		property ReachVectorY[x, y: integer]: byte read FGetReachVectorY write FSetReachVectorY;
		property ReachDblJump[x, y: integer]: boolean read FGetDblJump write FSetDblJump;
		property ReachPowerup[x, y: integer]: TPowerup read FGetPowerup write FSetPowerup;
	end;





	TReachCalc = class
	private

	public
		SysLog: TKSLog;
		CapRooms, NumRooms: integer;
		Room: array of TReachRoom;
		Level: TKSLevel;
		ProgressCur: integer;
		ProgressMax: integer;

		OnProgress: TProgressEvent;

		constructor Create(iLevel: TKSLevel);
		destructor Destroy(); override;
		procedure Clear();

		function GetRoom(iX, iY: integer): TReachRoom;

		procedure Calc();

		procedure DoProgress();

		procedure SetDirtyRoom(iRoom: TKSRoom);
		procedure SwapOutAFew(iAmount: integer = 6);
		procedure SwapOutAll();

		procedure Log(iLevel: integer; txt: string);
	end;
























implementation

uses
	SysUtils;





var
	TempPath: string;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// globals:

function AllocSwapFileName(iX, iY: integer): string;
begin
	Result := TempPath + '\KSLC_ReachCalc_' + IntToStr(iX) + '_' + IntToStr(iY) + '.tmp';
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TReachRoom:

constructor TReachRoom.Create(iRoom: TKSRoom; iParent: TReachCalc);
begin
	inherited Create();
	Room := iRoom;
	Parent := iParent;
	FReachRec := nil;
	FSwapFile := '';
end;





destructor TReachRoom.Destroy();
begin
	Clear();
	inherited Destroy();
end;





procedure TReachRoom.Clear();
begin
	// remove memory and / or swap
	if Assigned(FReachRec) then
	begin
		FreeMem(FReachRec);
		FReachRec := nil;
	end;

	if (FSwapFile <> '') then
	begin
		if (FileExists(FSwapFile)) then
		begin
			DeleteFile(FSwapFile);
			FSwapFile := '';
		end;
	end;
end;





procedure TReachRoom.SwapIn();
var
	f: file of TRoomReachRec;
begin
	if Assigned(FReachRec) then
	begin
		Exit;
	end;

	GetMem(FReachRec, sizeof(FReachRec^));
	if not(Assigned(FReachRec)) then
	begin
		Parent.SwapOutAFew();
		GetMem(FReachRec, sizeof(FReachRec^));
		if not(Assigned(FReachRec)) then
		begin
			Parent.SwapOutAll();
			GetMem(FReachRec, sizeof(FReachRec^));
			if not(Assigned(FReachRec)) then
			begin
				raise Exception.Create('TReachRoom.SwapIn(): not enough memory even after swapouts. Aborting calculation');
			end;
		end;
	end;		// if (first allocation unsuccessful)

	if (FSwapFile = '') then		// only valid after initial creation
	begin
		FillChar(FReachRec^, sizeof(FReachRec^), 0);
		SetInitial();
		Exit;
	end;

	AssignFile(f, FSwapFile);
	try
		Reset(f);
	except
		// TODO: exception processing for "file not found"
		raise;
	end;
	try
		Read(f, FReachRec^);
	finally
		CloseFile(f);
	end;
end;





procedure TReachRoom.SwapOut();
var
	f: file of TRoomReachRec;
begin
	if not(Assigned(FReachRec)) then
	begin
		Exit;
	end;

	if (FSwapFile = '') then
	begin
		FSwapFile := AllocSwapFileName(Room.XPos, Room.YPos);
	end;

	AssignFile(f, FSwapFile);
	try
		Rewrite(f);
	except
		Parent.Log(LOG_ERROR, 'TReachRoom.SwapOut(): cannot open file "' + FSwapFile + '" for writing!"');
		Exit;
	end;
	try
		Write(f, FReachRec^);
	finally
		CloseFile(f);
	end;
end;





procedure TReachRoom.CalcFromPoint(iX, iY: integer; iPowerup: TPowerup);		// starts calculation from the specified point (used for start position)
begin
	Room.UpdatePassable();
	IsDirty := false;
	if not(CheckPos(iX, iY)) then
	begin
		Parent.Log(LOG_ERROR, 'Cannot start Reach calculation, start position is inside a wall!');
		raise Exception.Create('Cannot start Reach calculation, start position is inside a wall!');
	end;
	iY := ReachFallDown(iX, iY, iPowerup);

	ReachWalkLeft(iX, iY, iPowerup);
	ReachWalkRight(iX, iY, iPowerup);
	// TODO
end;





procedure TReachRoom.CalcFromEdges();		// starts calculation from all four edges, if existing. (used for dirty-elimination)
begin
	IsDirty := false;
	// TODO
end;





function TReachRoom.ReachFallDown(iX, iY: integer; iPowerup: TPowerup): integer;		// Calculates reach from [iX, iY] with iPowerup, updates ReachRec and returns lowest y-pos available
begin
	while (CheckBottom(iX, iY - 1)) do
	begin
		ReachPowerup[iX, iY] := ReachPowerup[iX, iY] + iPowerup;
		iY := iY - 1;
	end;
	Result := iY + 1;
end;





procedure TReachRoom.ReachWalkLeft(iX, iY: integer; iPowerup: TPowerup);
var
	res: integer;
begin
	if not(CheckLeft(iX - 1, iY)) then
	begin
		Exit;
	end;

	// Walk left until either Reach is true and iPowerup in ReachPowerup or hit a wall or drop from a cliff or another event:
	repeat
		iX := iX - 1;
		if (iX < -MIN_WIDTH) then
		begin
			// reached the room's edge
			// TODO
			Exit;
		end;

		if (Reachable[iX, iY] and (iPowerup and ReachPowerup[iX, iY] = iPowerup)) then
		begin
			// we've been here already with same or even better powerups
			break;
		end;

		res := CheckWallAhead(iX, iY);
		if (res = 0) then
		begin
			// there is no wall and no step, continue walking:

			if (CheckBottom(iX, iY + 1)) then
			begin
				// TODO: drop off a cliff
				break;
			end;
		end
		else if (res <= WALKOVER_HEIGHT) then
		begin
			// there is a walkable step, step up:
			iY := iY - res;
		end
		else
		begin
			// there is a wall, climb if available
			// TODO
			break;
		end;

		ReachJump(iX, iY, iPowerup);
	until false;
end;





procedure TReachRoom.ReachWalkRight(iX, iY: integer; iPowerup: TPowerup);
begin
	// TODO
end;





procedure TReachRoom.ReachJump(iX, iY: integer; iPowerup: TPowerup);
begin
	// TODO
end;





procedure TReachRoom.SetInitial();
var
	x, y: integer;
	lay: integer;
	obj: TKSObjLayer;
begin
	// sets Powerups array based on the room topology (no climb, sticky, walls, floor etc)
	// ASSUME: FReachRec is zeroed out, Room is assigned, data swapped in

	Room.UpdatePassable();

	for lay := 4 to 7 do
	begin
		obj := Room.Data.Obj[lay];
		for x := 0 to 24 do
		begin
			for y := 0 to 9 do
			begin
				case obj.Bank[y, x] of
					// TODO
					0:		// system:
					begin
						case obj.Obj[y, x] of
							3:
							begin
								OrPowerupRect(y * 24 + 12, x * 24 + 6, y * 24 + 24, x * 24 + 6, kspRun);
							end;
							4:
							begin
								OrPowerupRect(y * 24 + 12, x * 24 + 6, y * 24 + 24, x * 24 + 6, kspWallClimb);
							end;
							5:
							begin
								OrPowerupRect(y * 24 + 12, x * 24 + 6, y * 24 + 24, x * 24 + 6, kspDblJump);
							end;
							6:
							begin
								OrPowerupRect(y * 24 + 12, x * 24 + 6, y * 24 + 24, x * 24 + 6, kspHighJump);
							end;
							7:
							begin
								OrPowerupRect(y * 24 + 12, x * 24 + 6, y * 24 + 24, x * 24 + 6, kspEye);
							end;
							8:
							begin
								OrPowerupRect(y * 24 + 12, x * 24 + 6, y * 24 + 24, x * 24 + 6, kspDetector);
							end;
							9:
							begin
								OrPowerupRect(y * 24 + 12, x * 24 + 6, y * 24 + 24, x * 24 + 6, kspUmbrella);
							end;
							10:
							begin
								OrPowerupRect(y * 24 + 12, x * 24 + 6, y * 24 + 24, x * 24 + 6, kspHologram);
							end;
							11:		// noclimb
							begin
								AndPowerupRect(y * 24, x * 24, y * 24 + 24, x * 24 + 24, word(not(possClimb)));		// TODO: no-climb has a several px overlap (?)
							end;
							13:		// sticky
							begin
								// AndPowerupRect(y * 24, x * 24, y * 24 + 24, x * 24 + 24, not(possWalk));		// TODO: sticky has a several px overlap (?)
							end;
							25:		// nojump
							begin
								AndPowerupRect(y * 24, x * 24, y * 24 + 24, x * 24 + 24, word(not(possJump or possDblJump)));		// TODO: no-jump has a several px overlap (?)
							end;
						end;
					end;		// case 0 - system bank

					1:		//
				end;
			end;		// for y
		end;		// for x
	end;		// for lay
end;





procedure TReachRoom.OrPowerup(iX, iY: integer; iVal: TPowerup);
begin
	SwapIn();
	if ((FReachRec.Powerup[iX, iY] or iVal) <> FReachRec.Powerup[iX, iY]) then
	begin
		FReachRec.Powerup[iX, iY] := FReachRec.Powerup[iX, iY] or iVal;
		IsDirty := true;
		ProcessBounds(iX, iY);
	end;
end;





procedure TReachRoom.OrPowerupRect(t, l, b, r: integer; iVal: TPowerup);
var
	x, y: integer;
	IsChanged: boolean;
begin
	SwapIn();
	IsChanged := false;
	for x := l to r do
	begin
		for y := t to b do
		begin
			if ((FReachRec.Powerup[x, y] or iVal) <> FReachRec.Powerup[x, y]) then
			begin
				FReachRec.Powerup[x, y] := FReachRec.Powerup[x, y] or iVal;
				IsChanged := true;
			end;
		end;		// for y
	end;		// for x

	if (IsChanged) then
	begin
		IsDirty := true;
		ProcessBounds(l, t);
		ProcessBounds(l, b);
		ProcessBounds(r, t);
		ProcessBounds(r, b);
	end;
end;





procedure TReachRoom.AndPowerup    (iX, iY: integer; iVal: TPowerup);
begin
	SwapIn();
	if ((FReachRec.Powerup[iX, iY] and iVal) <> FReachRec.Powerup[iX, iY]) then
	begin
		FReachRec.Powerup[iX, iY] := FReachRec.Powerup[iX, iY] and iVal;
		IsDirty := true;
		ProcessBounds(iX, iY);
	end;
end;





procedure TReachRoom.AndPowerupRect(t, l, b, r: integer; iVal: TPowerup);
var
	x, y: integer;
	IsChanged: boolean;
begin
	SwapIn();
	IsChanged := false;
	for x := l to r do
	begin
		for y := t to b do
		begin
			if ((FReachRec.Powerup[x, y] and iVal) <> FReachRec.Powerup[x, y]) then
			begin
				FReachRec.Powerup[x, y] := FReachRec.Powerup[x, y] and iVal;
				IsChanged := true;
			end;
		end;		// for y
	end;		// for x

	if (IsChanged) then
	begin
		IsDirty := true;
		ProcessBounds(l, t);
		ProcessBounds(l, b);
		ProcessBounds(r, t);
		ProcessBounds(r, b);
	end;
end;





function TReachRoom.CheckPos(iX, iY: integer): boolean;
var
	x, y: integer;
begin
	SwapIn();
	for y := iY to iY + MIN_HEIGHT do
	begin
		for x := iX to iX + MIN_WIDTH do
		begin
			if ((x > 0) and (x < ROOM_WIDTH) and (y > 0) and (y < ROOM_HEIGHT)) then
			begin
				if (Room.Passable[x, y] <> 0) then
				begin
					Result := false;
					Exit;
				end;
			end;
		end;		// for x
	end;		// for y
	Result := true;
end;





function TReachRoom.CheckBottom(iX, iY: integer): boolean;
var
	x, y: integer;
begin
	SwapIn();
	y := iY + MIN_HEIGHT;
	for x := iX to iX + MIN_WIDTH do
	begin
		if ((x > 0) and (x < ROOM_WIDTH) and (y > 0) and (y < ROOM_HEIGHT)) then
		begin
			if (Room.Passable[x, y] <> 0) then
			begin
				Result := false;
				Exit;
			end;
		end;
	end;		// for x
	Result := true;
end;





function TReachRoom.CheckLeft(iX, iY: integer): boolean;
var
	x, y: integer;
begin
	SwapIn();
	// TODO
	(*
	y := iY + MIN_HEIGHT;
	for x := iX to iX + MIN_WIDTH do
	begin
		if ((x > 0) and (x < ROOM_WIDTH) and (y > 0) and (y < ROOM_HEIGHT)) then
		begin
			if (Room.Passable[x, y] <> 0) then
			begin
				Result := false;
				Exit;
			end;
		end;
	end;		// for x
	*)
	Result := true;
end;





function TReachRoom.CheckRight(iX, iY: integer): boolean;
var
	x, y: integer;
begin
	SwapIn();
	// TODO
	(*
	y := iY + MIN_HEIGHT;
	for x := iX to iX + MIN_WIDTH do
	begin
		if ((x > 0) and (x < ROOM_WIDTH) and (y > 0) and (y < ROOM_HEIGHT)) then
		begin
			if (Room.Passable[x, y] <> 0) then
			begin
				Result := false;
				Exit;
			end;
		end;
	end;		// for x
	*)
	Result := true;
end;





function TReachRoom.CheckWallAhead(iX, iY: integer): integer;		// return the number of pixels required to climb up
var
	y: integer;
	LastWall: integer;
begin
	LastWall := iY;
	for y := iY downto 0 do
	begin
		if (Room.Passable[iX, y] <> 0) then
		begin
			LastWall := y;
		end
		else if (LastWall - y > MIN_HEIGHT) then
		begin
			Result := iY - LastWall;
			Exit;
		end;
	end;		// for y
	Result := iY;
end;





function TReachRoom.FGetReach(): PReachArray;
begin
	SwapIn();
	Result := Reach;
end;





function TReachRoom.FGetReachable(iX, iY: integer): boolean;
begin
	SwapIn();
	Result := FReachRec.Reach[iX + MIN_WIDTH, iY];
end;





function TReachRoom.FGetReachVector(iX, iY: integer): TReachVector;
begin
	SwapIn();
	Result := FReachRec.Vector[iX + MIN_WIDTH, iY];
end;





function TReachRoom.FGetReachVectorX(iX, iY: integer): byte;
begin
	SwapIn();
	Result := FReachRec.Vector[iX + MIN_WIDTH, iY].x;
end;





function TReachRoom.FGetReachVectorY(iX, iY: integer): byte;
begin
	SwapIn();
	Result := FReachRec.Vector[iX + MIN_WIDTH, iY].x;
end;





function TReachRoom.FGetDblJump(iX, iY: integer): boolean;
begin
	SwapIn();
	Result := (FReachRec.Powerup[iX + MIN_WIDTH, iY] and possDblJump) = possDblJump;
end;





function TReachRoom.FGetPowerup     (iX, iY: integer): TPowerup;
begin
	SwapIn();
	Result := FReachRec.Powerup[iX + MIN_WIDTH, iY];
end;





procedure TReachRoom.FSetReachable(iX, iY: integer; iVal: boolean);
begin
	SwapIn();
	if (FReachRec.Reach[iX, iY] <> iVal) then
	begin
		FReachRec.Reach[iX, iY] := iVal;
		IsDirty := true;
		ProcessBounds(iX, iY);
	end;
end;





procedure TReachRoom.FSetReachVector(iX, iY: integer; iVal: TReachVector);
var
	rv: TReachVector;
begin
	SwapIn();
	rv := FReachRec.Vector[iX, iY];
	// if (FReachRec.Vector[iX, iY] <> iVal) then		// incompatible types, must compare member-wise
	if ((rv.x <> iVal.x) or (rv.y <> iVal.y)) then
	begin
		FReachRec.Vector[iX, iY] := iVal;
		IsDirty := true;
		ProcessBounds(iX, iY);
	end;
end;





procedure TReachRoom.FSetReachVectorX(iX, iY: integer; iVal: byte);
begin
	SwapIn();
	if (FReachRec.Vector[iX, iY].x <> iVal) then
	begin
		FReachRec.Vector[iX, iY].x := iVal;
		IsDirty := true;
		ProcessBounds(iX, iY);
	end;
end;





procedure TReachRoom.FSetReachVectorY(iX, iY: integer; iVal: byte);
begin
	SwapIn();
	if (FReachRec.Vector[iX, iY].y <> iVal) then
	begin
		FReachRec.Vector[iX, iY].y := iVal;
		IsDirty := true;
		ProcessBounds(iX, iY);
	end;
end;






procedure TReachRoom.FSetDblJump     (iX, iY: integer; iVal: boolean);
var
	v: TPowerup;
begin
	SwapIn();
	if iVal then
	begin
		v := possDblJump;
	end
	else
	begin
		v := 0;
	end;
	
	if ((FReachRec.Powerup[iX, iY] and possDblJump) <> v) then
	begin
		FReachRec.Powerup[iX, iY] := (FReachRec.Powerup[iX, iY] and not(possDblJump)) or v;
		IsDirty := true;
		ProcessBounds(iX, iY);
	end;
end;





procedure TReachRoom.FSetPowerup     (iX, iY: integer; iVal: TPowerup);
begin
	SwapIn();
	if (FReachRec.Powerup[iX, iY] <> iVal) then
	begin
		FReachRec.Powerup[iX, iY] := iVal;
		IsDirty := true;
		ProcessBounds(iX, iY);
	end;
end;





procedure TReachRoom.ProcessBounds(iX, iY: integer);
begin
	if (iX = 0) then
	begin
		Parent.SetDirtyRoom(Room.RoomLeft);
	end
	else if (iX = REACHROOM_WIDTH - 1) then
	begin
		Parent.SetDirtyRoom(Room.RoomRight);
	end;

	if (iY = 0) then
	begin
		Parent.SetDirtyRoom(Room.RoomUp);
	end
	else if (iY = REACHROOM_HEIGHT - 1) then
	begin
		Parent.SetDirtyRoom(Room.RoomDown);
	end;
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TReachCalc:

constructor TReachCalc.Create(iLevel: TKSLevel);
begin
	inherited Create();
	NumRooms := 0;
	CapRooms := 0;
	Level := iLevel;
	ProgressCur := 0;
	ProgressMax := 1;
end;





destructor TReachCalc.Destroy();
begin
	Clear();
	inherited Destroy();
end;





procedure TReachCalc.Clear();
var
	i: integer;
begin
	for i := 0 to NumRooms - 1 do
	begin
		if Assigned(Room[i]) then
		begin
			Room[i].Free();
		end;
	end;
	NumRooms := 0;
	CapRooms := 0;
	SetLength(Room, 0);
end;





function TReachCalc.GetRoom(iX, iY: integer): TReachRoom;
var
	r: TKSRoom;
	i: integer;
begin
	r := Level.GetRoom(iX, iY);
	if not(Assigned(r)) then
	begin
		Result := nil;
		Exit;
	end;
	for i := 0 to NumRooms - 1 do
	begin
		if (Room[i].Room = r) then
		begin
			Result := Room[i];
			Exit;
		end;
	end;
	Result := TReachRoom.Create(r, Self);
	if not(Assigned(Result)) then
	begin
		Exit;
	end;
	if (NumRooms >= CapRooms) then
	begin
		CapRooms := NumRooms + 32;
		SetLength(Room, CapRooms);
	end;
	Room[NumRooms] := Result;
	NumRooms := NumRooms + 1;
end;





procedure TReachCalc.Calc();
var
	FirstRoom: TReachRoom;
	i: integer;
	NumDirty: integer;
	pwrup: TPowerup;
begin
	Clear();
	ProgressMax := 1;
	ProgressCur := 0;
	DoProgress();
	FirstRoom := GetRoom(Level.StartRoomX, Level.StartRoomY);
	pwrup := 0;
	if (Level.StartPower[0]) then pwrup := pwrup or kspRun;
	if (Level.StartPower[1]) then pwrup := pwrup or kspWallClimb;
	// TODO: other powerups:
	
	FirstRoom.CalcFromPoint(Level.StartX, Level.StartY, pwrup);
	repeat
		// update the dirty rooms:
		for i := 0 to NumRooms - 1 do
		begin
			if (Room[i].IsDirty) then
			begin
				Room[i].CalcFromEdges();
			end;
		end;		// for i

		// count the dirty rooms:
		NumDirty := 0;
		for i := 0 to NumRooms - 1 do
		begin
			if (Room[i].IsDirty) then
			begin
				NumDirty := NumDirty + 1;
			end;
		end;		// for i
	until (NumDirty = 0);
end;





procedure TReachCalc.DoProgress();
begin
	if Assigned(OnProgress) then
	begin
		OnProgress(Self, ProgressCur, ProgressMax);
	end;
end;





procedure TReachCalc.SetDirtyRoom(iRoom: TKSRoom);
var
	r: TReachRoom;
begin
	r := GetRoom(iRoom.XPos, iRoom.YPos);
	if Assigned(r) then
	begin
		r.IsDirty := true;
	end;
end;





procedure TReachCalc.SwapOutAFew(iAmount: integer);
var
	i, j, Idx: integer;
	LastUseTime: cardinal;
begin
	if (NumRooms <= 0) then
	begin
		Exit;
	end;
	for j := 1 to iAmount do
	begin
		Idx := 0;
		LastUseTime := Room[0].LastUseTime;
		for i := 1 to NumRooms - 1 do
		begin
			if (Room[i].LastUseTime < LastUseTime) then
			begin
				LastUseTime := Room[i].LastUseTime;
				Idx := i;
			end;
		end;
		Room[Idx].SwapOut();
	end;
end;





procedure TReachCalc.SwapOutAll();
var
	i: integer;
begin
	for i := 0 to NumRooms - 1 do
	begin
		Room[i].SwapOut();
	end;
end;





procedure TReachCalc.Log(iLevel: integer; txt: string);
begin
	if Assigned(SysLog) then
	begin
		SysLog.Log(iLevel, txt);
	end
	else
	begin
		//*h OutputDebugString(PChar(txt + #13#10));
	end;
end;





initialization
	// find the temporary path, create subfolder:
        //*h
        (*
        SetLength(TempPath, MAX_PATH);
	SetLength(TempPath, GetTempPath(MAX_PATH, @(TempPath[1])));
        *)
	if (TempPath = '') then TempPath := '/home/comhon';
	if (TempPath[Length(TempPath)] <> '/') then TempPath := TempPath + '/';
	TempPath := TempPath + '.KSLC';
	if not(DirectoryExists(TempPath)) then
	begin
		CreateDir(TempPath);
	end;




	
end.
