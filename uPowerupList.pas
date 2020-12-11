
unit uPowerupList;

interface

uses
	uKSRepresentations;




type
	TPowerup = class
	public
		Kind: integer;
		RoomX: integer;
		RoomY: integer;
		Layer: integer;
		X: integer;
		Y: integer;

		constructor Create(iKind, iRoomX, iRoomY, iLayer, iX, iY: integer);
	end;





	TPowerupList = class
	public
		CapPowerups, NumPowerups: integer;
		Powerup: array of TPowerup;

		constructor Create();
		destructor Destroy(); override;
		procedure Clear();

		procedure UpdateFromLevel(iLevel: TKSLevel);
		procedure AddPowerup(Kind, RoomX, RoomY, Layer, X, Y: integer);
	end;





function GetPowerupCaptionFromKind(iKind: integer): string;

















	
implementation

uses
	uKSObjects;






function GetPowerupCaptionFromKind(iKind: integer): string;
const
	PowerupCaptionFromKind: array[3..10] of string = (
		'Run',
		'WallClimb',
		'DblJump',
		'HighJump',
		'Eye',
		'Detector',
		'Umbrella',
		'Hologram'
	);
	KeyFromKind: array[0..3] of string = (
		'Red key',
		'Yellow key',
		'Blue key',
		'Purple key'
	);
begin
	if ((iKind >= KSOBJ_PU_RUN) and (iKind <= KSOBJ_PU_HOLOGRAM)) then
	begin
		Result := PowerupCaptionFromKind[iKind];
		Exit;
	end;
	if ((iKind >= KSOBJ_KEY_RED) and (iKind <= KSOBJ_KEY_PURPLE)) then
	begin
		Result := KeyFromKind[iKind - KSOBJ_KEY_RED];
		Exit;
	end
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TPowerup:

constructor TPowerup.Create(iKind, iRoomX, iRoomY, iLayer, iX, iY: integer);
begin
	inherited Create();
	Kind := iKind;
	RoomX := iRoomX;
	RoomY := iRoomY;
	Layer := iLayer;
	X := iX;
	Y := iY;
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TPowerupList:

constructor TPowerupList.Create();
begin
	NumPowerups := 0;
	CapPowerups := 0;
end;





destructor TPowerupList.Destroy();
begin
	Clear();
end;





procedure TPowerupList.Clear();
var
	i: integer;
begin
	for i := 0 to NumPowerups - 1 do
	begin
		Powerup[i].Free();
	end;
	NumPowerups := 0;
	CapPowerups := 0;
	SetLength(Powerup, 0);
end;





procedure TPowerupList.UpdateFromLevel(iLevel: TKSLevel);
var
	i: integer;
	lay, x, y: integer;
	Data: TKSRoomRec;
begin
	for i := 0 to NumPowerups - 1 do
	begin
		Powerup[i].Free();
	end;
	NumPowerups := 0;

	// scan level for powerups:
	for i := 0 to iLevel.NumRooms - 1 do
	begin
		// scan room:
		Data := iLevel.Room[i].Data;
		for lay := 4 to 7 do
		begin
			for y := 0 to 9 do
			begin
				for x := 0 to 24 do
				begin
					if (Data.Obj[lay].Bank[y, x] <> 0) then continue;
					case (Data.Obj[lay].Obj[y, x]) of
						KSOBJ_PU_RUN,
						KSOBJ_PU_WALLCLIMB,
						KSOBJ_PU_HIGHJUMP,
						KSOBJ_PU_DBLJUMP,
						KSOBJ_PU_EYE,
						KSOBJ_PU_UMBRELLA,
						KSOBJ_PU_DETECTOR,
						KSOBJ_PU_HOLOGRAM,
						KSOBJ_KEY_RED,
						KSOBJ_KEY_YELLOW,
						KSOBJ_KEY_BLUE,
						KSOBJ_KEY_PURPLE:
						begin
							AddPowerup(Data.Obj[lay].Obj[y, x], iLevel.Room[i].XPos, iLevel.Room[i].YPos, lay, x, y);
						end;
					end;
				end;		// for x
			end;		// for y
		end;		// for lay
	end;		// for i - rooms
end;





procedure TPowerupList.AddPowerup(Kind, RoomX, RoomY, Layer, X, Y: integer);
begin
	if (NumPowerups >= CapPowerups) then
	begin
		CapPowerups := NumPowerups + 16;
		SetLength(Powerup, CapPowerups);
	end;
	Powerup[NumPowerups] := TPowerup.Create(Kind, RoomX, RoomY, Layer, X, Y);
	NumPowerups := NumPowerups + 1;
end;





end.
