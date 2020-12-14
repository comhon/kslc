
unit uKSObjPass;

{$MODE Delphi}

interface

uses
	Classes;





type
	TPassArray = array[0..599, 0..249] of byte;
	PPassArray = ^TPassArray;




	
procedure ApplyObjPass(iArr: PPassArray; iBank, iObj: byte; x, y: integer);






















implementation

uses
	uKSRepresentations;





procedure FillPassBlock(iArr: PPassArray; x, y, t, l, b, r: integer; iVal: byte = KSPASS_IMPASSABLE);
var
	u, v: integer;
begin
	for u := l to r do
	begin
		for v := t to b do
		begin
			iArr^[x * 24 + u, y * 24 + v] := iVal;
		end;
	end;
end;





procedure FillBlockToInvWall(iArr: PPassArray; x, y, t, l, b, r: integer);
var
	u, v: integer;
begin
	for u := l to r do
	begin
		for v := t to b do
		begin
			if (iArr^[x * 24 + u, y * 24 + v] = KSPASS_PASSABLE) then
			begin
				iArr^[x * 24 + u, y * 24 + v] := KSPASS_INVWALL;
			end;
		end;
	end;
end;





procedure FillBlockToInvHole(iArr: PPassArray; x, y, t, l, b, r: integer);
var
	u, v: integer;
begin
	for u := l to r do
	begin
		for v := t to b do
		begin
			if (iArr^[x * 24 + u, y * 24 + v] = KSPASS_IMPASSABLE) then
			begin
				iArr^[x * 24 + u, y * 24 + v] := KSPASS_INVHOLE;
			end;
		end;
	end;
end;





procedure ClearPassBlock(iArr: PPassArray; x, y, t, l, b, r: integer);
var
	u, v: integer;
begin
	for u := l to r do
	begin
		for v := t to b do
		begin
			iArr^[x * 24 + u, y * 24 + v] := 0;
		end;
	end;
end;





procedure ApplyObjPass(iArr: PPassArray; iBank, iObj: byte; x, y: integer);
var
	i: integer;
begin
	case iBank of
		1:		// liquids
		begin
			case iObj of
				1, 2:   FillPassBlock(iArr, x, y, 11, 0, 23, 23);
				5:      FillPassBlock(iArr, x, y, 9,  0, 23, 23);
				10, 12: FillPassBlock(iArr, x, y, 6,  0, 23, 23);
				14:     FillPassBlock(iArr, x, y, 10, 0, 23, 23);
				19, 22: FillPassBlock(iArr, x, y, 17, 0, 23, 23);
				7, 8, 9, 11, 17, 21, 24: FillPassBlock(iArr, x, y, 0, 0, 23, 23);
			end;
		end;

		6:		// traps:
		begin
			case iObj of
				10: FillPassBlock(iArr, x, y, 22, 0, 23, 23);
				11: FillPassBlock(iArr, x, y, 0, 0, 1, 23);
				12: FillPassBlock(iArr, x, y, 0, 22, 23, 23);
				13: FillPassBlock(iArr, x, y, 0, 0, 23, 1);
			end;
		end;

		12:		// ghosts
		begin
			case iObj of
				5:  FillBlockToInvWall(iArr, x, y, 0, 0, 23, 23);		// inv wall
				17: FillBlockToInvHole(iArr, x, y, 0, 0, 23, 23);		// inv hole
			end;
		end;

		15:		// objects and areas
		begin
			case iObj of
				1, 2, 3, 4, 6: FillPassBlock(iArr, x, y, 0, 0, 23, 23);
			end;
		end;

		16:		// invisible
		begin
			case iObj of
				2:  FillPassBlock(iArr, x, y, 0, 0, 5, 23);		// kill top
				3:  FillPassBlock(iArr, x, y, 18, 0, 23, 23);		// kill bottom
				4:  FillPassBlock(iArr, x, y, 0, 18, 23, 23);		// kill right
				5:  FillPassBlock(iArr, x, y, 0, 0, 23, 5);		// kill left
				6:  FillPassBlock(iArr, x, y, 0, 0, 23, 23);		// kill square
				7:  FillPassBlock(iArr, x, y, 9, 0, 14, 23);		// kill mid horz
				8:  FillPassBlock(iArr, x, y, 0, 9, 23, 14);		// kill mid vert
				9:  FillPassBlock(iArr, x, y, 9, 9, 14, 23);		// kill mid right
				10: FillPassBlock(iArr, x, y, 0, 9, 14, 14);		// kill mid top
				11: FillPassBlock(iArr, x, y, 9, 0, 14, 14);		// kill mid left
				12: FillPassBlock(iArr, x, y, 9, 9, 23, 14);		// kill mid bottom

				13,
				14: FillPassBlock(iArr, x, y, 0, 0, 23, 23);
				15:
				begin
					// triangle bottom left
					for i := 0 to 23 do
					begin
						FillPassBlock(iArr, x, y, i, 0, i, i);
					end;
				end;
				16:
				begin
					// triangle bottom right
					for i := 0 to 23 do
					begin
						FillPassBlock(iArr, x, y, i, 23 - i, i, 23);
					end;
				end;
			end;
		end;
	end;
end;





end.
