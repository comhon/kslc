
unit uKSRepresentations;

{$MODE Delphi}

// room: 600 x 240 = (25 x 10) x (24 x 24)
// tiles: 24 x 24
// tileset: 16 x 8 = 384 x 192 px

interface

uses
	Windows,
	Classes,
	SysUtils,
	Graphics,
	gzio, //* Included Lazarus library for gzip
	uMultiEvent,
	uKSObjects,
	uKSObjPass,
	uKSLog,
	IntfGraphics,
	uKSRender;





const
	ROOM_WIDTH= 600;
	ROOM_HEIGHT = 240;
	WALLSWIM_HEIGHT = 4;
	WALLSWIM_WIDTH  = 4;
	MIN_HEIGHT      = 17;
	MIN_WIDTH       = 11;
	MIN_EDGE_WIDTH  = 6;		// can fall through a 6px hole at the edge
	MIN_EDGE_HEIGHT = 7;		// can walk through a 7px hole at the top without bumping into next screen
	WALKOVER_HEIGHT = 3;		// can walk over 3 px difference in height
	CLIMBOVER_WIDTH = 0;		// cannot climb when there's even a 1 px difference
	CONST_HEADER_UNKNOWNCHUNK = #190#11#0#0;		// $be $0b $00 $00

	(*
	When walking horz to next screen Juni can wallswim within up to 6 px wide hole, or even 7 px wide hole with run powerup
	Even 1px dot can stop the walk and can be climbed.
	A platform has may be at most 12 px above Juni for climbing, 13 px won't climb. 
	*)

	KSPASS_IMPASSABLE = 255;
	KSPASS_PASSABLE   = 0;
	KSPASS_INVWALL    = 192;
	KSPASS_INVHOLE    = 64;





type
	// fwd:
	TKSShiftList = class;
	TKSLevel = class;
	TKSRoom = class;
	TKSTileset = class;
	PKSRoomRec = ^TKSRoomRec;
	PKSObjLayer = ^TKSObjLayer;
	PKSTileLayer = ^TKSTileLayer;




	TKSProgressEvent = procedure(Sender: TObject; iCurrent, iMax: integer; iMessage: string) of object;


	


	TKSTileLayer = record
		Tile: array[0..9, 0..24] of byte;
	end;





	TKSObjLayer = record
		Obj: array[0..9, 0..24] of byte;
		Bank: array[0..9, 0..24] of byte;
	end;





	TKSRoomRec = record
		Tile: array[0..3] of TKSTileLayer;
		Obj:  array[4..7] of TKSObjLayer;
		TilesetA: byte;
		TilesetB: byte;
		AtmosA: byte;
		AtmosB: byte;
		Music: byte;
		Background: byte;
	end;		// 3006 bytes





{ TKSTileset }

 TKSTileset = class
	public
		Number: integer;
		NeedsLoading: boolean;
		IsLoaded: boolean;
		Img: TPortableNetworkGraphic;
		ImgLaz: TLazIntfImage;
		IsMaskCreated: boolean;

		Log: TKSLog;
		TileCache: array[0..15,0..8] of TLazIntfImage;

		// TODO: transparency preparations?

		constructor Create(iNumber: integer; iLog: TKSLog);
		destructor Destroy(); override;
		procedure Clear();

		procedure Load(iLevelDir: string);

		procedure NeedMask();
		function GetTile(ix,iy: integer): TLazIntfImage;
	end;





	TKSRoom = class
	public
		Parent: TKSLevel;
		XPos: integer;
		YPos: integer;
		Data: TKSRoomRec;
		Tileset: array[0..1] of TKSTileset;

		Passable: PPassArray;		// KSPASS_XXX

		// TODO: some parameters, such as wraps, ... (?)
		EventParams: TStringList;

		RoomLeft:  TKSRoom;
		RoomRight: TKSRoom;
		RoomUp:    TKSRoom;
		RoomDown:  TKSRoom;

		TagInt: integer;		// for various algorithms that need to store an integer value with each room
		TagDbl: double;		// for various algorithms that need to store a double value with each room

		// Warp parameters: (updated by UpdateFromData())
		WarpXL, WarpYL, WarpXR, WarpYR, WarpXU, WarpYU, WarpXD, WarpYD: integer;
		HasWarpL, HasWarpR, HasWarpU, HasWarpD: boolean;

		constructor Create(iParent: TKSLevel);
		destructor  Destroy(); override;
		procedure   Clear();

		procedure   UpdateFromData();

		function DoesContainObject(iBank: byte; iObj: byte): boolean;

		procedure UpdatePassable();
		procedure FreePassable();

		function  GetEventParams(): TStringList;
		procedure SetEventParams(iParams: TStrings);
		procedure CopyEventParamsFrom(iRoom: TKSRoom);
		function  ParseShift(var oRoomX, oRoomY, oX, oY: integer; var oHasRoomX, oHasRoomY, oHasX, oHasY, oHasAbsolute: boolean; iKind: Char): boolean;
		function  HasShiftInLayers(iKind: char): boolean;
		procedure ModifyShiftDest(iShift: char; iRoomX, iRoomY: integer);
		procedure ParseWarps(var oHasWarpL, oHasWarpR, oHasWarpU, oHasWarpD: boolean; var oWarpXL, oWarpYL, oWarpXR, oWarpYR, oWarpXU, oWarpYU, oWarpXD, oWarpYD: integer);
		procedure SetWarpsToParams();
		procedure SetWarpToParams(iKind: char; iHas: boolean; iX, iY: integer);
	end;





	TKSLevel = class
	public
		ShiftList: TKSShiftList;

		FileName: string;
		LevelDir: string;

		NumRooms: integer;
		CapRooms: integer;
		Room: array of TKSRoom;

		Tileset: array[0..255] of TKSTileset;
		Background: array[0..255] of TPortableNetworkGraphic;
		BackgroundIntF: array[0..255] of TLazIntfImage;
		BackgroundFillIntf: array[0..255] of TLazIntfImage;

		StartRoomX, StartRoomY: integer;
		StartX, StartY: integer;
		StartPower: array[0..11] of boolean;

		EventParams: TStringList;
		WorldParams: TStringList;
		CutsceneMusicParams: TStringList;
		OtherParams: TStringList;

		LevelName: string;
		AuthorName: string;

		Log: TKSLog;
		OnProgress: TKSProgressEvent;

		IsModified: boolean;
		ChangedListeners: TMultiEvent;

		constructor Create(iLog: TKSLog);
		destructor Destroy(); override;
		procedure Clear();

		procedure LoadFromFile(iFileName: string);
		procedure LoadTiles();
		procedure LoadEventParams();
		procedure LoadTilesets();
		procedure LoadTileset(iNumber: integer);
		procedure LoadBackgrounds();
		procedure LoadBackground(iNumber: integer);
		procedure LoadDefaultSavegame();

		procedure SaveToFile(iFileName: string);
		procedure SaveTiles();
		procedure SaveEventParams();
		procedure SaveDefaultSavegame();

		function  AddRoom(x, y: integer; const iRoomRec: PKSRoomRec): TKSRoom;
		function  GetRoom(x, y: integer): TKSRoom;

		procedure ParseWorldParams();

		procedure Changed();		// notifies all listeners that the level has changed
	end;





	TKSObject = class
		Bank: byte;
		ID: byte;
		Description: byte;
		Img: TPortableNetworkGraphic;
		IntfImg: TLazIntfImage;

		constructor Create(iBank: byte; iID: byte);
		destructor Destroy(); override;

		procedure NeedImage();
	end;





	TKSObjectCollection = class
	public
		CapObjs, NumObjs: integer;
		Obj: array of TKSObject;

		constructor Create();
		destructor Destroy(); override;
		procedure Clear();

		function GetObject(iBank, iObj: byte): TKSObject;
	end;





	TKSShift = class
	public
		FromRoomX, FromRoomY: integer;
		FromX, FromY: integer;
		ToRoomX, ToRoomY: integer;
		ToX, ToY: integer;
		Kind: Char;		// A / B / C

		constructor Create(iFromRoomX, iFromRoomY, iFromX, iFromY, iToRoomX, iToRoomY, iToX, iToY: integer; iKind: Char);
	end;


	TKSShiftList = class
		CapShifts, NumShifts: integer;
		Shift: array of TKSShift;

		constructor Create();
		destructor Destroy(); override;
		procedure Clear();

		function ExtractSingleFromRoom(iX, iY: integer): TKSShiftList;
		function ExtractSingleToRoom  (iX, iY: integer): TKSShiftList;

		procedure UpdateFromLevel(iLevel: TKSLevel);

	protected
		procedure AddSingleRoom(iRoom: TKSRoom);
		procedure AddShift(iFromRoomX, iFromRoomY, iFromX, iFromY, iToRoomX, iToRoomY, iToX, iToY: integer; iKind: Char);
		procedure ParseShift(iRoom: TKSRoom; iX, iY: integer; iKind: Char);
	end;





var
	gKSObjects: TKSObjectCollection;
	gKSDir: string;


















implementation

//Previously used unit zlib1dll that was not included in the original source replaced with Lazarus implementation





function BoolToVal(iBool: boolean): string;
begin
	if (iBool) then
		Result := '1'
	else
		Result := '0';
end;





function ParsePosData(data: PChar; var XPos, YPos: integer): integer;		// returns the position of the #0 char
var
	i: integer;
	cval: integer;
begin
	cval := 0;
	for i := 0 to 10 do
	begin
		case data[i] of
			'0'..'9':
			begin
				cval := cval * 10 + ord(data[i]) - ord('0');
			end;
			'x': ;
			'y', 'Y':
			begin
				XPos := cval;
				cval := 0;
			end;
			#0:
			begin
				YPos := cval;
				Result := i;
				Exit;
			end;
		end;
	end;
	YPos := cval;
	Result := -1;
end;





procedure ReadHeader(gzf: gzFile; var XPos, YPos: integer); overload;
var
	a: char;
	poz: integer;
	cval: integer;
	cvalneg: boolean;
	dummy: array[0..3] of char;
begin
	poz := 0;
	cval := 0;
	cvalneg := false;
	while (true) do
	begin
		a := chr(gzGetc(gzf));
		case a of
			'x':
			begin
				if (poz <> 0) then
				begin
					raise Exception.Create('Invalid header, x coord at an unexpected place ("' + IntToStr(poz) + '")');
				end;
			end;		// 'x'

			'y':
			begin
				if (cvalneg) then
					XPos := -cval
				else
					XPos := cval;
				cval := 0;
				cvalneg := false;
			end;		// 'y'

			'0'..'9':
			begin
				cval := cval * 10 + ord(a) - ord('0');
			end;

			'-':
			begin
				cvalneg := true;
				cval := 0;
			end;

			#0:
			begin
				if (cvalneg) then
					YPos := -cval
				else
					YPos := cval;
				break;
			end;		// #0
		end;		// case ReadChar()
		poz := poz + 1;
	end;		// while (true)

	gzRead(gzf, @dummy, 4);
	// TODO: check whether dummy == CONST_HEADER_UNKNOWNCHUNK
end;





(*
// ZLibEx doesn't work, it complains about "data error"
procedure ReadHeader(gzs: TZDecompressionStream; var XPos, YPos: integer); overload;
var
	a: char;
	poz: integer;
	cval: integer;
	cvalneg: boolean;
	dummy: array[0..3] of char;
begin
	poz := 0;
	cval := 0;
	cvalneg := false;
	while (true) do
	begin
		gzs.Read(a, 1);
		case a of
			'x':
			begin
				if (poz <> 0) then
				begin
					raise Exception.Create('Invalid header, x coord at an unexpected place ("' + IntToStr(poz) + '")');
				end;
			end;		// 'x'

			'y':
			begin
				if (cvalneg) then
					XPos := -cval
				else
					XPos := cval;
				cval := 0;
				cvalneg := false;
			end;		// 'y'

			'0'..'9':
			begin
				cval := cval * 10 + ord(a) - ord('0');
			end;

			'-':
			begin
				cvalneg := true;
				cval := 0;
			end;

			#0:
			begin
				if (cvalneg) then
					YPos := -cval
				else
					YPos := cval;
				break;
			end;		// #0
		end;		// case ReadChar()
		poz := poz + 1;
	end;		// while (true)

	gzs.Read(dummy, 4);
	// TODO: check whether dummy == CONST_HEADER_UNKNOWNCHUNK
end;
*)





//Function used to remove pink background from non-alpha png images
procedure CreatePNGAlpha(Img: TPortableNetworkGraphic);
var
	x, y: integer;
	pngal: PByteArray;
begin
        Exit; //*h CreatePNGAlpha commented out, not yet copatible with TPortableNetworkGraphic 

        (*
	case Img.Header.ColorType of
		COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA: ;		// nothing needed
		COLOR_RGB, COLOR_PALETTE:
		begin
			Img.CreateAlpha();
			if not(Assigned(Img.AlphaScanline[0])) then
			begin
				Exit;
			end;
			for y := 0 to Img.Height - 1 do
			begin
				pngal := Img.AlphaScanline[y];
				for x := 0 to Img.Width - 1 do
				begin
					if (Img.Pixels[x, y] = $ff00ff) then
					begin
						pngal[x] := 0;
					end
					else
					begin
						pngal[x] := 255;
					end;
				end;
			end;		// for y
		end;
	end;		// case ColorType of
        *)
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSTileset:

constructor TKSTileset.Create(iNumber: integer; iLog: TKSLog);
begin
	inherited Create();
	Log := iLog;
	Number := iNumber;
	IsLoaded := false;
	NeedsLoading := false;
	Img := nil;
	IsMaskCreated := false;
	Clear();
end;





destructor TKSTileset.Destroy();
begin
	Clear();
	inherited Destroy();
end;





procedure TKSTileset.Clear();
var
	i,j: byte;
begin
	for i:=0 to 15 do
	begin
		for j:=0 to 7 do
		begin
			if Assigned(TileCache[i,j]) then TileCache[i,j].Free;
			TileCache[i,j]:=nil;
		end;
	end;

	if (IsLoaded) then
	begin
		Img.Free();
		Img := nil;
		IsLoaded := false;
	end;
end;





procedure TKSTileset.Load(iLevelDir: string);
var
	fnam: string;
begin
	Img := TPortableNetworkGraphic.Create();
	ImgLaz:=TLazIntfImage.Create(0,0);
	fnam := iLevelDir + 'Tilesets\Tileset' + IntToStr(Number) + '.png';
	if not(FileExists(fnam)) then
	begin
		fnam := gKSDir + 'Data\Tilesets\Tileset' + IntToStr(Number) + '.png';
	end;
	if Assigned(Log) then Log.Log(LOG_INFO, 'Loading tileset #' + IntToStr(Number) + ' from file "' + fnam + '"');
	try
		Img.LoadFromFile(fnam);
		ImgLaz:=CreateIntfImage(Img);
	except
		on e: Exception do
		begin
			if Assigned(Log) then
			begin
				Log.Log(LOG_ERROR, 'Cannot load tileset #' + IntToStr(Number) + ' from file "' + fnam + '", the following exception occurred: "' + e.Message + '"');
			end;
			raise;
		end;
	end;
	IsLoaded := True;
	try
		NeedMask();
	except
	end;
end;





procedure TKSTileset.NeedMask();
var
	x, y: integer;
	pngal: PByteArray;
begin
        Exit; //*h NeedMask function commented out, not yet copatible with TPortableNetworkGraphic
        (*
        if (IsMaskCreated) then Exit;
	if not(IsLoaded) then Exit;
	CreatePNGAlpha(Img);

	// force the zero-th tile full transparent!
	for y := 0 to 23 do
	begin
		pngal := Img.AlphaScanline[y];
		if not(Assigned(pngal)) then
		begin
			if Assigned(Log) then
			begin
				Log.Log(LOG_ERROR, 'Cannot load tileset #' + IntToStr(Self.Number) + ' because it is not a transparent-able PNG format.');
				Exit;
			end;
		end;
		for x := 0 to 23 do
		begin
			pngal[x] := 0;
		end;		// for x
	end;		// for y
        *)

	IsMaskCreated := True;
end;

function TKSTileset.GetTile(ix, iy: integer): TLazIntfImage;
var
	tile: TLazIntfImage;
	tx,ty: integer;
begin
	tx:= ix*24;
	ty:= iy*24;
	tile:=self.TileCache[ix,iy];
	if tile = nil then
	begin
		tile:=CreateRegion(self.ImgLaz,TRect.Create(tx,ty,tx+24,ty+24));
		self.TileCache[ix,iy]:=tile;
	end;
	result:=tile;
end;




///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSRoom:

constructor TKSRoom.Create(iParent: TKSLevel);
begin
	Parent := iParent;
	Tileset[0] := nil;
	Tileset[1] := nil;
	EventParams := TStringList.Create();
	Passable := nil;
end;





destructor  TKSRoom.Destroy();
begin
	Clear();
	inherited Destroy();
end;





procedure TKSRoom.Clear();
begin
	// nothing needed yet
	EventParams.Free();
	EventParams := nil;
	FreePassable();
end;





procedure TKSRoom.UpdateFromData();
begin
	Tileset[0] := Parent.Tileset[Data.TilesetA];
	Tileset[1] := Parent.Tileset[Data.TilesetB];

	// Process possible warps into RoomLeft, RoomRight, ...
	ParseWarps(HasWarpL, HasWarpR, HasWarpU, HasWarpD, WarpXL, WarpYL, WarpXR, WarpYR, WarpXU, WarpYU, WarpXD, WarpYD);
	RoomLeft  := Parent.GetRoom(XPos + WarpXL - 1, YPos + WarpYL);
	RoomRight := Parent.GetRoom(XPos + WarpXR + 1, YPos + WarpYR);
	RoomUp    := Parent.GetRoom(XPos + WarpXU,     YPos + WarpYU - 1);
	RoomDown  := Parent.GetRoom(XPos + WarpXD,     YPos + WarpYD + 1);
end;





function TKSRoom.DoesContainObject(iBank: byte; iObj: byte): boolean;
var
	lay, x, y: integer;
begin
	for lay := 4 to 7 do
	begin
		for y := 0 to 9 do
		begin
			for x := 0 to 24 do
			begin
				if ((Data.Obj[lay].Bank[y, x] = iBank) and (Data.Obj[lay].Obj[y, x] = iObj)) then
				begin
					Result := true;
					Exit;
				end;
			end;		// for x
		end;		// for y
	end;		// for lay
	Result := false;
end;




//*h Passables not yet compatible with new used structures, temporarily commented out, this might break wallswim detection and 3rd layer grass
procedure TKSRoom.UpdatePassable();
var
	lay, x, y, u, v: integer;		// iterators
	ts, tx, ty: integer;
	pngal: PByteArray;
begin
	if Assigned(Passable) then Exit;
	New(Passable);

	// first from L3 using the tileset data:
	for x := 0 to 24 do
	begin
		for y := 0 to 9 do
		begin
			ts := (Data.Tile[3].Tile[y, x] and $80) shr 7;
			tx := ((Data.Tile[3].Tile[y, x] and $7f) mod 16) * 24;
			ty := ((Data.Tile[3].Tile[y, x] and $7f) div 16) * 24;
			for v := 0 to 23 do
			begin
                                //*h Needs reimplementation
                                (*
                                pngal := Tileset[ts].Img.AlphaScanline[ty + v];
				for u := 0 to 23 do
				begin
					Passable[x * 24 + u, y * 24 + v] := pngal[tx + u];
				end;		// for u
                                *)

			end;		// for v
		end;		// for y
	end;		// for x

	// now modify by objects:
	for lay := 4 to 7 do
	begin
		for x := 0 to 24 do
		begin
			for y := 0 to 9 do
			begin
				if ((Data.Obj[lay].Bank[y, x] = 0) and (Data.Obj[lay].Obj[y, x] = 0)) then continue;
				// there is an object here, try to find its mask:
				ApplyObjPass(Passable, Data.Obj[lay].Bank[y, x], Data.Obj[lay].Obj[y, x], x, y);				
			end;		// for y
		end;		// for x
	end;		// for lay
end;





procedure TKSRoom.FreePassable();
begin
	if not(Assigned(Passable)) then Exit;
	Dispose(Passable);
	Passable := nil;
end;





function TKSRoom.GetEventParams(): TStringList;
begin
	// This function composes all events parameters into the World.ini block format.
	// It will become more complicated when events are parsed and possibly stored otherwise than a simple EventParams StringList

	if (EventParams.Count <= 0) then
	begin
		Result := nil;
		Exit;
	end;
	Result := TStringList.Create();
	Result.AddStrings(EventParams);
end;





procedure TKSRoom.SetEventParams(iParams: TStrings);
begin
	// This function takes the World.ini block format iParams and parses it into internal vars.
	// It will become more complicated when events are parsed and possibly stored otherwise than a simple EventParams Stringlist
	EventParams.Assign(iParams);
	UpdateFromData();
end;





procedure TKSRoom.CopyEventParamsFrom(iRoom: TKSRoom);
begin
	EventParams.Assign(iRoom.EventParams);
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSLevel:

constructor TKSLevel.Create(iLog: TKSLog);
var
	i: integer;
begin
	inherited Create();
	Log := iLog;
	NumRooms := 0;
	CapRooms := 0;
	EventParams := TStringList.Create();
	WorldParams := TStringList.Create();
	CutsceneMusicParams := TStringList.Create();
	OtherParams := TStringList.Create();
	ShiftList := TKSShiftList.Create();
	ChangedListeners := TMultiEvent.Create();
	IsModified := false;
	if Assigned(Log) then Log.Log(LOG_INFO, 'TKSLevel.Create(): creating tileset holders');
	for i := 0 to 255 do
	begin
		Tileset[i] := TKSTileset.Create(i, iLog);
		Background[i] := TPortableNetworkGraphic.Create();
	end;
end;





destructor TKSLevel.Destroy();
var
	i: integer;
begin
	Clear();
	ChangedListeners.Free();
	ShiftList.Free();
	OtherParams.Free();
	OtherParams := nil;
	CutsceneMusicParams.Free();
	CutsceneMusicParams := nil;
	WorldParams.Free();
	WorldParams := nil;
	EventParams.Free();
	EventParams := nil;
	for i := 0 to 255 do
	begin
		Background[i].Free();
		BackgroundIntf[i].Free();
		BackgroundFillIntf[i].Free();
		Tileset[i].Free();
	end;
	inherited Destroy();
end;





procedure TKSLevel.Clear();
var
	i: integer;
begin
	for i := 0 to NumRooms - 1 do
	begin
		Room[i].Free();
	end;
	NumRooms := 0;
	CapRooms := 0;
	SetLength(Room, 0);
	for i := 0 to 255 do
	begin
		Background[i].Free();
		Tileset[i].Free();
		Tileset[i] := TKSTileset.Create(i, Log);
		Background[i] := TPortableNetworkGraphic.Create();
	end;
end;





procedure TKSLevel.LoadFromFile(iFileName: string);
var
	i: integer;
begin
	FileName := iFileName;
	LevelDir := ExtractFilePath(FileName);
	if (LevelDir <> '') then
	begin
		if (LevelDir[Length(LevelDir)] <> '\') then LevelDir := LevelDir + '\';
	end;

	if Assigned(Log) then
	begin
		Log.Log(LOG_INFO, 'TKSLevel.LoadFromFile("' + FileName + '"):');
		Log.AddIndent();
	end;

	try
		if Assigned(OnProgress) then OnProgress(Self, 0, 6, 'Loading tiles');
		LoadTiles();
		if Assigned(OnProgress) then OnProgress(Self, 1, 6, 'Loading event params');
		LoadEventParams();
		if Assigned(OnProgress) then OnProgress(Self, 2, 6, 'Loading tilesets');
		LoadTilesets();
		if Assigned(OnProgress) then OnProgress(Self, 3, 6, 'Loading backgrounds');
		LoadBackgrounds();
		if Assigned(OnProgress) then OnProgress(Self, 4, 6, 'Loading default savegame');
		LoadDefaultSavegame();
		if Assigned(OnProgress) then OnProgress(Self, 5, 6, 'Organizing data');

		if Assigned(Log) then Log.Log(LOG_INFO, 'TKSLevel.LoadFromFile(): updating rooms from data...');
		for i := 0 to NumRooms - 1 do
		begin
			Room[i].UpdateFromData();
		end;
		ShiftList.UpdateFromLevel(Self);
	finally
		if Assigned(Log) then
		begin
			Log.DelIndent();
			Log.Log(LOG_INFO, 'TKSLevel.LoadFromFile() complete.');
		end;
	end;
end;





procedure TKSLevel.LoadTiles();
var
	gzf: gzFile;
	rr: TKSRoomRec;
	xpos, ypos: integer;
	// fs: TFileStream;
	// gzs: TZDecompressionStream;
begin
	if Assigned(Log) then Log.Log(LOG_INFO, 'TKSLevel.LoadTiles()...');
	gzf := gzOpen(PChar(FileName), 'r');
	if (gzf = nil) then
	begin
		if Assigned(Log) then Log.Log(LOG_ERROR, 'Cannot open level tile file "' + FileName + '"!');
		raise Exception.Create('Cannot open level tile file "' + FileName + '"!');
	end;
	try
		while (not gzeof(gzf)) do
		begin
			ReadHeader(gzf, xpos, ypos);
			if (gzRead(gzf, @rr, sizeof(rr)) <> sizeof(rr)) then break;
			AddRoom(xpos, ypos, @rr);
		end;
	finally
		gzClose(gzf);
	end;

	(*
	// ZLibEx doesn't work, it complains about "data error"
	fs := TFileStream.Create(FileName, fmOpenRead);
	try
		gzs := TZDecompressionStream.Create(fs);
		try
			repeat
				ReadHeader(gzs, xpos, ypos);
				if (gzs.Read(rr, sizeof(rr)) <> sizeof(rr)) then break;
				AddRoom(xpos, ypos, @rr);
			until false;
		finally
			gzs.Free();
		end;
	finally
		fs.Free();
	end;
	*)
end;





procedure TKSLevel.LoadEventParams();
type
	TWorldHeaderType = (whtNone, whtWorld, whtCutsceneMusic, whtRoom, whtOther);
var
	CurrentRoom: TKSRoom;
	i: integer;
	txt: string;
	CurrentHeader: TWorldHeaderType;
	xpos, ypos: integer;
begin
	if Assigned(Log) then Log.Log(LOG_INFO, 'TKSLevel.LoadEventParams()...');

	// load from INI:
	try
		EventParams.LoadFromFile(LevelDir + 'World.ini');
	except
		on e: Exception do
		begin
			if Assigned(Log) then Log.Log(LOG_ERROR, '!!! Exception received: "' + e.Message + '" !!!');
			raise;
		end;
	end;

	// parse into globals and per-room:
	CurrentRoom := nil;
	CurrentHeader := whtNone;
	for i := 0 to EventParams.Count - 1 do
	begin
		txt := trim(EventParams[i]);
		if (txt = '') then continue;		// no need to parse empty lines
		if (txt[1] = '[') then
		begin
			// header line
			if (AnsiCompareText(txt, '[World]') = 0) then
			begin
				CurrentHeader := whtWorld;
			end
			else if (AnsiCompareText(txt, '[Cutscene Music]') = 0) then
			begin
				CurrentHeader := whtCutsceneMusic;
			end
			else if ((txt[2] = 'x') and (pos('y', txt) > 0)) then
			begin
				txt := Trim(Copy(txt, 2, Length(txt)));
				if (Length(txt) < 4) then continue;		// line too short to contain a position
				if (txt[Length(txt)] = ']') then txt := trim(Copy(txt, 1, Length(txt) - 1));		// strip the trailing bracket with whitespace
				if ((txt[1] <> 'x') and (txt[1] <> 'X')) then continue;		// not a position
				txt := txt + #0;
				if (ParsePosData(@(txt[1]), xpos, ypos) < 0) then continue;
				CurrentRoom := GetRoom(xpos, ypos);
				CurrentHeader := whtRoom;
			end
			else
			begin
				CurrentHeader := whtOther;
				OtherParams.Add(txt);
			end
		end
		else if (txt[1] = ';') then
		begin
			// ignore comments
			// Don't even save them into OtherParams, since this would move the comments all out to the end of the file
			// OtherParams.Add(txt);
		end
		else
		begin
			// data line
			case CurrentHeader of
				whtNone: ;		// ignore text outside header
				whtWorld:
				begin
					WorldParams.Add(txt);
				end;
				whtCutsceneMusic:
				begin
					CutsceneMusicParams.Add(txt);
				end;
				whtRoom:
				begin
					// distribute among rooms:
					if not(Assigned(CurrentRoom)) then continue;
					CurrentRoom.EventParams.Add(txt);
				end;
				whtOther:
				begin
					OtherParams.Add(txt);
				end;
			end;		// case CurrentHeader
		end;		// data line
	end;		// for i - lines
	Eventparams.Clear();
	ParseWorldParams();
end;





procedure TKSLevel.LoadTilesets();
var
	i: integer;
begin
	if Assigned(Log) then
	begin
		Log.Log(LOG_INFO, 'TKSLevel.LoadTilesets()...');
		Log.AddIndent();
	end;

	try
		// clear previous data:
		for i := 0 to 255 do
		begin
			Tileset[i].Clear();
			Tileset[i].NeedsLoading := false;
		end;

		// parse needed tilesets:
		for i := 0 to NumRooms - 1 do
		begin
			Tileset[Room[i].Data.TilesetA].NeedsLoading := true;
			Tileset[Room[i].Data.TilesetB].NeedsLoading := true;
		end;

		// load only the needed tilesets:
		for i := 0 to 255 do
		begin
			if (Tileset[i].NeedsLoading) then
			begin
				Tileset[i].Load(LevelDir);
			end;
		end;
	finally
		if Assigned(Log) then
		begin
			Log.DelIndent();
		end;
	end;
end;





procedure TKSLevel.LoadTileset(iNumber: integer);
begin
	Tileset[iNumber].NeedsLoading := true;
	Tileset[iNumber].Load(LevelDir);
end;





procedure TKSLevel.LoadBackgrounds();
var
	i: integer;
	bkgUsed: array[0..255] of boolean;
begin
	if Assigned(Log) then
	begin
		Log.Log(LOG_INFO, 'TKSLevel.LoadBackgrounds()...');
		Log.AddIndent();
	end;
	try
		for i := 0 to 255 do
		begin
			bkgUsed[i] := false;
		end;

		for i := 0 to NumRooms - 1 do
		begin
			bkgUsed[Room[i].Data.Background] := true;
		end;

		for i := 0 to 255 do
		begin
			if not(bkgUsed[i]) then continue;
			LoadBackground(i);
		end;
	finally
		if Assigned(Log) then
		begin
			Log.DelIndent();
		end;
	end;
end;





procedure TKSLevel.LoadBackground(iNumber: integer);
var
	fnam: string;
begin
	fnam := LevelDir + 'Gradients\Gradient' + IntToStr(iNumber) + '.png';
	if not(FileExists(fnam)) then
	begin
		fnam := gKSDir + 'Data\Gradients\Gradient' + IntToStr(iNumber) + '.png';
	end;
	if Assigned(Log) then Log.Log(LOG_INFO, 'Loading background #' + IntToStr(iNumber) + ' from file "' + fnam + '"');
	try
		Background[iNumber].LoadFromFile(fnam);
		BackgroundIntf[iNumber]:=Background[iNumber].CreateIntfImage;
	except
		on e: Exception do
		begin
			if Assigned(Log) then
			begin
				Log.Log(LOG_ERROR, 'Cannot load background #' + IntToStr(iNumber) + ' from file "' + fnam + '", the following exception occurred: "' + e.Message + '"');
			end;
			raise;
		end;
	end;
end;





procedure TKSLevel.LoadDefaultSavegame();
var
	fnam: string;
	dsg: TStringList;
	Code: integer;
	txt: string;
	Section, i, idx, v, strt: integer;
begin
	if Assigned(Log) then
	begin
		Log.Log(LOG_INFO, 'TKSLevel.LoadDefaultSavegame()');
		Log.AddIndent();
	end;
	try
		fnam := LevelDir + 'DefaultSavegame.ini';
		if not(FileExists(fnam)) then
		begin
			if Assigned(Log) then
			begin
				Log.Log(LOG_WARNING, 'Warning: DefaultSavegame.ini not found at expected path "' + fnam + '"');
			end;
			Exit;
		end;
		dsg := TStringList.Create();
		try
			dsg.LoadFromFile(fnam);

			// Parse the contents:
			Section := 0;
			for i := 0 to dsg.Count - 1 do
			begin
				txt := AnsiLowercase(dsg[i]);
				if (txt = '[positions]') then
				begin
					Section := 1;
					continue;
				end;
				if (txt = '[powers]') then
				begin
					Section := 2;
					continue;
				end;
				if (Section = 1) then
				begin
					if (Pos('x map=', txt) = 1) then begin Val(trim(Copy(txt, 7, Length(txt))), StartRoomX, Code); continue; end;
					if (Pos('y map=', txt) = 1) then begin Val(trim(Copy(txt, 7, Length(txt))), StartRoomY, Code); continue; end;
					if (Pos('x pos=', txt) = 1) then begin Val(trim(Copy(txt, 7, Length(txt))), StartX,     Code); continue; end;
					if (Pos('y pos=', txt) = 1) then begin Val(trim(Copy(txt, 7, Length(txt))), StartY,     Code); continue; end;
				end;
				if (Section = 2) then
				begin
					if (Pos('power', txt) = 1) then
					begin
						if (Length(txt) < 7) then continue;
						idx := 0;
						strt := 8;
						case txt[6] of
							'1':
							begin
								case txt[7] of
									'0'..'9':
									begin
										idx := 10 + ord(txt[7]) - ord('0');
										strt := 9;
									end;
									'=':
									begin
										idx := 1;
									end;
								end;
							end;
							'0', '2'..'9':
							begin
								idx := ord(txt[6]) - ord('0');
							end;
						end;
						Val(Trim(Copy(txt, strt, Length(txt))), v, Code);
						StartPower[idx] := (v <> 0);
						continue;
					end;
				end;
			end;
		finally
			dsg.Free();
		end;
	finally
		if Assigned(Log) then
		begin
			Log.DelIndent();
		end;
	end;
end;





procedure TKSLevel.SaveToFile(iFileName: string);
begin
	FileName := iFileName;

	if Assigned(Log) then
	begin
		Log.Log(LOG_INFO, 'TKSLevel.SaveToFile("' + FileName + '"):');
		Log.AddIndent();
	end;	
	try
		SaveTiles();
		SaveEventParams();
		SaveDefaultSavegame();
		IsModified := false;
	finally
		if Assigned(Log) then
		begin
			Log.DelIndent();
			Log.Log(LOG_INFO, 'TKSLevel.SaveToFile() complete.');
		end;
	end;
end;

//Lazarus gzip library does not contain wrappers to write string values directly, added one
procedure gzWriteString(igzf: gzFile; istr: string);
var
        i: integer;
begin
        for i := 1 to length(istr) do
        begin
                gzputc(igzf,istr[i]);
        end;
end;




procedure TKSLevel.SaveTiles();
var
	gzf: gzFile;
	i: integer;
begin
	if Assigned(Log) then Log.Log(LOG_INFO, 'TKSLevel.SaveTiles()...');
	gzf := gzOpen(PChar(FileName), 'w');
	if (gzf = nil) then
	begin
		if Assigned(Log) then Log.Log(LOG_ERROR, 'Cannot open level tile file "' + FileName + '"!');
		raise Exception.Create('Cannot open level tile file "' + FileName + '"!');
	end;
	try
		for i := 0 to NumRooms - 1 do
		begin
			// write the header:
			gzWriteString(gzf, 'x' + IntToStr(Room[i].XPos) + 'y' + IntToStr(Room[i].YPos) + #0);
			gzWriteString(gzf, CONST_HEADER_UNKNOWNCHUNK);

			// write the room data:
			gzWrite(gzf, @(Room[i].Data), sizeof(Room[i].Data));
		end;
	finally
		gzClose(gzf);
	end;
end;





procedure TKSLevel.SaveEventParams();
var
	ep, sl: TStringList;
	i: integer;
begin
	ep := TStringList.Create();
	try
		ep.Add('[World]');
		ep.AddStrings(WorldParams);
		ep.Add('');
		ep.Add('[Cutscene Music]');
		ep.AddStrings(CutsceneMusicParams);
		ep.Add('');
		for i := 0 to NumRooms - 1 do
		begin
			sl := Room[i].GetEventParams();
			if Assigned(sl) then
			begin
				if (sl.Count > 0) then
				begin
					ep.Add('[x' + IntToStr(Room[i].XPos) + 'y' + IntToStr(Room[i].YPos) + ']');
					ep.AddStrings(sl);
					ep.Add('');
				end;
				sl.Free();
			end;
		end;		// for i

		// Add OtherParams, as parsed in LoadEventParams():
		ep.AddStrings(OtherParams);

		// save to INI:
		try
			ep.SaveToFile(LevelDir + 'World.ini');
		except
			on e: Exception do
			begin
				if Assigned(Log) then Log.Log(LOG_ERROR, '!!! Exception received: "' + e.Message + '" !!!');
				raise;
			end;
		end;
	finally
		ep.Free();
	end;
end;





procedure TKSLevel.SaveDefaultSavegame();
var
	dsg: TStringList;
	i: integer;
begin
	dsg := TStringList.Create();
	try
		dsg.Add('[Positions]');
		dsg.Add('X Map=' + IntToStr(StartRoomX));
		dsg.Add('Y Map=' + IntToStr(StartRoomY));
		dsg.Add('X Pos=' + IntToStr(StartX));
		dsg.Add('Y Pos=' + IntToStr(StartY));
		dsg.Add('[Powers]');
		for i := 0 to 11 do
		begin
			dsg.Add('Power' + IntToStr(i) + '=' + BoolToVal(StartPower[i]));
		end;
		try
			dsg.SaveToFile(LevelDir + 'DefaultSavegame.ini');
		except
			on e: Exception do
			begin
				if Assigned(Log) then Log.Log(LOG_ERROR, '!!! Exception received: "' + e.Message + '" !!!');
				raise;
			end;
		end;
	finally
		dsg.Free();
	end;
end;





function TKSLevel.AddRoom(x, y: integer; const iRoomRec: PKSRoomRec): TKSRoom;
begin
	if (NumRooms >= CapRooms) then
	begin
		CapRooms := NumRooms + 16;
		SetLength(Room, CapRooms);
	end;
	Result := TKSRoom.Create(Self);
	Room[NumRooms] := Result;
	Result.XPos := X;
	Result.YPos := Y;
	Result.Data := iRoomRec^;
	NumRooms := NumRooms + 1;
end;





function  TKSLevel.GetRoom(x, y: integer): TKSRoom;
var
	i: integer;
begin
	for i := 0 to NumRooms - 1 do
	begin
		if ((Room[i].XPos = x) and (Room[i].YPos = y)) then
		begin
			Result := Room[i];
			Exit;
		end;
	end;
	Result := nil;
end;





procedure TKSLevel.ParseWorldParams();
var
	i: integer;
	Line: string;
	EqSign: integer;
	Name, Value: string;
begin
	// first try extracting the level name from INI file settings:
	LevelName := '';
	AuthorName := '';
	for i := 0 to WorldParams.Count - 1 do
	begin
		Line := Trim(WorldParams[i]);
		if (Line = '') then
		begin
			// skip empty lines
			continue;
		end;
		EqSign := Pos('=', Line);
		if (EqSign <= 0) then
		begin
			// not a "Name=Value" line
			continue;
		end;

		Name := AnsiLowerCase(Trim(Copy(Line, 1, EqSign - 1)));
		Value := Trim(Copy(Line, EqSign + 1, Length(Line)));
		if (Name = 'name') then
		begin
			Levelname := Value;
			continue;
		end;
		if (Name = 'author') then
		begin
			AuthorName := Value;
			continue;
		end;
	end;
end;





procedure TKSLevel.Changed();
begin
	IsModified := true;
	ChangedListeners.Trigger(Self);
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSObject:

constructor TKSObject.Create(iBank: byte; iID: byte);
begin
	inherited Create();
	Bank := iBank;
	ID := iID;
	// TODO: description loading
	// Description := TryLoadDescription();
	Img := nil;
	IntfImg := nil;
end;





destructor TKSObject.Destroy();
begin
	if Assigned(Img) then Img.Free();
	Img := nil;
	if Assigned(IntfImg) then IntfImg.Free();
	IntfImg := nil;
	inherited Destroy();
end;





procedure TKSObject.NeedImage();
var
	fnam: string;
begin
	if Assigned(Img) then
	begin
		Exit;
	end;

	fnam := gKSDir + 'Data\Objects\Bank' + IntToStr(Bank) + '\Object' + IntToStr(ID) + '.png';
	if not(FileExists(fnam)) then
	begin
		Exit;
	end;
	
	Img := TPortableNetworkGraphic.Create();
	try
		Img.LoadFromFile(fnam);
		CreatePNGAlpha(Img);
	except
		Img.Free();
		Img := nil;
	end;
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSObjectCollection:
constructor TKSObjectCollection.Create();
begin
	inherited Create();
	NumObjs := 0;
	CapObjs := 0;
end;





destructor TKSObjectCollection.Destroy();
begin
	Clear();
	inherited Destroy();
end;





procedure TKSObjectCollection.Clear();
var
	i: integer;
begin
	for i := 0 to NumObjs - 1 do
	begin
		Obj[i].Free();
	end;
	SetLength(Obj, 0);
	NumObjs := 0;
	CapObjs := 0;
end;





function TKSObjectCollection.GetObject(iBank, iObj: byte): TKSObject;
var
	i: integer;
begin
	// TODO: upgrade to binsearch:
	for i := 0 to NumObjs - 1 do
	begin
		if ((Obj[i].Bank = iBank) and (Obj[i].ID = iObj)) then
		begin
			Result := Obj[i];
			Exit;
		end;
	end;

	// not found, add it:
	Result := TKSObject.Create(iBank, iObj);
	if (NumObjs >= CapObjs) then
	begin
		CapObjs := NumObjs + 16;
		SetLength(Obj, CapObjs);
	end;
	Obj[NumObjs] := Result;
	NumObjs := NumObjs + 1;
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSShift:

constructor TKSShift.Create(iFromRoomX, iFromRoomY, iFromX, iFromY, iToRoomX, iToRoomY, iToX, iToY: integer; iKind: Char);
begin
	FromRoomX := iFromRoomX;
	FromRoomY := iFromRoomY;
	FromX := iFromX;
	FromY := iFromY;
	ToRoomX := iToRoomX;
	ToRoomY := iToRoomY;
	ToX := iToX;
	ToY := iToY;
	Kind := iKind;
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TShiftList:

constructor TKSShiftList.Create();
begin
	inherited Create();
	NumShifts := 0;
	CapShifts := 0;
end;






destructor TKSShiftList.Destroy();
begin
	Clear();
	inherited Destroy();
end;





procedure TKSShiftList.Clear();
var
	i: integer;
begin
	for i := 0 to NumShifts - 1 do
	begin
		Shift[i].Free();
	end;
	NumShifts := 0;
	CapShifts := 0;
	SetLength(Shift, 0);
end;





procedure TKSShiftList.UpdateFromLevel(iLevel: TKSLevel);
var
	i: integer;
begin
	Clear();

	// scan level for powerups:
	for i := 0 to iLevel.NumRooms - 1 do
	begin
		AddSingleRoom(iLevel.Room[i]);
	end;
end;





function TKSShiftList.ExtractSingleFromRoom(iX, iY: integer): TKSShiftList;
var
	i: integer;
begin
	Result := TKSShiftList.Create();
	for i := 0 to NumShifts - 1 do
	begin
		if ((Shift[i].FromRoomX = iX) and (Shift[i].FromRoomY = iY)) then
		begin
			Result.AddShift(Shift[i].FromRoomX, Shift[i].FromRoomY, Shift[i].FromX, Shift[i].FromY, Shift[i].ToRoomX, Shift[i].ToRoomY, Shift[i].ToX, Shift[i].ToY, Shift[i].Kind);
		end;
	end;
end;





function TKSShiftList.ExtractSingleToRoom(iX, iY: integer): TKSShiftList;
var
	i: integer;
begin
	Result := TKSShiftList.Create();
	for i := 0 to NumShifts - 1 do
	begin
		if ((Shift[i].ToRoomX = iX) and (Shift[i].ToRoomY = iY)) then
		begin
			Result.AddShift(Shift[i].FromRoomX, Shift[i].FromRoomY, Shift[i].FromX, Shift[i].FromY, Shift[i].ToRoomX, Shift[i].ToRoomY, Shift[i].ToX, Shift[i].ToY, Shift[i].Kind);
		end;
	end;
end;





procedure TKSShiftList.AddSingleRoom(iRoom: TKSRoom);
var
	lay, x, y: integer;
	Data: TKSRoomRec;
begin
	// scan room:
	Data := iRoom.Data;
	for lay := 4 to 7 do
	begin
		for y := 0 to 9 do
		begin
			for x := 0 to 24 do
			begin
				if (Data.Obj[lay].Bank[y, x] <> 0) then continue;
				case (Data.Obj[lay].Obj[y, x]) of
					KSOBJ_SHIFTA:
					begin
						ParseShift(iRoom, x, y, 'A');
					end;
					KSOBJ_SHIFTB:
					begin
						ParseShift(iRoom, x, y, 'B');
					end;
					KSOBJ_SHIFTC:
					begin
						ParseShift(iRoom, x, y, 'C');
					end;
				end;
			end;		// for x
		end;		// for y
	end;		// for lay
end;





procedure TKSShiftList.ParseShift(iRoom: TKSRoom; iX, iY: integer; iKind: Char);
var
	HasShift: boolean;
	rxfound, ryfound, xfound, yfound, afound: boolean;
	rx, ry, x, y: integer;
begin
	HasShift := iRoom.ParseShift(rx, ry, x, y, rxfound, ryfound, xfound, yfound, afound, iKind);
	if not(HasShift) then
	begin
		Exit;
	end;
	if not(rxfound) then
	begin
		rx := iRoom.XPos;
	end;
	if not(ryfound) then
	begin
		ry := iRoom.YPos;
	end;
	if not(xfound) then
	begin
		x := iX;
	end;
	if not(yfound) then
	begin
		y := iY;
	end;
	if not(afound) then
	begin
		if (rxfound) then
		begin
			rx := rx + iRoom.XPos;
		end;
		if (ryfound) then
		begin
			ry := ry + iRoom.YPos;
		end;
	end;
	AddShift(iRoom.XPos, iRoom.YPos, iX, iY, rx, ry, x, y, iKind);
end;





procedure TKSShiftList.AddShift(iFromRoomX, iFromRoomY, iFromX, iFromY, iToRoomX, iToRoomY, iToX, iToY: integer; iKind: Char);
begin
	if (NumShifts >= CapShifts) then
	begin
		CapShifts := NumShifts + 16;
		SetLength(Shift, CapShifts);
	end;
	Shift[NumShifts] := TKSShift.Create(iFromRoomX, iFromRoomY, iFromX, iFromY, iToRoomX, iToRoomY, iToX, iToY, iKind);
	NumShifts := NumShifts + 1;
end;






function TKSRoom.ParseShift(var oRoomX, oRoomY, oX, oY: integer; var oHasRoomX, oHasRoomY, oHasX, oHasY, oHasAbsolute: boolean; iKind: Char): boolean;
var
	sfrx, sfry, sfx, sfy, sfa: string;
	code: integer;
	i: integer;
	s: string;
	iKindTmp: string;
begin
	iKindTmp:= LowerCase(iKind);
	iKind := iKindTmp[1];
	sfrx := 'shiftxmap(' + iKind + ')=';
	sfry := 'shiftymap(' + iKind + ')=';
	sfx  := 'shiftxpos(' + iKind + ')=';
	sfy  := 'shiftypos(' + iKind + ')=';
	sfa  := 'shiftabsolutetarget(' + iKind + ')=';
	oHasRoomX := false;
	oHasRoomY := false;
	oHasX := false;
	oHasY := false;
	oHasAbsolute := false;
	oRoomX := 0;
	oRoomY := 0;
	oX := 0;
	oY := 0;
	for i := 0 to EventParams.Count - 1 do
	begin
		s := LowerCase(EventParams[i]);
		if (Copy(s, 1, Length(sfrx)) = sfrx) then
		begin
			val(copy(s, Length(sfrx) + 1, Length(s)), oRoomX, code);
			oHasRoomX := (code = 0);
		end
		else if (Copy(s, 1, Length(sfry)) = sfry) then
		begin
			val(copy(s, Length(sfry) + 1, Length(s)), oRoomY, code);
			oHasRoomY := (code = 0);
		end
		else if (Copy(s, 1, Length(sfx)) = sfx) then
		begin
			val(copy(s, Length(sfx) + 1, Length(s)), oX, code);
			oHasX := (code = 0);
		end
		else if (Copy(s, 1, Length(sfy)) = sfy) then
		begin
			val(copy(s, Length(sfy) + 1, Length(s)), oY, code);
			oHasY := (code = 0);
		end
		else if (Copy(s, 1, Length(sfa)) = sfa) then
		begin
			oHasAbsolute := true;
		end
	end;		// for i
	Result := (oHasRoomX or oHasRoomY or oHasX or oHasY) and HasShiftInLayers(iKind);
end;





function  TKSRoom.HasShiftInLayers(iKind: char): boolean;
begin
	case (iKind) of
		'a', 'A': Result := DoesContainObject(0, KSOBJ_SHIFTA);
		'b', 'B': Result := DoesContainObject(0, KSOBJ_SHIFTB);
		'c', 'C': Result := DoesContainObject(0, KSOBJ_SHIFTC);
		else Result := false;
	end;
end;





procedure TKSRoom.ModifyShiftDest(iShift: char; iRoomX, iRoomY: integer);
var
	i: integer;
	txt: string;
	sfrx, sfry: string;
begin
	sfrx := 'shiftxmap(' + LowerCase(iShift) + ')=';
	sfry := 'shiftymap(' + LowerCase(iShift) + ')=';
	for i := 0 to EventParams.Count - 1 do
	begin
		txt := LowerCase(trim(EventParams[i]));
		if (pos(sfrx, txt) > 0) then
		begin
			EventParams[i] := 'ShiftXMap(' + UpperCase(iShift) + ')=' + IntToStr(iRoomX);
			continue;
		end;
		if (pos(sfry, txt) > 0) then
		begin
			EventParams[i] := 'ShiftYMap(' + UpperCase(iShift) + ')=' + IntToStr(iRoomY);
			continue;
		end;
	end;		// for i - EventParams[]
end;





procedure TKSRoom.ParseWarps(var oHasWarpL, oHasWarpR, oHasWarpU, oHasWarpD: boolean; var oWarpXL, oWarpYL, oWarpXR, oWarpYR, oWarpXU, oWarpYU, oWarpXD, oWarpYD: integer);
var
	i: integer;
	txt, vp: string;
	code: integer;
begin
	oHasWarpL := false;
	oHasWarpR := false;
	oHasWarpU := false;
	oHasWarpD := false;
	oWarpXL := 0;
	oWarpYL := 0;
	oWarpXR := 0;
	oWarpYR := 0;
	oWarpXU := 0;
	oWarpYU := 0;
	oWarpXD := 0;
	oWarpYD := 0;
	if not(DoesContainObject(0, KSOBJ_WARP)) then
	begin
		Exit;
	end;
	
	for i := 0 to EventParams.Count - 1 do
	begin
		txt := AnsiLowerCase(EventParams[i]);
		vp := trim(Copy(txt, 10, Length(txt)));
		if (Pos('warpx(l)=', txt) = 1) then
		begin
			oHasWarpL := true;
			Val(vp, oWarpXL, code);
		end
		else if (Pos('warpy(l)=', txt) = 1) then
		begin
			oHasWarpL := true;
			Val(vp, oWarpYL, code);
		end
		else if (Pos('warpx(r)=', txt) = 1) then
		begin
			oHasWarpR := true;
			Val(vp, oWarpXR, code);
		end
		else if (Pos('warpy(r)=', txt) = 1) then
		begin
			oHasWarpR := true;
			Val(vp, oWarpYR, code);
		end
		else if (Pos('warpx(u)=', txt) = 1) then
		begin
			oHasWarpU := true;
			Val(vp, oWarpXU, code);
		end
		else if (Pos('warpy(u)=', txt) = 1) then
		begin
			oHasWarpU := true;
			Val(vp, oWarpYU, code);
		end
		else if (Pos('warpx(d)=', txt) = 1) then
		begin
			oHasWarpD := true;
			Val(vp, oWarpXD, code);
		end
		else if (Pos('warpy(d)=', txt) = 1) then
		begin
			oHasWarpD := true;
			Val(vp, oWarpYD, code);
		end
	end;		// for i - EventParams[]
end;





procedure TKSRoom.SetWarpsToParams();
begin
	// Update EventParams[]:
	SetWarpToParams('L', HasWarpL, WarpXL, WarpYL);
	SetWarpToParams('R', HasWarpR, WarpXR, WarpYR);
	SetWarpToParams('U', HasWarpU, WarpXU, WarpYU);
	SetWarpToParams('D', HasWarpD, WarpXD, WarpYD);

	// Update neighbors:
	RoomLeft  := Parent.GetRoom(XPos + WarpXL - 1, YPos + WarpYL);
	RoomRight := Parent.GetRoom(XPos + WarpXR + 1, YPos + WarpYR);
	RoomUp    := Parent.GetRoom(XPos + WarpXU,     YPos + WarpYU - 1);
	RoomDown  := Parent.GetRoom(XPos + WarpXD,     YPos + WarpYD + 1);
end;





procedure TKSRoom.SetWarpToParams(iKind: char; iHas: boolean; iX, iY: integer);
var
	i: integer;
	txt, vp: string;
	iKindLC, iKindUC: char;
	iKindLCtmp, iKindUctmp: string;
begin
	iKindLCtmp:= LowerCase(iKind);
	iKindLC := iKindLCtmp[1];
	iKindUCtmp:= UpperCase(iKind);
	iKindUC := iKindUCtmp[1];
	for i := EventParams.Count - 1 downto 0 do
	begin
		txt := AnsiLowerCase(EventParams[i]);
		vp := trim(Copy(txt, 10, Length(txt)));
		if (Pos('warpx(' + iKindLC + ')=', txt) = 1) then
		begin
			if (iHas) then
			begin
				EventParams[i] := 'WarpX(' + iKindUC + ')=' + IntToStr(iX);
			end
			else
			begin
				EventParams.Delete(i);
			end;
			continue;
		end;

		if (Pos('warpy(' + iKindLC + ')=', txt) = 1) then
		begin
			if (iHas) then
			begin
				EventParams[i] := 'WarpY(' + iKindUC + ')=' + IntToStr(iY);
			end
			else
			begin
				EventParams.Delete(i);
			end;
			continue;
		end;
	end;		// for i - EventParams[]
end;





initialization
	gKSObjects := TKSObjectCollection.Create();





finalization
	gKSObjects.Free();





end.
