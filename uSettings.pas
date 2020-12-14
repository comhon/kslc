
unit uSettings;
{$MODE Delphi}

interface

uses
        Windows,
	Classes,
	Registry;




type
	TSettings = class
	public
		LastLevelDir: string;

		MRUFileName: array[0..4] of string;

		MainLeft, MainTop: integer;
		MapLeft, MapTop, MapWidth, MapHeight: integer;
		MapVisible: boolean;
		LogLeft, LogTop, LogWidth, LogHeight: integer;
		LogVisible: boolean;
		RoomParamsLeft, RoomParamsTop, RoomParamsWidth, RoomParamsHeight: integer;
		RoomParamsVisible: boolean;

		AllowWebVersionCheck: boolean;
		AllowWebStats: boolean;

		OnUpdateMRUMenu: TNotifyEvent;

		constructor Create();
		destructor Destroy(); override;
		procedure Clear();

		procedure Load();
		procedure Save();

		procedure GetForms();
		procedure SetForms();
		procedure SetDefaults();

		procedure PushMRUItem(iFileName: string);
		procedure UpdateMRUMenu();
	end;


procedure InitializeSettings;
procedure FinalizeSettings;


var
	gSettings: TSettings;

















implementation

uses
	Forms,
	SysUtils,
	Dialogs,
	ufrmMain,
	uKSRepresentations;







constructor TSettings.Create();
begin
	inherited Create();
	OnUpdateMRUMenu := nil;
	SetDefaults();
end;





destructor TSettings.Destroy();
begin
	Clear();
	inherited Destroy();
end;





procedure TSettings.Clear();
begin
	// nothing needed yet
end;





procedure TSettings.Load();
var
	s: TStringList;

	function LoadInt(idx: integer; iDefault: integer): integer;
	var
		code: integer;
	begin
		if (s.Count > idx) then
		begin
			val(s[idx], Result, code);
			if (Code <> 0) then Result := iDefault;
		end
		else
		begin
			Result := iDefault;
		end;
	end;

var
	i: integer;
begin
	SetDefaults();
	s := TStringList.Create();
	try
		try
			s.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'KSLC.ini');
		except
			ShowMessage('KSLC.ini not found, using default settings.');
			Exit;
		end;

		if (s.Count > 0) then
		begin
			if (s[0] <> 'KSLC.ini v1') then
			begin
				ShowMessage('KSLC.ini header is broken, settings were lost. Using default settings.');
				Exit;
			end;
		end;

		if (s.Count > 1) then
		begin
			gKSDir := s[1];
		end;

		if (s.Count > 2) then
		begin
			LastLevelDir := s[2];
		end
		else
		begin
			LastLevelDir := gKSDir + 'Worlds\';
		end;

		// MRU:
		for i := 0 to 4 do
		begin
			if (s.Count > i + 3) then
			begin
				MRUFileName[i] := s[i + 3];
			end
			else
			begin
				break;
			end;
		end;		// for i

		// main wnd:
		MainLeft := LoadInt(8, MainLeft);
		MainTop  := LoadInt(9, MainTop);

		// map wnd:
		MapLeft    := LoadInt(10, MapLeft);
		MapTop     := LoadInt(11, MapTop);
		MapWidth   := LoadInt(12, MapWidth);
		MapHeight  := LoadInt(13, MapHeight);
		MapVisible := (LoadInt(14, integer(MapVisible)) <> 0);

		// log wnd:
		LogLeft    := LoadInt(15, LogLeft);
		LogTop     := LoadInt(16, LogTop);
		LogWidth   := LoadInt(17, LogWidth);
		LogHeight  := LoadInt(18, LogHeight);
		LogVisible := (LoadInt(19, integer(LogVisible)) <> 0);

		// RoomParams wnd:
		RoomParamsLeft   := LoadInt(20, RoomParamsLeft);
		RoomParamsTop    := LoadInt(21, RoomParamsTop);
		RoomParamsWidth  := LoadInt(22, RoomParamsWidth);
		RoomParamsHeight := LoadInt(23, RoomParamsHeight);
		RoomParamsVisible := (LoadInt(24, integer(RoomParamsVisible)) <> 0);

		AllowWebVersionCheck := (LoadInt(25, integer(AllowWebVersionCheck)) <> 0);
		AllowWebStats        := (LoadInt(26, integer(AllowWebStats)) <> 0);
	finally
		s.Free();
	end;
end;





procedure TSettings.Save();
var
	s: TStringList;
	i: integer;
begin
	s := TStringList.Create();
	try
		s.Add('KSLC.ini v1');
		s.Add(gKSDir);
		s.Add(LastLevelDir);
		for i := 0 to 4 do
		begin
			s.Add(MRUFilename[i]);
		end;		// for i
		s.Add(IntToStr(MainLeft));
		s.Add(IntToStr(MainTop));
		s.Add(IntToStr(MapLeft));
		s.Add(IntToStr(MapTop));
		s.Add(IntToStr(MapWidth));
		s.Add(IntToStr(MapHeight));
		s.Add(IntToStr(integer(MapVisible)));
		s.Add(IntToStr(LogLeft));
		s.Add(IntToStr(LogTop));
		s.Add(IntToStr(LogWidth));
		s.Add(IntToStr(LogHeight));
		s.Add(IntToStr(integer(LogVisible)));
		s.Add(IntToStr(RoomParamsLeft));
		s.Add(IntToStr(RoomParamsTop));
		s.Add(IntToStr(RoomParamsWidth));
		s.Add(IntToStr(RoomParamsHeight));
		s.Add(IntToStr(integer(RoomParamsVisible)));
		s.Add(IntToStr(integer(AllowWebVersionCheck)));
		s.Add(IntToStr(integer(AllowWebStats)));
		try
			s.SaveToFile(ExtractFilePath(ParamStr(0)) + 'KSLC.ini');
		except
			ShowMessage('Cannot write KSLC.ini, settings will be lost');
		end;
	finally
		s.Free();
	end;
end;





procedure TSettings.GetForms();
begin
	MainLeft := frmMain.Left;
	MainTop  := frmMain.Top;
end;





procedure TSettings.SetForms();
begin
	frmMain.Left := MainLeft;
	frmMain.Top  := MainTop;
end;





procedure TSettings.SetDefaults();
var
	xedge, yedge: integer;
	FontHeight: integer;
begin
	xedge := GetSystemMetrics(SM_CXEDGE) + 1;
	yedge := GetSystemMetrics(SM_CYEDGE) + 1;
	if (Screen.MenuFont.Height < 0) then
	begin
		FontHeight := -Screen.MenuFont.Height;
	end
	else
	begin
		FontHeight := Screen.MenuFont.Height;
	end;

	MainLeft := 0;
	MainTop  := 0;

	// map wnd:
	MapLeft    := 650 + 2 * xedge;
	MapTop     := 0;
	MapWidth   := Screen.Width - MapLeft;
	MapHeight  := 300 + yedge + GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYMENU) + FontHeight + 1;
	MapVisible := true;

	// RoomParams wnd:
	RoomParamsLeft   := 0;
	RoomParamsTop    := MapHeight;
	RoomParamsWidth  := MapLeft;
	RoomParamsHeight := GetSystemMetrics(SM_CYMAXIMIZED) - RoomParamsTop - 2 * yedge;
	RoomParamsVisible := true;

	// log wnd:
	LogLeft    := MapLeft;
	LogTop     := RoomParamsTop;
	LogWidth   := MapWidth;
	LogHeight  := RoomParamsHeight;
	LogVisible := true;

	AllowWebVersionCheck := true;
	AllowWebStats := true;
end;





procedure TSettings.PushMRUItem(iFileName: string);
var
	lc: string;
	i, j: integer;
begin
	// search for it in the array:
	lc := AnsiLowercase(iFileName);
	for i := 0 to 4 do
	begin
		if (AnsiLowercase(MRUFileName[i]) = lc) then
		begin
			for j := i downto 1 do
			begin
				MRUFileName[j] := MRUFileName[j - 1];
			end;		// for j
			MRUFileName[0] := iFileName;
			UpdateMRUMenu();
			Exit;
		end;
	end;		// for i

	// not found, push down from top:
	for i := 4 downto 1 do
	begin
		MRUFileName[i] := MRUFileName[i - 1];
	end;
	MRUFileName[0] := iFileName;
	UpdateMRUMenu();
end;





procedure TSettings.UpdateMRUMenu();
begin
	if Assigned(OnUpdateMRUMenu) then
	begin
		OnUpdateMRUMenu(Self);
	end;
end;

//Replaced initialization section with procedure
procedure InitializeSettings;
begin
     gSettings := TSettings.Create();
     gSettings.Load(); //* This functions could ShowMessage function that requires Application object to be initialized
end;

//Replaced finalization section with procedure
procedure FinalizeSettings;
begin
     gSettings.Save();
     gSettings.Free();
end;


end.
 
