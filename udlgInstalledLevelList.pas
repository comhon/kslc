unit udlgInstalledLevelList;

{$MODE Delphi}

interface

uses
        {$DEFINE UseCThreads} {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
        Messages,
	SysUtils,
	Variants,
	Classes,
	Graphics,
	Controls,
	Forms,
	Dialogs,
	ExtCtrls,
	ComCtrls,
	StdCtrls,
	Contnrs,
	Buttons,
	uSettings;





const
	mrKSDirNotFound = -1;
	mrKSDirNoWorlds = -2;
	mrNoWorldFound = -3;





type
	TWorldDesc = class
	public
		Path: string;
		DirName: string;
		WorldName: string;
		AuthorName: string;
	end;





	TdlgInstalledLevelList = class(TForm)
		pMain: TPanel;
		pTl: TPanel;
		lWorlds: TLabel;
		lvWorld: TListView;
		tlOK: TSpeedButton;
		tlCancel: TSpeedButton;
		procedure tlCancelClick(Sender: TObject);
		procedure tlOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
	public
		Path: string;		// the chosen world's path is stored here

		constructor Create(AOwner: TComponent; iCaption: string = ''); reintroduce;
		destructor Destroy(); override;

		function ShowModal(): integer; override;

	protected
		Worlds: TObjectList;

		function  FindWorlds(): integer;

		procedure OnWorldUpdate(iWorld: TWorldDesc; iWorldName, iAuthor: string);		// TWorldParamUpdater callback
	end;





	TWorldUpdateProc = procedure(iWorld: TWorldDesc; iWorldName, iAuthor: string) of object;





	TWorldParamUpdater = class(TThread)
	public
		constructor Create(iWorlds: TObjectList; iOnUpdate: TWorldUpdateProc);
	protected
		Worlds: TObjectList;
		CurrentWorld: TWorldDesc;
		WorldName: string;
		Author: string;

		OnUpdate: TWorldUpdateProc;

		procedure Execute(); override;
		
		procedure DoUpdateOne();		// synced
	end;






















implementation

uses
	uKSRepresentations;



	

{$R *.lfm}





constructor TdlgInstalledLevelList.Create(AOwner: TComponent; iCaption: string = '');
begin
	inherited Create(AOwner);

	Worlds := TObjectList.Create(true);
	
	if (iCaption <> '') then
	begin
		Caption := iCaption;
	end;
	tlOK.Glyph.LoadFromResourceName(HInstance, 'BBOK');
	tlCancel.Glyph.LoadFromResourceName(HInstance, 'BBCANCEL');
end;





destructor TdlgInstalledLevelList.Destroy();
begin
	Worlds.Free();
	Worlds := nil;

	inherited Destroy();
end;





function TdlgInstalledLevelList.ShowModal(): integer;
begin
	Result := FindWorlds();
	if (Result <> mrOK) then
	begin
		Exit;
	end;

	Result := inherited ShowModal();
end;





function TdlgInstalledLevelList.FindWorlds(): integer;
var
	wrld: TWorldDesc;
	sr: TSearchRec;
	li: TListItem;
	i: integer;
begin
        //ShowMessage(gKSDir);
        if not(DirectoryExists(gKSDir)) then
	begin
		Result := mrKSDirNotFound;
		Exit;
	end;

	if not(DirectoryExists(IncludeTrailingPathDelimiter(gKSDir+'Worlds'))) then
	begin
		Result := mrKSDirNoWorlds;
		Exit;
	end;

	Worlds.Clear();
	if (FindFirst(gKSDir + 'Worlds' + PathDelim + '*', faAnyFile, sr) = 0) then
	begin
		repeat
                        if sr.Name = '..' then continue;
                        if sr.Name = '.' then continue;
                        wrld := TWorldDesc.Create();
			wrld.Path := IncludeTrailingpathDelimiter(ConcatPaths([gKSDir,'Worlds',sr.Name]));
			wrld.DirName := sr.Name;
			if (not(FileExists(wrld.Path + 'Map.bin')) or not(FileExists(wrld.Path + 'World.ini'))) then
			begin
				continue;
			end;
			Worlds.Add(wrld);
		until (FindNext(sr) <> 0);
		FindClose(sr);
	end;
	if (Worlds.Count <= 0) then
	begin
		Result := mrNoWorldFound;
		Exit;
	end;

	// fill in lvWorld:
	lvWorld.Items.BeginUpdate;
	try
		for i := 0 to Worlds.Count - 1 do
		begin
			li := lvWorld.Items.Add();
			li.Caption := TWorldDesc(Worlds[i]).DirName;
			li.SubItems.Add('');
			li.SubItems.Add('');
			li.Data := Worlds[i];
		end;
	finally
		lvWorld.Items.EndUpdate();
	end;
	TWorldParamUpdater.Create(Worlds, OnWorldUpdate);

	Result := mrOK;
end;





procedure TdlgInstalledLevelList.OnWorldUpdate(iWorld: TWorldDesc; iWorldName, iAuthor: string);		// TWorldParamUpdater callback
var
	i: integer;
begin
	iWorld.WorldName := iWorldName;
	iWorld.AuthorName := iAuthor;

	for i := 0 to lvWorld.Items.Count - 1 do
	begin
		if (lvWorld.Items[i].Data = iWorld) then
		begin
			lvWorld.Items[i].SubItems[0] := iWorldName;
			lvWorld.Items[i].SubItems[1] := iAuthor;
		end;
	end;
end;





procedure TdlgInstalledLevelList.tlCancelClick(Sender: TObject);
begin
	ModalResult := mrCancel;
end;





procedure TdlgInstalledLevelList.tlOKClick(Sender: TObject);
var
	li: TListItem;
begin
	li := lvWorld.ItemFocused;
	if not(Assigned(li)) then
	begin
		Exit;
	end;
	Path := TWorldDesc(li.Data).Path;

	ModalResult := mrOK;
end;






///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TWorldParamUpdater:

constructor TWorldParamUpdater.Create(iWorlds: TObjectList; iOnUpdate: TWorldUpdateProc);
begin
	inherited Create(true);
	Worlds := iWorlds;
	OnUpdate := iOnUpdate;
	FreeOnTerminate := true;
	Resume();
end;





procedure TWorldParamUpdater.Execute();
const
	bufsize = 1024;
var
	i: integer;
	fnam: string;
begin
	for i := 0 to Worlds.Count - 1 do
	begin
		CurrentWorld := TWorldDesc(Worlds[i]);
		// parse World.ini for author and name:
		fnam := ConcatPaths([CurrentWorld.Path,'World.ini']);
		if not(FileExists(fnam)) then
		begin
			continue;
		end;
		WorldName := '';
		//*h SetLength(WorldName, bufsize);
		//*h SetLength(WorldName, GetPrivateProfileString('World', 'Name', '', @(WorldName[1]), bufsize, PChar(fnam)));
		Author := '';
		//*h SetLength(Author, bufsize);
		//*h SetLength(Author, GetPrivateProfileString('World', 'Author', '', @(Author[1]), bufsize, PChar(fnam)));
		Synchronize(DoUpdateOne);
	end;
end;





procedure TWorldParamUpdater.DoUpdateOne();
begin
	if Assigned(OnUpdate) then
	begin
		OnUpdate(CurrentWorld, WorldName, Author);
	end;
end;




procedure TdlgInstalledLevelList.FormResize(Sender: TObject);
begin
	tlOK.Width := pTl.ClientWidth div 2;
end;





procedure TdlgInstalledLevelList.FormShow(Sender: TObject);
begin
	ActiveControl := lvWorld;
end;





procedure TdlgInstalledLevelList.FormKeyPress(Sender: TObject;
  var Key: Char);
begin
	case Key of
		#13:
		begin
			tlOKClick(Sender);
		end;
		#27:
		begin
			tlCancelClick(Sender);
		end;
	end;
end;





end.
