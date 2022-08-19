unit udlgDuplicateRooms;

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
	ExtCtrls,
	Buttons,
	StdCtrls,
	uKSRepresentations,
	uRoomDuplicator,
	uKSMapView;





type
	TdlgDuplicateRooms = class(TForm)
    pTl: TPanel;
    tlOK: TSpeedButton;
    tlCancel: TSpeedButton;
    pMap: TPanel;
    pTop: TPanel;
    pSettings: TPanel;
    Label3: TLabel;
    pOffset: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lCollisionWarning: TLabel;
    eOffsetX: TEdit;
    eOffsetY: TEdit;
    chbUseSelection: TCheckBox;
    chbModRelContainedShifts: TCheckBox;
    chbModAbsContainedShifts: TCheckBox;
    chbModContainedWarps: TCheckBox;
    chbModRelOutShifts: TCheckBox;
    chbModAbsOutShifts: TCheckBox;
    chbModOutWarps: TCheckBox;

		procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tlCancelClick(Sender: TObject);
    procedure tlOKClick(Sender: TObject);
		procedure eOffsetChange(Sender: TObject);
		procedure MapRoomSelectionChanged(Sender: TObject);

	private
		Duplicator: TRoomDuplicator;
		UpdatingProps: boolean;
		Settings: TRoomDuplicatorSettings;
		
	public
		MapView: TKSMapView;
		Level: TKSLevel;

		OffsetX, OffsetY: integer;

		constructor Create(AOwner: TComponent; iLevel: TKSLevel); reintroduce;
		destructor Destroy(); override;
	end;


















implementation

uses
	ufrmMain;





{$R *.lfm}





constructor TdlgDuplicateRooms.Create(AOwner: TComponent; iLevel: TKSLevel);
begin
	inherited Create(AOwner);
	Settings := TRoomDuplicatorSettings.Create();
	Level := iLevel;
	MapView := TKSMapView.Create(Self);
	MapView.Level := Level;
	MapView.Align := alClient;
	MapView.GoToCoord(frmMain.CurrentXPos, frmMain.CurrentYPos);
	MapView.Parent := pMap;
	MapView.Align := alClient;
	MapView.HighlightCenter := false;
	MapView.OnRoomSelectionChanged := MapRoomSelectionChanged;
	chbModRelContainedShifts.Checked := Settings.ModifyRelativeContainedShifts;
	chbModAbsContainedShifts.Checked := Settings.ModifyAbsoluteContainedShifts;
	chbModContainedWarps.Checked     := Settings.ModifyContainedWarps;
	chbModRelOutShifts.Checked       := Settings.ModifyRelativeOutgoingShifts;
	chbModAbsOutShifts.Checked       := Settings.ModifyAbsoluteOutgoingShifts;
	chbModOutWarps.Checked           := Settings.ModifyContainedWarps;
end;





destructor TdlgDuplicateRooms.Destroy();
begin
	inherited Destroy();
end;





procedure TdlgDuplicateRooms.FormCreate(Sender: TObject);
begin
	tlOK.Glyph.LoadFromResourceName(HInstance, 'BBOK');
	tlCancel.Glyph.LoadFromResourceName(HInstance, 'BBCANCEL');
end;





procedure TdlgDuplicateRooms.FormShow(Sender: TObject);
var
	x, y: integer;
begin
	Duplicator := TRoomDuplicator.Create(Level, MapView.Selection, (MapView.Selection.Count > 0));
	Duplicator.Guess(x, y);
	UpdatingProps := true;
	eOffsetX.Text := IntToStr(x);
	UpdatingProps := false;
	eOffsetY.Text := IntToStr(y);
	chbUseSelection.Checked := (MapView.Selection.Count > 0);
end;





procedure TdlgDuplicateRooms.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caHide;
	Duplicator.Free();
	Duplicator := nil;
end;





procedure TdlgDuplicateRooms.tlCancelClick(Sender: TObject);
begin
	ModalResult := mrCancel;
end;





procedure TdlgDuplicateRooms.tlOKClick(Sender: TObject);
begin
	ModalResult := mrOK;
	Settings.ModifyAbsoluteContainedShifts := chbModAbsContainedShifts.Checked;
	Settings.ModifyAbsoluteOutgoingShifts  := chbModAbsOutShifts.Checked;
	Settings.ModifyContainedWarps          := chbModContainedWarps.Checked;
	Settings.ModifyRelativeContainedShifts := chbModRelContainedShifts.Checked;
	Settings.ModifyRelativeOutgoingShifts  := chbModRelOutShifts.Checked;
	Settings.ModifyOutgoingWarps           := chbModOutWarps.Checked;
	Duplicator.Duplicate(OffsetX, OffsetY, Settings);
end;





procedure TdlgDuplicateRooms.eOffsetChange(Sender: TObject);
var
	Decoded, code: integer;
begin
	if UpdatingProps then Exit;
	val(eOffsetX.Text, Decoded, code);
	if (code = 0) then
	begin
		OffsetX := Decoded;
	end;
	val(eOffsetY.Text, Decoded, code);
	if (code = 0) then
	begin
		OffsetY := Decoded;
	end;
	Duplicator.SelectionOnly := chbUseSelection.Checked;
	lCollisionWarning.Visible := not(Duplicator.Check(OffsetX, OffsetY));
end;





procedure TdlgDuplicateRooms.MapRoomSelectionChanged(Sender: TObject);
begin
	lCollisionWarning.Visible := not(Duplicator.Check(OffsetX, OffsetY));
end;





end.
