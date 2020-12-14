unit udlgMultiRoomParam;

{$MODE Delphi}

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
	Buttons,
	ExtCtrls,
	StdCtrls,
	uVectors,
	uKSRepresentations,
	uKSMapView;





type
	TKSParam = (kspMusic, kspAtmosA, kspAtmosB, kspTilesetA, kspTilesetB, kspBackground);





	TdlgMultiRoomParam = class(TForm)
		pTop: TPanel;
    pMap: TPanel;
    pTl: TPanel;
    tlChange: TSpeedButton;
    tlCancel: TSpeedButton;
    pParam: TPanel;
    pChange: TPanel;
    Label1: TLabel;
    lbParam: TListBox;
    Label2: TLabel;
    rbChSetTo: TRadioButton;
    rbChAdd: TRadioButton;
    rbChMultiplyBy: TRadioButton;
    rbChCopyFrom: TRadioButton;
    cbChCopyFrom: TComboBox;
    rbChExchangeWith: TRadioButton;
    cbChExchangeWith: TComboBox;
    eChSetTo: TEdit;
    eChAdd: TEdit;
		eChMultiplyBy: TEdit;

		procedure FormCreate(Sender: TObject);
    procedure tlChangeClick(Sender: TObject);
    procedure tlCancelClick(Sender: TObject);
		
	private
		Level: TKSLevel;

	public
		MapView: TKSMapView;

		constructor Create(AOwner: TComponent; iLevel: TKSLevel); reintroduce;
		destructor Destroy(); override;

		procedure DoSetTo();
		procedure DoAdd();
		procedure DoMultiply();
		procedure DoCopyFrom();
		procedure DoExchangeWith();

		procedure SetRoomParam(iRoom: TKSRoom; iParam: TKSParam; iVal: integer);
		function  GetRoomParam(iRoom: TKSRoom; iParam: TKSParam): integer;
	end;





















implementation

uses ufrmMain;





{$R *.lfm}





constructor TdlgMultiRoomParam.Create(AOwner: TComponent; iLevel: TKSLevel);
begin
	inherited Create(AOwner);
	Level := iLevel;
	MapView := TKSMapView.Create(Self);
	MapView.Level := Level;
	MapView.Align := alClient;
	MapView.GoToCoord(frmMain.CurrentXPos, frmMain.CurrentYPos);
	MapView.Parent := pMap;
	MapView.Align := alClient;
	MapView.HighlightCenter := false;
end;





destructor TdlgMultiRoomParam.Destroy();
begin
	inherited Destroy();
end;





procedure TdlgMultiRoomParam.FormCreate(Sender: TObject);
begin
	lbParam.Items.Clear();
	lbParam.Items.AddObject('Music',      TObject(kspMusic));
	lbParam.Items.AddObject('AtmosA',     TObject(kspAtmosA));
	lbParam.Items.AddObject('AtmosB',     TObject(kspAtmosB));
	lbParam.Items.AddObject('TilesetA',   TObject(kspTilesetA));
	lbParam.Items.AddObject('TilesetB',   TObject(kspTilesetB));
	lbParam.Items.AddObject('Background', TObject(kspBackground));
	lbParam.ItemIndex := 0;
	cbChCopyFrom.Items.Clear();
	cbChCopyFrom.Items.AddStrings(lbParam.Items);
	cbChCopyFrom.ItemIndex := 0;
	cbChExchangeWith.Items.Clear();
	cbChExchangeWith.Items.AddStrings(lbParam.Items);
	cbChExchangeWith.ItemIndex := 0;
	tlChange.Glyph.LoadFromResourceName(HInstance, 'BBOK');
	tlCancel.Glyph.LoadFromResourceName(HInstance, 'BBCANCEL');
	pMap.DoubleBuffered := true;
end;





procedure TdlgMultiRoomParam.tlChangeClick(Sender: TObject);
begin
	if (MapView.Selection.Count <= 0) then
	begin
		ShowMessage('Warning: empty room selection. To select rooms for which to apply the change, click and drag on the map screen. Selection is drawn yellow.'); 
	end;
	// Do the actual change:
	if (rbChSetTo.Checked) then
	begin
		DoSetTo();
	end
	else if (rbChAdd.Checked) then
	begin
		DoAdd();
	end
	else if (rbChMultiplyBy.Checked) then
	begin
		DoMultiply();
	end
	else if (rbChCopyFrom.Checked) then
	begin
		DoCopyFrom();
	end
	else if (rbChExchangeWith.Checked) then
	begin
		DoExchangeWith();
	end;
end;






procedure TdlgMultiRoomParam.tlCancelClick(Sender: TObject);
begin
	ModalResult := mrCancel;
end;





procedure TdlgMultiRoomParam.DoSetTo();
var
	v, code: integer;
	i: integer;
	param: TKSParam;
begin
	val(eChSetTo.Text, v, code);
	param := TKSParam(lbParam.Items.Objects[lbParam.ItemIndex]);
	for i := 0 to MapView.Selection.Count - 1 do
	begin
		SetRoomParam(TKSRoom(MapView.Selection[i]), param, v);
	end;
end;





procedure TdlgMultiRoomParam.DoAdd();
var
	v, code: integer;
	i: integer;
	param: TKSParam;
begin
	val(eChAdd.Text, v, code);
	param := TKSParam(lbParam.Items.Objects[lbParam.ItemIndex]);
	for i := 0 to MapView.Selection.Count - 1 do
	begin
		SetRoomParam(TKSRoom(MapView.Selection[i]), param, v + GetRoomParam(TKSRoom(MapView.Selection[i]), param));
	end;
end;





procedure TdlgMultiRoomParam.DoMultiply();
var
	v, code: integer;
	i: integer;
	param: TKSParam;
begin
	val(eChMultiplyBy.Text, v, code);
	param := TKSParam(lbParam.Items.Objects[lbParam.ItemIndex]);
	for i := 0 to MapView.Selection.Count - 1 do
	begin
		SetRoomParam(TKSRoom(MapView.Selection[i]), param, v * GetRoomParam(TKSRoom(MapView.Selection[i]), param));
	end;
end;





procedure TdlgMultiRoomParam.DoCopyFrom();
var
	i: integer;
	param, param2: TKSParam;
begin
	param := TKSParam(lbParam.Items.Objects[lbParam.ItemIndex]);
	param2 := TKSParam(cbChCopyFrom.Items.Objects[cbChCopyFrom.ItemIndex]);
	for i := 0 to MapView.Selection.Count - 1 do
	begin
		SetRoomParam(TKSRoom(MapView.Selection[i]), param, GetRoomParam(TKSRoom(MapView.Selection[i]), param2));
	end;
end;




procedure TdlgMultiRoomParam.DoExchangeWith();
var
	v: integer;
	i: integer;
	param, param2: TKSParam;
begin
	param := TKSParam(lbParam.Items.Objects[lbParam.ItemIndex]);
	param2 := TKSParam(cbChCopyFrom.Items.Objects[cbChCopyFrom.ItemIndex]);
	for i := 0 to MapView.Selection.Count - 1 do
	begin
		v := GetRoomParam(TKSRoom(MapView.Selection[i]), param2);
		SetRoomParam(TKSRoom(MapView.Selection[i]), param2, GetRoomParam(TKSRoom(MapView.Selection[i]), param));
		SetRoomParam(TKSRoom(MapView.Selection[i]), param,  v);
	end;
end;





procedure TdlgMultiRoomParam.SetRoomParam(iRoom: TKSRoom; iParam: TKSParam; iVal: integer);
begin
	case iParam of
		kspAtmosA:     iRoom.Data.AtmosA     := iVal;
		kspAtmosB:     iRoom.Data.AtmosB     := iVal;
		kspBackground: iRoom.Data.Background := iVal;
		kspMusic:      iRoom.Data.Music      := iVal;
		kspTilesetA:   iRoom.Data.TilesetA   := iVal;
		kspTilesetB:   iRoom.Data.TilesetB   := iVal;
	end;
	iRoom.UpdateFromData();
	iRoom.Parent.ChangedListeners.Trigger(Self);
end;





function TdlgMultiRoomParam.GetRoomParam(iRoom: TKSRoom; iParam: TKSParam): integer;
begin
	case iParam of
		kspAtmosA: Result := iRoom.Data.AtmosA;
		kspAtmosB: Result := iRoom.Data.AtmosB;
		kspBackground: Result := iRoom.Data.Background;
		kspMusic: Result := iRoom.Data.Music;
		kspTilesetA: Result := iRoom.Data.TilesetA;
		kspTilesetB: Result := iRoom.Data.TilesetB;
		else
			Result := 0;
	end;
end;





end.
