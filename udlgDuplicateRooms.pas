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
	uKSMapView, uKSRoomView, udlgRoomView;





type

        { TdlgDuplicateRooms }

 TdlgDuplicateRooms = class(TForm)
          cbPreview: TCheckBox;
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

                procedure chbUseSelectionChange(Sender: TObject);
                procedure cbPreviewChange(Sender: TObject);
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

                fDlgPreview: TdlgRoomView;
                procedure MapRoomMouseUp(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
	public
		MapView: TKSMapView;
                MapView2: TKSMapView;
		Level: TKSLevel;
                Target: TKSLevel;

		OffsetX, OffsetY: integer;

		constructor Create(AOwner: TComponent; iLevel: TKSLevel; iTarget: TKSLevel = nil); reintroduce;
		destructor Destroy(); override;
	end;


















implementation

//uses
//	ufrmMain;





{$R *.lfm}





constructor TdlgDuplicateRooms.Create(AOwner: TComponent; iLevel: TKSLevel; iTarget: TKSLevel = nil);
begin
	inherited Create(AOwner);
	Settings := TRoomDuplicatorSettings.Create();
	Level := iLevel;
        Target:= iTarget;
        if Target<>nil then
        begin
          Width:=Width*2;
        end;

	MapView := TKSMapView.Create(Self);
	MapView.Level := Level;
	MapView.Align := alClient;
	MapView.Parent := pMap;
	MapView.HighlightCenter := false;
	MapView.OnRoomSelectionChanged := MapRoomSelectionChanged;
        MapView.OnMouseUp := MapRoomMouseUp;

      if Target<>nil then
      begin
        MapView2 := TKSMapView.Create(Self);
        MapView2.Width:=Width div 2;
	MapView2.Level := Target;
	MapView2.Align := alRight;
	MapView2.Parent := pMap;
	MapView2.HighlightCenter := false;
	MapView2.OnRoomSelectionChanged := MapRoomSelectionChanged;
        MapView2.XPos:=1000;
        MapView2.YPos:=1000;
      end;

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
//	tlOK.Glyph.LoadFromResourceName(HInstance, 'BBOK');
//	tlCancel.Glyph.LoadFromResourceName(HInstance, 'BBCANCEL');
      fDlgPreview:=TDlgRoomView.Create(Application);
end;

procedure TdlgDuplicateRooms.chbUseSelectionChange(Sender: TObject);
begin
  MapRoomSelectionChanged(Sender);
end;

procedure TdlgDuplicateRooms.cbPreviewChange(Sender: TObject);
begin
  fdlgPreview.Visible:=cbPreview.Checked;
  MapView.HighlightCenter := cbPreview.Checked;
end;





procedure TdlgDuplicateRooms.FormShow(Sender: TObject);
var
	x, y: integer;
begin
        if Target <> nil then
        begin
             Duplicator := TRoomDuplicator.Create(Level, Target, MapView.Selection, (MapView.Selection.Count > 0));
        end
        else
        begin
             Duplicator := TRoomDuplicator.Create(Level, MapView.Selection, (MapView.Selection.Count > 0));
        end;


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
        fDlgPreview.Free;
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

        MapRoomSelectionChanged(Sender);
end;





procedure TdlgDuplicateRooms.MapRoomSelectionChanged(Sender: TObject);
var
   i: integer;
   aSelMapView: TKSMapView;
   r: TKSRoom;
begin
        lCollisionWarning.Visible := not(Duplicator.Check(OffsetX, OffsetY));

   if Target <> nil then
   begin
        aSelMapView:=MapView2;
   end
   else
   begin
        aSelMapView:=MapView;
   end;

        aSelMapView.Addition.Clear();

        if Duplicator.SelectionOnly then
        begin
          for i:=0 to MapView.Selection.Count-1 do
          begin
            aSelMapView.Addition.Add(MapView.Selection[i]);
          end;
        end
        else
        begin
          for i:=0 to Level.NumRooms-1 do
          begin
            aSelMapView.Addition.Add(Level.Room[i]);
          end;
        end;

        aSelMapView.AdditionOffsetX:=OffsetX;
        aSelMapView.AdditionOffsetY:=OffsetY;

        r:=Level.GetRoom(MapView.XPos, MapView.YPos);
        if r<>nil then
        begin
             fDlgPreview.rv1.Room:=r;
        end;

        Invalidate;

end;

procedure TdlgDuplicateRooms.MapRoomMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   r: TKSRoom;
   i: integer;
begin
        r:=Level.GetRoom(MapView.XPos, MapView.YPos);

        for i:=0 to (MapView.Addition.Count)-1 do
        begin
          if (TKSRoom(MapView.Addition[i]).XPos+MapView.AdditionOffsetX = MapView.XPos) and
             (TKSRoom(MapView.Addition[i]).YPos+MapView.AdditionOffsetY = MapView.YPos) then
          begin
            r:=MapView.Addition[i];
            break;
          end;

        end;


        if r<>nil then
        begin
             fDlgPreview.rv1.Room:=r;
        end;
        Invalidate;
end;




end.
