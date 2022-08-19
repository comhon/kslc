unit uKSObjectChooser;

{$MODE Delphi}

interface

uses
	SysUtils,
	Classes,
	Graphics,
	Controls;





type
	TKSObjectChooser = class(TGraphicControl)
	protected
		fBank: integer;
		fObj: integer;

		BankImage: TPortableNetworkGraphic;
		ObjImage: TPortableNetworkGraphic;

		fShouldReloadBankImage: boolean;
		fShouldReloadObjImage: boolean;

		procedure fSetBank(iVal: integer);
		procedure fSetObj (iVal: integer);

		procedure CanvasTextOutCentered(X, Y: integer; aText: string);
		procedure Paint(); override;
		procedure ReloadBankImage();
		procedure ReloadObjImage();
		procedure ReloadImage(var Image: TPortableNetworkGraphic; iFileName: string);

		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

		property OnMouseMove;
		property OnMouseUp;
		property OnMouseDown;

	public

		OnChange: TNotifyEvent;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy(); override;

		function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): boolean; override;		// containing controls must call this in order to provide mousewheel functionality

	published
	
		property Bank: integer read fBank write fSetBank;
		property Obj:  integer read fObj  write fSetObj;

		property Align;
		property Anchors;
		property Color;
	end;





procedure Register();




















implementation

uses
	uKSRepresentations;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// globals:

procedure Register();
begin
	RegisterComponents('KSLC', [TKSObjectChooser]);
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSObjectChooser:

constructor TKSObjectChooser.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	BankImage := TPortableNetworkGraphic.Create();
	ObjImage := TPortableNetworkGraphic.Create();
	fBank := 0;
	fObj := 0;
	Width := 100;
	Height := 240;
	Constraints.MinHeight := 240;
	Constraints.MinWidth := 100;
end;





destructor TKSObjectChooser.Destroy();
begin
	// TODO
	inherited Destroy();
end;





procedure TKSObjectChooser.fSetBank(iVal: integer);
begin
	if (fBank = iVal) then
	begin
		Exit;
	end;
	fBank := iVal;
	fShouldReloadBankImage := true;
	fShouldReloadObjImage  := true;
	Invalidate();
end;





procedure TKSObjectChooser.fSetObj (iVal: integer);
begin
	if (fObj = iVal) then
	begin
		Exit;
	end;
	fObj := iVal;
	fShouldReloadObjImage := true;
	Invalidate();
end;




procedure TKSObjectChooser.CanvasTextOutCentered(X,Y: integer; aText: string);
begin
	Canvas.TextOut(X-Canvas.TextWidth(aText)div 2,Y,aText);
end;

procedure TKSObjectChooser.Paint();
begin
	if (csDesigning in ComponentState) then
	begin
		Canvas.Brush.Color := Color;
		Canvas.Pen.Style := psDash;
		Canvas.Pen.Color := 0;
		Canvas.Rectangle(Rect(0, 0, Width, Height));
		Exit;
	end;

	// reload images: if asked to:
	if (fShouldReloadBankImage) then
	begin
		ReloadBankImage();
	end;
	if (fShouldReloadObjImage) then
	begin
		ReloadObjImage();
	end;

	Canvas.Brush.Color := Color;
	Canvas.FillRect(Rect(0, 0, Width, Height));
	Canvas.Font.Size := -18;
	Canvas.Font.Name := 'Arial';
	Canvas.Brush.Style := bsClear;

	CanvasTextOutCentered(Width div 2, 1, 'Bank: ' + IntToStr(fBank));
	if (Assigned(BankImage)) then
	begin
		Canvas.Draw((Width - BankImage.Width) div 2, 70 - BankImage.Height div 2, BankImage);
	end;

	CanvasTextOutCentered(Width div 2, 121, 'Obj: ' + IntToStr(fObj));
	if (Assigned(ObjImage)) then
	begin
		Canvas.Draw((Width - ObjImage.Width) div 2, 190 - ObjImage.Height div 2, ObjImage);
	end;
end;





procedure TKSObjectChooser.ReloadBankImage();
begin
	ReloadImage(BankImage, ConcatPaths([gKSDir,'Data','Objects','Bank' + IntToStr(fBank),'Bank.png']));
	fShouldReloadBankImage := false;
end;





procedure TKSObjectChooser.ReloadObjImage();
begin
	ReloadImage(ObjImage, ConcatPaths([gKSDir,'Data','Objects','Bank' + IntToStr(fBank),'Object'+ IntToStr(fObj) + '.png']));
	fShouldReloadObjImage := false;
end;





procedure TKSObjectChooser.ReloadImage(var Image: TPortableNetworkGraphic; iFileName: string);
begin
	if not(FileExists(iFileName)) then
	begin
		Image.Clear;
		Exit;
	end;
	try
		Image.LoadFromFile(iFileName);
	except
		Image.Clear;
	end;
end;





procedure TKSObjectChooser.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	Add: integer;
begin
	Add := 0;
	if (Button = mbLeft) then
	begin
		Add := 1;
	end
	else if (Button = mbRight) then
	begin
		Add := -1;
	end;
	if (ssShift in Shift) then
	begin
		Add := Add * 8;
	end;

	if (Y < 120) then
	begin
		Bank := (256 + fBank + Add) mod 256;
	end
	else
	begin
		Obj := (256 + fObj + Add) mod 256;
	end;

	inherited MouseDown(Button, Shift, X, Y);
end;





function TKSObjectChooser.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): boolean; 
var
	Add: integer;
begin
	// we do NOT want to call inherited!
	Add := 0;
	if (WheelDelta > 0) then
	begin
		Add := 1;
	end
	else if (WheelDelta < 0) then
	begin
		Add := -1;
	end;
	if (ssShift in Shift) then
	begin
		Add := Add * 8;
	end;

	if (MousePos.Y < 120) then
	begin
		Bank := (256 + fBank + Add) mod 256;
	end
	else
	begin
		Obj := (256 + fObj + Add) mod 256;
	end;
	Result := true;
end;





end.
