unit uKSGraphic;

{$mode delphi}

interface

uses
  Classes, Graphics, SysUtils, IntfGraphics,graphtype, lazcanvas, LCLType, fpImage, uKSRender;

type

  { TKSImage }

  TKSImage = class
  private
    FHeight: integer;
    FWidth: integer;
    PNG: TPortableNetworkGraphic;
    Intf: TKSIntfImage;
  public
    constructor Create; overload;
    constructor Create(AWidth, AHeight: Integer); overload;
    destructor Destroy;
    procedure LoadFromPNGFile(aFileName: string);
    function GetLazImg: TLazIntfImage;
    procedure Clear;
    procedure Draw(ACanvas:TCanvas;XPos,YPos: integer);
    procedure AddLayer(ABitmap: TBitmap; XPos,YPos: integer); overload;
    procedure AddLayer(AKSImage: TKSImage; XPos,YPos: integer); overload;
    property Width: integer read FWidth;
    property Height: integer read FHeight;


  end;

implementation

{ TKSImage }

constructor TKSImage.Create;
begin
  PNG := nil;
  Intf:= nil;
  FWidth:=0;
  FHeight:=0;
end;

constructor TKSImage.Create(AWidth,AHeight: Integer);
begin
  Create;
  Intf:=TKSIntfImage.Create(AWidth,AHeight);
  FWidth:=AWidth;
  FHeight:=AHeight;
end;

destructor TKSImage.Destroy;
begin
  if Assigned(PNG) then
  begin
    PNG.Free;
  end;
  if Assigned(Intf) then
  begin
    Intf.Free;
  end;
end;

procedure TKSImage.LoadFromPNGFile(aFileName: string);
begin
  if not(Assigned(PNG)) then
  begin
    PNG:=TPortableNetworkGraphic.Create;
  end;
  PNG.LoadFromFile(aFileName);
  FWidth:=PNG.Width;
  FHeight:=PNG.Height;
  if Assigned(Intf) then
  begin
    Intf.Clear;
    Intf.Free;
  end;
  Intf:=TKSIntfImage.Create(PNG);
end;

function TKSImage.GetLazImg: TLazIntfImage;
begin
  result:=Intf.Obj;
end;

procedure TKSImage.Clear;
begin
  if Assigned(PNG) then
  begin
    PNG.Clear;
  end;
  if Assigned(Intf) then
  begin
    Intf.Clear;
  end;
end;

procedure TKSImage.Draw(ACanvas: TCanvas; XPos, YPos: integer);
begin
  if (Assigned(Intf.Obj)) then
  begin
    DrawIntfImage(Intf.Obj,ACanvas,TPoint.Create(XPos,YPos));
  end;
end;

procedure TKSImage.AddLayer(ABitmap: TBitmap; XPos,YPos: integer);
begin
	Intf.Obj.CopyPixels(ABitmap.CreateIntfImage,XPos,YPos);
end;

procedure TKSImage.AddLayer(AKSImage: TKSImage; XPos, YPos: integer);
begin
	WriteLayer(Intf.Obj,AKSImage.GetLazImg,TPoint.Create(XPos,YPos));
end;

end.

