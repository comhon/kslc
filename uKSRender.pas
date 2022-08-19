unit uKSRender;

{$mode delphi}

interface

uses
  Classes, Graphics, SysUtils, IntfGraphics,graphtype, lazcanvas, LCLType, fpImage;

type

  { TKSIntfImage }

  TKSIntfImage = class
  private
    FWidth, FHeight: Integer;
    FEmptyIntfImg: TLazIntfImage;
  public
    Obj: TLazIntfImage;

    constructor Create(Width, Height: Integer); overload;
    constructor Create(var PngImage: TPortableNetworkGraphic); overload;
    destructor Destroy; override;
    procedure Clear;

    procedure Draw(var KSIntfImage: TKSIntfImage; Left,Top: Integer); overload;
    procedure Draw(var Bitmap: TBitmap; Left,Top: Integer); overload;


    property Width: integer read FWidth;
    property Height: integer read FHeight;

  end;

procedure ClearIntfImage(var AIntfImage: TLazIntfImage; aWidth, aheight: Integer; AClearMem:boolean=false);
function EmptyIntfImage(AWidth, AHeight: LongInt;AClearMem:boolean=false): TLazIntfImage;
function CreateRegion(aSource: TLazIntfImage; r: TRect): TLazIntfImage;
procedure SaveAsPng(AImg: TLazIntfImage; aFileName: String);
procedure AddLayer(aTarget: TBitmap; aSource: TlazIntfImage; Pos: TPoint);
procedure WriteLayer(var aTarget: TLazIntfImage; aSource: TlazIntfImage; Pos: TPoint);
function CreateIntfImage(APng: TPortableNetworkGraphic): TLazIntfImage;
procedure DrawIntfImage(AImage: TLazIntfImage; Canvas: TCanvas; aPosition: TPoint);
function CreateMaskedBitmap(AImage: TLazIntfImage): TBitmap;

implementation

procedure WriteLayer(var aTarget: TLazIntfImage; aSource: TlazIntfImage; Pos: TPoint);
var
  TempBitmap: TBitmap;
  ImgHandle,ImgMaskHandle: HBitmap;
begin
  TempBitmap:=TBitmap.Create;
  ImgMaskHandle:=TempBitmap.MaskHandle;

  aTarget.CreateBitmaps(ImgHandle,ImgMaskHandle,true);
  TempBitmap.Handle:=ImgHandle;
  TempBitmap.MaskHandle:=ImgMaskHandle;

  AddLayer(TempBitmap,aSource,Pos);

  aTarget:=TempBitmap.CreateIntfImage;
end;

procedure AddLayer(aTarget: TBitmap; aSource: TlazIntfImage; Pos: TPoint);
var
  SrcIntfImg, TempIntfImg: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  FadeStep: Integer;
  px, py: Integer;
//  CurColor: TFPColor;
  TempBitmap: TBitmap;
  DestCanvas: TLazCanvas;
begin
  TempBitmap:=TBitmap.Create;

  aSource.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
  TempBitmap.Handle:=ImgHandle;
  TempBitmap.MaskHandle:=ImgMaskHandle;

  aTarget.Canvas.Draw(Pos.x,Pos.y,TempBitmap);
  aTarget.Canvas.Free;

  TempBitmap.Free;
end;

function CreateIntfImage(APng: TPortableNetworkGraphic): TLazIntfImage;
var
  dy,dx: integer;
  w,h: integer;
  col: TFPColor;
  c: TColor;
  SrcIntfImg: TLazIntfImage;
  TempIntfImg: TLazIntfImage;
begin
  SrcIntfImg:=APng.CreateIntfImage;

  if not SrcIntfImg.HasTransparency then
  begin

    w:=APng.Width;
    h:=APng.Height;

    //create a new image with transparency, and copy pixel data
    TempIntfImg:=EmptyIntfImage(w,h);
    TempIntfImg.CopyPixels(SrcIntfImg,0,0,False);

    SrcIntfImg.Free;

    // replace fuchsia pixels
    for dy := 0 to h - 1 do begin
      for dx := 0 to w - 1 do begin
        col := TempIntfImg.Colors[dx, dy];
        c := FPColorToTColor(col);
        if (c = clFuchsia) then
          TempIntfImg.Colors[dx, dy] := colTransparent;
      end;
    end;
    result:=TempIntfImg;
  end
  else
  begin
    result:=SrcIntfImg;
  end;
end;

function EmptyIntfImage(AWidth, AHeight: LongInt; AClearMem: boolean = false): TLazIntfImage;
var
  lRawImage: TRawImage;
begin
  lRawImage.Init;
  lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(aWidth,aHeight);
  lRawImage.CreateData(AClearMem);
  result := TLazIntfImage.Create(0,0);
  result.SetRawImage(lRawImage);
end;

procedure ClearIntfImage(var AIntfImage: TLazIntfImage; aWidth, aheight: Integer; AClearMem: boolean = false);
var
  TempIntfImg: TLazIntfImage;
begin
  TempIntfImg:=EmptyIntfImage(aWidth,aHeight,AClearMem);
  AIntfImage.CopyPixels(TempIntfImg,0,0);
  TempIntfImg.Free;
end;

function CreateRegion(aSource: TLazIntfImage; r: TRect): TLazIntfImage;
begin
  result:=EmptyIntfImage(r.Width,r.Height);
  result.CopyPixels(aSource,-r.Left,-r.Top);
end;

procedure SaveAsPng(AImg: TLazIntfImage; aFileName: String);
var
  png: TPortableNetworkGraphic;
begin
  Exit;
  png := TPortableNetworkGraphic.Create;
  png.LoadFromIntfImage(AImg);
  png.SaveToFile(aFileName);
  png.Free;
end;

function CreateMaskedBitmap(AImage: TLazIntfImage): TBitmap;
var
  TempBitmap: TBitmap;
  ImgHandle,ImgMaskHandle: HBitmap;
begin
  if AImage = nil then
  begin
    result:=nil;
    Exit;
  end;

  TempBitmap:=TBitmap.Create;
  AImage.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
  TempBitmap.Handle:=ImgHandle;
  TempBitmap.MaskHandle:=ImgMaskHandle;

  result:=TempBitmap;
end;

procedure DrawIntfImage(AImage: TLazIntfImage; Canvas: TCanvas; aPosition: TPoint);
var
  TempBitmap: TBitmap;
begin
  TempBitmap:=CreateMaskedBitmap(aImage);
  Canvas.Draw(aPosition.X,aPosition.Y,TempBitmap);
  TempBitmap.Free;
end;

{ TKSIntfImage }

constructor TKSIntfImage.Create(Width, Height: Integer);
begin
  FWidth:=Width;
  FHeight:=Height;
  Obj:=EmptyIntfImage(Width,Height,False);

end;

constructor TKSIntfImage.Create(var PngImage: TPortableNetworkGraphic);
begin
  FWidth:=PNGImage.Width;
  FHeight:=PNGImage.Height;
  Obj:=CreateIntfImage(PngImage);
  FEmptyIntfImg:=nil;
end;

destructor TKSIntfImage.Destroy;
begin
  Obj.Free;
  FEmptyIntfImg.Free;
end;

procedure TKSIntfImage.Clear;
begin
  if not Assigned(FEmptyIntfImg) then
  begin
    FEmptyIntfImg:=EmptyIntfImage(FWidth,FHeight,False);
  end;

  Obj.CopyPixels(FEmptyIntfImg,0,0,True);
end;

procedure TKSIntfImage.Draw(var KSIntfImage: TKSIntfImage; Left, Top: Integer);
begin
  WriteLayer(Obj,KSIntfImage.Obj,TPoint.Create(Left,Top));
end;

procedure TKSIntfImage.Draw(var Bitmap: TBitmap; Left, Top: Integer);
begin
  DrawIntfImage(Obj,Bitmap.Canvas,TPoint.Create(0,0))
end;

end.

