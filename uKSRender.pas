unit uKSRender;

{$mode delphi}

interface

uses
  Classes, Graphics, SysUtils, IntfGraphics,graphtype, lazcanvas, LCLType, fpImage;

procedure ClearIntfImage(var AIntfImage: TLazIntfImage; aWidth, aheight: Integer);
function EmptyIntfImage(AWidth, AHeight: LongInt): TLazIntfImage;
function CreateRegion(aSource: TLazIntfImage; r: TRect): TLazIntfImage;
procedure SaveAsPng(AImg: TLazIntfImage; aFileName: String);
procedure AddLayer(aTarget: TBitmap; aSource: TlazIntfImage; Pos: TPoint);
procedure WriteLayer(var aTarget: TLazIntfImage; aSource: TlazIntfImage; Pos: TPoint);
function CreateIntfImage(APng: TPortableNetworkGraphic): TLazIntfImage;

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

function EmptyIntfImage(AWidth, AHeight: LongInt): TLazIntfImage;
var
  lRawImage: TRawImage;
begin
  lRawImage.Init;
  lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(aWidth,aHeight);
  lRawImage.CreateData(false);
  result := TLazIntfImage.Create(0,0);
  result.SetRawImage(lRawImage);
end;

procedure ClearIntfImage(var AIntfImage: TLazIntfImage; aWidth, aheight: Integer);
var
  TempIntfImg: TLazIntfImage;
begin
  TempIntfImg:=EmptyIntfImage(aWidth,aHeight);
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

end.

