unit uVersionInfo;

{$mode delphi}

interface

uses
  Classes, SysUtils, Fileinfo;

function ReadVersionInfo(exename: string; var maj, min, rel, build: word): boolean;
function ParseVersionString(iVersion: string; var vmaj, vmin, vrel, vbui: word): boolean;

implementation

function ReadVersionInfo(exename: string; var maj, min, rel, build: word): boolean;
var
  aVersionInfo: TVersionInfo;
begin
  aVersionInfo:=TVersionInfo.Create;
  try
    aVersionInfo.Load(exename);

    maj  :=aVersionInfo.FixedInfo.FileVersion[0];
    min  :=aVersionInfo.FixedInfo.FileVersion[1];
    rel  :=aVersionInfo.FixedInfo.FileVersion[2];
    build:=aVersionInfo.FixedInfo.FileVersion[3];

  finally
    if assigned(aVersionInfo) then aVersionInfo.Free;
  end;

  result:=true;
end;

function ParseVersionString(iVersion: string; var vmaj, vmin, vrel, vbui: word): boolean;
var
  aVersion: TStringList;
begin
  aVersion:=TStringList.Create;
  try
    aVersion.Delimiter:='.';
    aVersion.DelimitedText:=iVersion;
    try
      vmaj:=StrToInt(aVersion[0]);
      vmin:=StrToInt(aVersion[1]);
      vrel:=StrToInt(aVersion[2]);
      vbui:=StrToInt(aVersion[3]);

      result:=true;
    except
      result:=false;
    end;
  finally
    aVersion.Free;
  end;
end;

end.

