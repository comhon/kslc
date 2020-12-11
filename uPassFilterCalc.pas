
unit uPassFilterCalc;

interface

uses
	uKSRepresentations;





type
	TPassFilterCalc = class
	public
		Level: TKSLevel;
		CapRooms, NumRooms: integer;

		constructor Create(iLevel: TKSLevel);
		
	end;

















implementation





constructor TPassFilterCalc.Create(iLevel: TKSLevel);
begin
	inherited Create();
	Level := iLevel;
	NumRooms := 0;
	CapRooms := 0;
end;





end.

