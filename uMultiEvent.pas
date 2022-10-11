
unit uMultiEvent;

{$MODE Delphi}

interface

uses
	Classes;





type
	TMultiEventListener = procedure(Sender: TObject) of object;

	TMultiEvent = class
	protected
		fNumListeners: integer;
		fCapListeners: integer;
		fListener: array of TMultiEventListener;
	public
		constructor Create();
		destructor  Destroy(); override;
		procedure   Clear();

		procedure Add(iListener: TMultiEventListener);
		procedure Del(iListener: TMultiEventListener);

		procedure Trigger(iSender: TObject);

		property NumListeners: integer read fNumListeners;
	end;

















implementation





constructor TMultiEvent.Create();
begin
	inherited Create();
	fNumListeners := 0;
	fCapListeners := 0;
end;





destructor  TMultiEvent.Destroy();
begin
	Clear();
	inherited Destroy();
end;





procedure TMultiEvent.Clear();
begin
	SetLength(fListener, 0);
	fNumListeners := 0;
	fCapListeners := 0;
end;





procedure TMultiEvent.Add(iListener: TMultiEventListener);
var
	cap: integer;
begin
	if (fNumListeners >= fCapListeners) then
	begin
		cap := fNumListeners + fNumListeners div 2 + 8;		// 0, 8, 20, 38, 65, ...
		SetLength(fListener, cap);
		fCapListeners := cap;
	end;
	fListener[fNumListeners] := iListener;;
	fNumListeners := fNumListeners + 1;
end;





procedure TMultiEvent.Del(iListener: TMultiEventListener);
var
	i: integer;
begin
        Exit;
        for i := 0 to fNumListeners - 1 do
	begin
		if (@fListener[i] = @iListener) then
		begin
			fNumListeners := fNumListeners - 1;
			fListener[i] := fListener[fNumListeners];
			Exit;
		end;
	end;		// for i - Listener[]
end;





procedure TMultiEvent.Trigger(iSender: TObject);
var
	i: integer;
begin
	for i := 0 to fNumListeners - 1 do
	begin
		fListener[i](iSender);
	end;
end;





end.
