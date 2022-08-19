
unit uKSLog;

{$MODE Delphi}

interface

uses
        {$DEFINE UseCThreads} {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
        Classes,
	SyncObjs,
        uMultiEvent;





const
	LOG_INFO = 1000;
	LOG_WARNING = 500;
	LOG_ERROR = 100;
	LOG_FATAL = 1;





type
	TKSLog = class(TThread)
	protected
		Level: integer;
		Indent: integer;
		CS: TCriticalSection;
		ShouldUpdate: boolean;

		procedure Execute(); override;		// Log thread

	public
		Items: TStrings;

		OnUpdate: TMultiEvent;		// triggerred with CS locked, then Items are cleared automatically

		constructor Create(iLevel: integer); reintroduce;
		destructor Destroy(); override;
		procedure Clear();

		procedure Log   (iLevel: integer; txt: string);
		procedure LogUpd(iLevel: integer; txt: string);

		procedure AddIndent();
		procedure DelIndent();
		
		procedure Update();		// GUI thread
	end;













implementation

uses
	SysUtils;





constructor TKSLog.Create(iLevel: integer);
begin
        Level := iLevel;
	Items := TStringList.Create();
	CS := TCriticalSection.Create();
	ShouldUpdate := false;
	OnUpdate := TMultiEvent.Create();
	Resume();
end;



destructor TKSLog.Destroy();
begin
	Terminate();
	ShouldUpdate := true;
	WaitFor();
	Clear();
	Items.Free();
	Items := nil;
	CS.Free();
	CS := nil;
	OnUpdate.Free();
	
	inherited Destroy();
end;





procedure TKSLog.Clear();
begin
	CS.Enter();
	try
		Items.Clear();
	finally
		CS.Leave();
	end;
end;





procedure TKSLog.Log(iLevel: integer; txt: string);
begin
	if (iLevel > Level) then Exit;
	CS.Enter();
	try
		Items.Add(IntToStr(iLevel) + StringOfChar(#9, Indent + 1) + txt);
		ShouldUpdate := true;
	finally
		CS.Leave();
	end;
end;





procedure TKSLog.LogUpd(iLevel: integer; txt: string);
begin
	if (iLevel > Level) then Exit;
	CS.Enter();
	try
		Items.Add(IntToStr(iLevel) + StringOfChar(#9, Indent + 1) + txt);
		ShouldUpdate := true;
	finally
		CS.Leave();
	end;
	Update();
end;





procedure TKSLog.AddIndent();
begin
	CS.Enter();
	try
		Indent := Indent + 1;
	finally
		CS.Leave();
	end;
end;





procedure TKSLog.DelIndent();
begin
	CS.Enter();
	try
		if (Indent > 0) then
		begin
			Indent := Indent - 1;
		end;
	finally
		CS.Leave();
	end;
end;





procedure TKSLog.Execute();		// Log thread
begin
	while (not(Terminated)) do
	begin
		Sleep(500);
		if (ShouldUpdate) then
		begin
			Synchronize(Update);
		end;
	end;
end;





procedure TKSLog.Update();		// GUI thread
begin
	CS.Enter();
	try
		OnUpdate.Trigger(Self);
		if (OnUpdate.NumListeners > 0) then
		begin
			Items.Clear();
		end;
		ShouldUpdate := false;
	finally
		CS.Leave();
	end;
end;





end.
