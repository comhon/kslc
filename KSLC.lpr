
program KSLC;

{$MODE Delphi}

{$IFDEF UNIX} 
	{$DEFINE UseCThreads}
{$ENDIF}

//{$DEFINE USEJCL}

{%File 'Todo.txt'}

uses
    
	{$IFDEF UseCThreads} cthreads,{$ENDIF}
	Interfaces, 
    Forms,
	Dialogs,
	Controls,
	SysUtils,
	Classes,
  
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uKSRepresentations in 'uKSRepresentations.pas',
  ufrmViewPowerups in 'ufrmViewPowerups.pas' {frmViewPowerups},
  uPowerupList in 'uPowerupList.pas',
  uSettings in 'uSettings.pas',
  uWallSwimChecker in 'uWallSwimChecker.pas',
  uKSObjects in 'uKSObjects.pas',
  ufrmWallSwimList in 'ufrmWallSwimList.pas' {frmWallSwimList},
  uKSLog in 'uKSLog.pas',
  udlgAbout in 'udlgAbout.pas' {dlgAbout},
  udlgSettings in 'udlgSettings.pas' {dlgSettings},
  uKSObjPass in 'uKSObjPass.pas',
  uVectors in 'uVectors.pas',
  uRoomDuplicator in 'uRoomDuplicator.pas',
  udlgDuplicateRooms in 'udlgDuplicateRooms.pas' {dlgDuplicateRooms},
  uPassFilterCalc in 'uPassFilterCalc.pas',
  uKSMapView in 'uKSMapView.pas',
  udlgMultiRoomParam in 'udlgMultiRoomParam.pas' {dlgMultiRoomParam},
  uReachCalc in 'uReachCalc.pas',
  ufrmShiftList in 'ufrmShiftList.pas' {frmShiftList},
  uWebVersionCheckThread in 'uWebVersionCheckThread.pas',
  udlgInstalledLevelList in 'udlgInstalledLevelList.pas' {dlgInstalledLevel},
  ufrmShiftsToHere in 'ufrmShiftsToHere.pas' {frmShiftsToHere},
  uMultiEvent in 'uMultiEvent.pas',
  uKSRoomView in 'uKSRoomView.pas',
  {$IFDEF USEJCL}
  JclDebug in '..\..\Lib\D6\JCL\source\jclDebug.pas',
  JclHookExcept in '..\..\Lib\D6\JCL\source\JclHookExcept.pas',
  {$ENDIF}
  uKSRender;



{$R *.res}





var
	{$IFDEF USEJCL}
	gProcessingException: boolean = false;
	{$ENDIF}
	dlgOpen: TOpenDialog;
	dlg: TdlgInstalledLevelList;




{$IFDEF USEJCL}

// debugging functions using JCL are copied over from SiteFlow

procedure KSLCStackTrace(); stdcall;
var
	sl: TJclStackInfoList;
	s: TStrings;
	i: integer;
begin
	sl := JclCreateStackList(false, 0, nil);		// do not free this object!
	s := TStringList.Create();
	try
		sl.AddToStrings(s, true, true);
		sl := JclCreateStackList(true, 0, nil);		// do not free this object!
		gLog.LogUpd(LOG_FATAL, '--- raw trace ---');
		sl.AddToStrings(s, true, false);
		gLog.LogUpd(LOG_FATAL, 'Stack trace:');
		gLog.AddIndent();
		try
			for i := 0 to s.Count - 1 do
			begin
				gLog.LogUpd(LOG_FATAL, s[i]);
			end;
		finally
			gLog.DelIndent();
		end;
	finally
		s.Free;
	end;
end;





procedure KSLCExceptionNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
var
	el: TJclExceptFrameList;
	s: TStringList;
	i: integer;
begin
	if (gProcessingException) then
	begin
		// already processing one exception, re-entry not acceptable, don't cycle!
		Exit;
	end;

	gProcessingException := true;
	try
		gLog.Log(LOG_FATAL, 'KSLCExceptionNotify: exception received!');
		gLog.AddIndent();
		try
			gLog.LogUpd(LOG_FATAL, 'Addr: [' + IntToHex(integer(ExceptAddr), 8) + ']');
			gLog.LogUpd(LOG_FATAL, GetLocationInfoStr(ExceptAddr, True));
			gLog.LogUpd(LOG_FATAL, string(Exception(ExceptObj).ClassName));
			gLog.LogUpd(LOG_FATAL, Exception(ExceptObj).Message);
			el := JclCreateExceptFrameList(0);		// do not free this object!
			s := TStringList.Create();
			try
				el.AddToStrings(s, True);
				gLog.LogUpd(LOG_FATAL, 'Exception Frame trace:');
				gLog.AddIndent();
				try
					for i := 0 to s.Count - 1 do
					begin
						gLog.LogUpd(LOG_FATAL, s[i]);
					end;
				finally
					gLog.DelIndent();
					gLog.LogUpd(LOG_FATAL, 'Exception Frame trace end');
				end;
			finally
				s.Free;
			end;
			KSLCStackTrace();
		finally
			gLog.DelIndent();
			gLog.LogUpd(LOG_FATAL, 'KSLCExceptionNotify end');
		end;

		if (MessageDlg('KSLC has crashed. The author asks that You post the contents of the Log tab at the KSLC bugreport forum.'#13#10#13#10'Would You like to open the KSLC bug forum in a browser now?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
		begin
			OpenURL('http://xoft.cz/forum');
			if (Assigned(frmMain) and Assigned(frmMain.pcMain) and Assigned(frmMain.tsLog)) then
			begin
				frmMain.pcMain.ActivePage := frmMain.tsLog;
			end;
		end;
	finally
		gProcessingException := false;
	end;
end;

{$ENDIF}




begin
  Application.Initialize();
  InitializeSettings;

	gLog := TKSLog.Create(LOG_INFO);
	gLog.Log(LOG_INFO, 'Initializing');

    {$IFDEF JCLENA}
        // initialize JCL debugging:
	JclStackTrackingOptions := [stStack, stAllModules, stExceptFrame];
	JclHookExceptions();
	JclAddExceptNotifier(KSLCExceptionNotify);
    {$ENDIF}

	Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmViewPowerups, frmViewPowerups);
  Application.CreateForm(TfrmWallSwimList, frmWallSwimList);
  Application.CreateForm(TfrmShiftList, frmShiftList);
  Application.CreateForm(TfrmShiftsToHere, frmShiftsToHere);
  if (gKSDir = '') then
	begin
		gLog.Log(LOG_WARNING, 'Knytt Stories directory not set, asking the user');
		dlgOpen := TOpenDialog.Create(frmMain);
		try
			dlgOpen.InitialDir := '.';
			dlgOpen.Title := 'Where is your Knytt Stories exe:';
			//dlgOpen.Filter := 'Knytt Stories Executable|Knytt Stories.exe';
			if (dlgOpen.Execute()) then
			begin
				gKSDir := IncludeTrailingPathDelimiter(ExtractFilePath(dlgOpen.Filename));
				gLog.Log(LOG_INFO, 'Knytt Stories directory set to "' + gKSDir + '"');
			end
			else
			begin
				gLog.Log(LOG_WARNING, 'Knytt Stories directory not set by user');
			end;
		finally
			dlgOpen.Free();
		end;
		frmMain.Show();
		frmMain.BringToFront();
		Application.BringToFront();
	end;
	gLog.Log(LOG_INFO, 'Knytt Stories directory is "' + gKSDir + '"');
	gLog.Log(LOG_INFO, 'Initialization complete, starting main window');
	gSettings.SetForms();

	// open initial level
	dlg := TdlgInstalledLevelList.Create(nil);
	try
		dlg.Caption := 'Open level:';
		case dlg.ShowModal() of
			mrKSDirNotFound:
			begin
				ShowMessage('KS folder not found. Please use View->Settings to set KS folder.'#13#10#13#10'Normal file open will commence now.');
				frmMain.actFileOpenExecute(nil);
				Exit;
			end;

			mrKSDirNoWorlds,mrNoWorldFound:
			begin
				ShowMessage('There are no levels installed in the KS folder. Please use View->Settings to check and set KS folder.'#13#10#13#10'Normal file open will commence now.');
				frmMain.actFileOpenExecute(nil);
				Exit;
			end;

			mrCancel:
			begin
				Exit;
			end;
		end;

		frmMain.OpenFile(dlg.Path + 'Map.bin');
	finally
		dlg.Release();
	end;

	Application.Run();
	gSettings.GetForms();
    FinalizeSettings;
end.
