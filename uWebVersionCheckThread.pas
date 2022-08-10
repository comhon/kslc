unit uWebVersionCheckThread;

{$MODE Delphi}

interface

uses
        {$DEFINE UseCThreads} {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
        //*h WinInet,
        Classes;






type
	TWebVersionRecvProc = procedure(Sender: TObject; WebVersion: string) of object;



	TWebVersionCheckThread = class(TThread)
	public
		constructor Create(iOnWebVersionReceived: TWebVersionRecvProc; iLevelName, iAuthorName, iCurrentVersion: string);

	private
		LevelName: string;
		AuthorName: string;
		CurrentVersion: string;

		WebVersion: string;
		
		OnReceived: TWebVersionRecvProc;

		procedure RecvNotify();	
	protected
		procedure Execute; override;
	end;


















implementation

uses
	SysUtils;
	



const
	DOWNLOAD_BUFFER_SIZE = 4096;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// globals:

function URLEncode(const S: string): string;
var
	Idx: Integer; // loops thru characters in string
begin
	Result := '';
	for Idx := 1 to Length(S) do
	begin
		case S[Idx] of
			'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
			begin
				Result := Result + S[Idx];
			end;
			
			' ':
			begin
				Result := Result + '+'
			end;

			else
			begin
				Result := Result + '%' + SysUtils.IntToHex(Ord(S[Idx]), 2);
			end;
		end;
	end;		// for Idx - S[]
end;




///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TWebVersionCheckThread

constructor TWebVersionCheckThread.Create(iOnWebVersionReceived: TWebVersionRecvProc; iLevelName, iAuthorName, iCurrentVersion: string);
begin
	inherited Create(true);
	LevelName := iLevelName;
	AuthorName := iAuthorName;
	CurrentVersion := iCurrentVersion;
	OnReceived := iOnWebVersionReceived;
	Resume();
end;





procedure TWebVersionCheckThread.Execute;
var
	URL: string;
	//hInet, hData: HINTERNET;
	buffer: array[0..DOWNLOAD_BUFFER_SIZE] of char;
	dwRead: cardinal;
	pos: cardinal;
begin
        (*//*h
        URL := 'http://xoft.cz/KSLC/version_check.php?level_name=' + URLEncode(LevelName) + '&author_name=' + URLEncode(AuthorName) + '&version=' + CurrentVersion;

	hInet := InternetOpen(PChar('KSLC' + CurrentVersion), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
	if not(Assigned(hInet)) then
	begin
		Exit;
	end;

	hData := InternetOpenUrl(hInet, PChar(URL), nil, 0, 0, 0);
	if not(Assigned(hData)) then
	begin
		InternetCloseHandle(hInet);
		Exit;
	end;

	pos := 0;
	while ((pos < DOWNLOAD_BUFFER_SIZE) and (InternetReadFile(hData, @(buffer[pos]), DOWNLOAD_BUFFER_SIZE - pos, dwRead))) do
	begin
		if (dwRead = 0) then
		begin
			break;
		end;
		pos := pos + dwRead;
	end;		// while InternetReadFile
	buffer[pos] := #0;
	InternetCloseHandle(hData);
	InternetCloseHandle(hINet);
        WebVersion := string(buffer);
        *)
        WebVersion := 'ERROR';

        Synchronize(RecvNotify)

end;





procedure TWebVersionCheckThread.RecvNotify();
begin
	if Assigned(OnReceived) then
	begin
		OnReceived(Self, WebVersion);
	end;
end;





end.
 
