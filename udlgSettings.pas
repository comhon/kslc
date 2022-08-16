unit udlgSettings;

{$MODE Delphi}

interface

uses
	Messages,
	SysUtils,
	Variants,
	Classes,
	Graphics,
	Controls,
	Forms,
	Dialogs,
	Buttons,
	ExtCtrls,
	StdCtrls;




	
type
  TdlgSettings = class(TForm)
    pGameFolder: TPanel;
    pTl: TPanel;
    tlOK: TSpeedButton;
    tlCancel: TSpeedButton;
    Label1: TLabel;
    eKSDir: TEdit;
    tlBrowseKSDir: TSpeedButton;
    pWebUpdate: TPanel;
    Label2: TLabel;
    chbAllowWebVersionCheck: TCheckBox;
    chbAllowWebStats: TCheckBox;
    mStatPlea: TMemo;
    procedure tlOKClick(Sender: TObject);
    procedure tlCancelClick(Sender: TObject);
    procedure tlBrowseKSDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chbAllowWebVersionCheckClick(Sender: TObject);
	end;





implementation

uses
	uKSLog,
	uSettings,
	ufrmMain,
	uKSRepresentations;





{$R *.lfm}





procedure TdlgSettings.tlOKClick(Sender: TObject);
begin
	gKSDir := eKSDir.Text;
	gKSDir := IncludeTrailingPathDelimiter(gKSDir);
	gLog.Log(LOG_INFO, 'Knytt Stories directory set to "' + gKSDir + '"');
	gSettings.AllowWebVersionCheck := chbAllowWebVersionCheck.Checked;
	gSettings.AllowWebStats := chbAllowWebStats.Checked;
	ModalResult := mrOK;
end;





procedure TdlgSettings.tlCancelClick(Sender: TObject);
begin
	ModalResult := mrCancel;
end;




procedure TdlgSettings.tlBrowseKSDirClick(Sender: TObject);
var
	dlgOpen: TOpenDialog;
begin
	dlgOpen := TOpenDialog.Create(Self);
	try
			dlgOpen.InitialDir := '.';
			dlgOpen.Title := 'Where is your Knytt Stories exe:';
			dlgOpen.Filter := 'Knytt Stories Executable|Knytt Stories.exe';
			if (dlgOpen.Execute()) then
			begin
				eKSDir.Text:=IncludeTrailingPathDelimiter(dlgOpen.Filename);
			end
	finally
		dlgOpen.Free();
	end;
end;





procedure TdlgSettings.FormCreate(Sender: TObject);
begin
	eKSDir.Text := gKSDir;
	chbAllowWebVersionCheck.Checked := gSettings.AllowWebVersionCheck;
	chbAllowWebVersionCheckClick(Sender);
	chbAllowWebStats.Checked := gSettings.AllowWebStats;

	tlOK.Glyph.LoadFromResourceName(HInstance, 'BBOK');
	tlCancel.Glyph.LoadFromResourceName(HInstance, 'BBCANCEL');
end;





procedure TdlgSettings.chbAllowWebVersionCheckClick(Sender: TObject);
begin
	// Only allow webstats when webcheck is enabled:
	chbAllowWebStats.Enabled := chbAllowWebVersionCheck.Checked;
	chbAllowWebStats.Checked := chbAllowWebStats.Checked and chbAllowWebStats.Enabled;
end;





end.
