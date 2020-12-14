unit udlgAbout;

{$MODE Delphi}

interface

uses
	Windows,
	Messages,
	SysUtils,
	Variants,
	Classes,
	Graphics,
	Controls,
	Forms,
	Dialogs,
	StdCtrls;





type

  { TdlgAbout }

  TdlgAbout = class(TForm)
    Label1: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    tlGoToKS: TButton;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    tlGoToSF: TButton;
    tlGoToKSLC: TButton;
    Label13: TLabel;
    Label14: TLabel;

    procedure tlGoToKSClick(Sender: TObject);
    procedure tlGoToSFClick(Sender: TObject);
    procedure tlGoToKSLCClick(Sender: TObject);
	public
	end;





















implementation

uses
	ShellAPI;





{$R *.lfm}





procedure TdlgAbout.tlGoToKSClick(Sender: TObject);
begin
	ShellExecute(Handle, nil, 'http://egomassive.com/ks/', '', '', SW_SHOWNORMAL);
end;




procedure TdlgAbout.tlGoToSFClick(Sender: TObject);
begin
	ShellExecute(Handle, nil, 'http://nifflas.lpchip.nl/', '', '', SW_SHOWNORMAL);
end;





procedure TdlgAbout.tlGoToKSLCClick(Sender: TObject);
begin
	ShellExecute(Handle, nil, 'http://xoft.cz/KSLC', '', '', SW_SHOWNORMAL);
end;





end.
