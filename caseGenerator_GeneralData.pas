unit caseGenerator_GeneralData;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Controls.Presentation;

type
  TformNewCase = class(TForm)
    Label1: TLabel;
    editName: TEdit;
    Label3: TLabel;
    editFocus: TEdit;
    Label4: TLabel;
    memoShortDescription: TMemo;
    btnConfirm: TButton;
    Label5: TLabel;
    memoDescription: TMemo;
    procedure FormShow(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formNewCase: TformNewCase;

implementation

{$R *.fmx}

procedure TformNewCase.btnConfirmClick(Sender: TObject);
begin
  formNewCase.Close;
end;

procedure TformNewCase.FormShow(Sender: TObject);
begin
  memoDescription.WordWrap := true;
end;

end.
