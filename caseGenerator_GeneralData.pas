unit caseGenerator_GeneralData;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Controls.Presentation;

type
  TformNewCase = class(TForm)
    Label1: TLabel;
    edName: TEdit;
    Label3: TLabel;
    edFocus: TEdit;
    Label4: TLabel;
    memoShortDescription: TMemo;
    btnConfirm: TButton;
    Label5: TLabel;
    memoDescription: TMemo;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FOperation: Integer;
    procedure SetOperation(const Value: Integer);
    { Private declarations }
    property Operation: Integer read FOperation write SetOperation;
  public
    { Public declarations }
  // 1 if the Save and Close button has been pressed, 0 if Cancel button has been chosen
     function ShowWithParamater(): Integer;
  end;

var
  formNewCase: TformNewCase;

implementation

{$R *.fmx}

procedure TformNewCase.btnCancelClick(Sender: TObject);
begin
  formNewCase.Close;
  SetOperation(0); // cancel, without creating a new case
end;

procedure TformNewCase.btnConfirmClick(Sender: TObject);
begin
  formNewCase.Close;
  SetOperation(1); // Save, creating a new case

end;

procedure TformNewCase.FormShow(Sender: TObject);
begin
  memoDescription.WordWrap := true;
  SetOperation(0); // cancel, without creating a new case: default operation
end;

procedure TformNewCase.SetOperation(const Value: Integer);
begin
  FOperation := Value;
end;

function TformNewCase.ShowWithParamater: Integer;
begin
  formNewCase.ShowModal();
  Result := FOperation;
end;

end.
