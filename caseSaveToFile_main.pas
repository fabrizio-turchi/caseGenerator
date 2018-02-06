unit caseSaveToFile_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, StrUtils, FMX.Layouts, FMX.ListBox;

type
  TForm2 = class(TForm)
    btnSave: TButton;
    lbLines: TListBox;
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.btnSaveClick(Sender: TObject);
var
  Uid: TGuid;
  Result: HResult;
  uuidGenerated, path: String;
  Value: HResult;
begin
  Value := CreateGUID(Uid);
  path := GetCurrentDir + '/';
  uuidGenerated := GuidToString(Uid);
  uuidGenerated := copy(uuidGenerated, 2, length(uuidGenerated) - 2);
  // get rid of of { begining and end of the uuid
  ShowMessage('current dir: ' + path);
  ShowMessage('file:  ' + path  + 'fabri-CASE.json');
  lbLines.Items.SaveToFile(path  + 'fabri-CASE.json');
end;


end.
