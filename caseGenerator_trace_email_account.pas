unit caseGenerator_trace_email_account;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation;

type
  TformTraceEmailAccount = class(TForm)
    Label1: TLabel;
    lbPhoneAccount: TListBox;
    edEmail: TEdit;
    Label3: TLabel;
    btnClose: TButton;
    btnAddPhoneAccount: TButton;
    btnDeletePhoneAccount: TButton;
    btnCancel: TButton;
    btnModifyTrace: TButton;
    procedure btnAddPhoneAccountClick(Sender: TObject);
    procedure btnDeletePhoneAccountClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyTraceClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function prepareTrace: String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceEmailAccount: TformTraceEmailAccount;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceEmailAccount.btnDeletePhoneAccountClick(Sender: TObject);
begin
  lbPhoneAccount.Items.Delete(lbPhoneAccount.ItemIndex);
end;


procedure TformTraceEmailAccount.btnModifyTraceClick(Sender: TObject);
begin
  if lbPhoneAccount.ItemIndex > - 1 then
    lbPhoneAccount.Items[lbPhoneAccount.ItemIndex] := prepareTrace();
end;

function TformTraceEmailAccount.prepareTrace: String;
var
  line, recSep: string;
  Uid: TGUID;
begin
  recSep := #30 + #30; // record separator, not printable
  if (Trim(edEmail.Text) = '')  then
    ShowMessage('Email is missing!')
  else
  begin
    CreateGUID(Uid);
    line := '{"@id":"' + GuidToString(Uid) + '", ' + recSep;
    line := line + '"@type":"Trace", ' + recSep;
    line := line + '"propertyBundle":[{' + recSep;
    line := line + '"@type":"EmailAccount", ' + recSep;
    line := line + '"emailAddress":"' + edEmail.Text + '"' + recSep;
    line := line + '}]}';
  end;
  Result := line;

end;

procedure TformTraceEmailAccount.btnCancelClick(Sender: TObject);
begin
  formTraceEmailAccount.Close;
end;

procedure TformTraceEmailAccount.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line:string;
  idx: integer;
begin
  //dir := GetCurrentDir;
  // create file JSON uuidCase-phone_account.json
  AssignFile(fileJSON, FpathCase + FuuidCase + '-traceEMAIL_ACCOUNT.json');
  if lbPhoneAccount.Items.Count > 0 then
  begin
    idx := 0;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_EMAIL_ACCOUNT":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbPhoneAccount.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbPhoneAccount.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbPhoneAccount.Items[idx]);
    line := '        ]\}';
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceEMAIL_ACCOUNT.json');

  formTraceEmailAccount.Close;
end;

procedure TformTraceEmailAccount.btnAddPhoneAccountClick(Sender: TObject);
begin
  lbPhoneAccount.Items.Add(prepareTrace());
  edEmail.Text := '';
end;

procedure TformTraceEmailAccount.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceEmailAccount.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceEmailAccount.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-tracePHONE_ACCOUNT.json
  if FileExists(FpathCase + FuuidCase + '-traceEMAIL_ACCOUNT.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceEMAIL_ACCOUNT.json');
    Reset(fileJSON);
    lbPhoneAccount.Items.Clear;
    while not Eof(fileJSON) do
    begin
      ReadLn(fileJSON, line);
      line := Trim(line);
      if (line = '{') or (line = '}') or  (line = ']') or (AnsiContainsStr(line, 'OBJECTS_'))  then  // first or last line or root element
      else
      begin
        subLine := Copy(line, Length(line), 1); // rule out of comma

        if subLine = ',' then
          line := Copy(line, 1, Length(line) - 1);

        lbPhoneAccount.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTraceEmailAccount.ShowModal;
end;

end.
