unit caseGenerator_trace_email_account;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, caseGenerator_util;

type
  TformTraceEmailAccount = class(TForm)
    Label1: TLabel;
    lbEmailAccount: TListBox;
    Label3: TLabel;
    btnClose: TButton;
    btnAddPhoneAccount: TButton;
    btnDeletePhoneAccount: TButton;
    btnCancel: TButton;
    btnModifyTrace: TButton;
    edEmail: TEdit;
    procedure btnAddPhoneAccountClick(Sender: TObject);
    procedure btnDeletePhoneAccountClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyTraceClick(Sender: TObject);
    procedure lbEmailAccountChange(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function prepareTrace(operation: String): String;
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
  lbEmailAccount.Items.Delete(lbEmailAccount.ItemIndex);
end;


procedure TformTraceEmailAccount.btnModifyTraceClick(Sender: TObject);
begin
  if lbEmailAccount.ItemIndex > - 1 then
    lbEmailAccount.Items[lbEmailAccount.ItemIndex] := prepareTrace('modify');
end;

procedure TformTraceEmailAccount.lbEmailAccountChange(Sender: TObject);
var
  line: String;
begin
  if lbEmailAccount.ItemIndex > - 1 then
  begin
    line := lbEmailAccount.Items[lbEmailAccount.ItemIndex];
    edEmail.Text := ExtractField(line, '"uco-observable:emailAddress":"');
  end;
end;

function TformTraceEmailAccount.prepareTrace(operation: String): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30; // record separator, not printable
  indent := '   ';

  if (Trim(edEmail.Text) = '')  then
    ShowMessage('Email is missing!')
  else
  begin
    line := '{';
    if operation = 'add' then
    begin
      CreateGUID(Uid);
      guidNoBraces := Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
      line := line + '"@id":"kb:' + guidNoBraces + '", ' + recSep;
    end
    else
    begin
      idx := lbEmailAccount.ItemIndex;
      line := line + indent + '"@id":"' + ExtractField(lbEmailAccount.Items[idx], '"@id":"') + '", ' + recSep;
    end;

    line := line + indent + '"@type":"uco-observable:CyberItem", ' + recSep;
    line := line + indent + '"uco-core:facets":[{' + recSep;
    line := line + indent + '"@type":"uco-observable:EmailAccount", ' + recSep;
    line := line + indent + '"uco-observable:emailAddress":"' + edEmail.Text + '"' + recSep;
    line := line + '}' + recSep + ']}';
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
  if lbEmailAccount.Items.Count > 0 then
  begin
    idx := 0;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_EMAIL_ACCOUNT":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbEmailAccount.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbEmailAccount.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbEmailAccount.Items[idx]);
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
  lbEmailAccount.Items.Add(prepareTrace('add'));
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
    lbEmailAccount.Items.Clear;
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

        lbEmailAccount.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTraceEmailAccount.ShowModal;
end;

end.
