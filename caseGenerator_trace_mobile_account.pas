unit caseGenerator_trace_mobile_account;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, caseGenerator_util;

type
  TformTraceMobileAccount = class(TForm)
    Label1: TLabel;
    lbPhoneAccount: TListBox;
    edMSISDN: TEdit;
    btnClose: TButton;
    btnAddPhoneAccount: TButton;
    btnDeletePhoneAccount: TButton;
    btnCancel: TButton;
    btnModifyTrace: TButton;
    Label2: TLabel;
    procedure btnAddPhoneAccountClick(Sender: TObject);
    procedure btnDeletePhoneAccountClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyTraceClick(Sender: TObject);
    procedure lbPhoneAccountChange(Sender: TObject);
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
  formTraceMobileAccount: TformTraceMobileAccount;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceMobileAccount.btnDeletePhoneAccountClick(Sender: TObject);
begin
  lbPhoneAccount.Items.Delete(lbPhoneAccount.ItemIndex);
end;


procedure TformTraceMobileAccount.btnModifyTraceClick(Sender: TObject);
begin
  if lbPhoneAccount.ItemIndex > - 1 then
    lbPhoneAccount.Items[lbPhoneAccount.ItemIndex] := prepareTrace('modify');
end;

procedure TformTraceMobileAccount.lbPhoneAccountChange(Sender: TObject);
begin
  if lbPhoneAccount.ItemIndex > - 1 then
    edMSISDN.Text := ExtractField(lbPhoneAccount.Items[lbPhoneAccount.ItemIndex], '"MSISDN":"');
end;

function TformTraceMobileAccount.prepareTrace(operation: String): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30; // record separator, not printable
  indent := '   ';

  if (Trim(edMSISDN.Text) = '')  then
    ShowMessage('MSISDN number is missing!')
  else
  begin
    line := '{';
    if operation = 'add' then
    begin
      CreateGUID(Uid);
      guidNoBraces := 'kb:' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
    end
    else
      guidNoBraces :=  ExtractField(lbPhoneAccount.Items[lbPhoneAccount.ItemIndex], '"@id":"');

    line := line + indent + '"@id":"' + guidNoBraces + '", ' + recSep;
    line := line + indent + '"@type":"Trace", ' + recSep;
    line := line + indent + '"propertyBundle":[' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@id":"' + guidNoBraces + '-Account", ' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":"Account", ' + recSep;
    line := line + RepeatString(indent, 2) + '"accountType":"PhoneAccount", ' + recSep;
    line := line + RepeatString(indent, 2) + '"isActive":"true"' + recSep;
    line := line + indent + '},' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@id":"' + guidNoBraces + '-MobileAccount", ' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":"MobileAccount", ' + recSep;
    line := line + RepeatString(indent, 2) + '"MSISDN":"' + edMSISDN.Text + '"' + recSep;
    line := line + indent + '}' + recSep;
    line := line + ']}';
  end;
  Result := line;

end;

procedure TformTraceMobileAccount.btnCancelClick(Sender: TObject);
begin
  formTraceMobileAccount.Close;
end;

procedure TformTraceMobileAccount.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line:string;
  idx: integer;
begin
  //dir := GetCurrentDir;
  // create file JSON uuidCase-phone_account.json
  AssignFile(fileJSON, FpathCase + FuuidCase + '-traceMOBILE_ACCOUNT.json');
  if lbPhoneAccount.Items.Count > 0 then
  begin
    idx := 0;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_MOBILE_ACCOUNT":[';
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
    deleteFile(FpathCase + FuuidCase + '-traceMOBILE_ACCOUNT.json');

  formTraceMobileAccount.Close;
end;

procedure TformTraceMobileAccount.btnAddPhoneAccountClick(Sender: TObject);
begin
    lbPhoneAccount.Items.Add(prepareTrace('add'));
    edMSISDN.Text := '';
end;

procedure TformTraceMobileAccount.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceMobileAccount.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceMobileAccount.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-tracePHONE_ACCOUNT.json
  if FileExists(FpathCase + FuuidCase + '-traceMOBILE_ACCOUNT.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceMOBILE_ACCOUNT.json');
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

  formTraceMobileAccount.ShowModal;
end;

end.
