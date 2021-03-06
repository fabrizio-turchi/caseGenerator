unit caseGenerator_trace_app_account;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, caseGenerator_util;

type
  TformTraceAppAccount = class(TForm)
    Label1: TLabel;
    lbFacebookAccount: TListBox;
    btnClose: TButton;
    btnAddPhoneAccount: TButton;
    btnDeletePhoneAccount: TButton;
    btnCancel: TButton;
    btnModifyTrace: TButton;
    Label2: TLabel;
    edIssuer: TEdit;
    Label3: TLabel;
    edID: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    edDisplayName: TEdit;
    procedure btnAddPhoneAccountClick(Sender: TObject);
    procedure btnDeletePhoneAccountClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyTraceClick(Sender: TObject);
    procedure lbFacebookAccountChange(Sender: TObject);
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
  formTraceAppAccount: TformTraceAppAccount;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceAppAccount.btnDeletePhoneAccountClick(Sender: TObject);
begin
  lbFacebookAccount.Items.Delete(lbFacebookAccount.ItemIndex);
end;


procedure TformTraceAppAccount.btnModifyTraceClick(Sender: TObject);
begin
  if lbFacebookAccount.ItemIndex > - 1 then
    lbFacebookAccount.Items[lbFacebookAccount.ItemIndex] := prepareTrace('modify');
end;

procedure TformTraceAppAccount.lbFacebookAccountChange(Sender: TObject);
var
  line: String;
begin
  if lbFacebookAccount.ItemIndex > - 1 then
  begin
    line := lbFacebookAccount.Items[lbFacebookAccount.ItemIndex];
    edID.Text := ExtractField(line, '"uco-observable:accountID":"');
    edIssuer.Text := ExtractField(line, '"uco-observable:accountIssuer":"');
    edDisplayName.Text:=  ExtractField(line, '"uco-observable:displayName":"');
  end;
end;

function TformTraceAppAccount.prepareTrace(operation: String): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30; // record separator, not printable
  indent := '   ';

  if (Trim(edID.Text) = '')  then
    ShowMessage('Account ID is missing!')
  else
  begin
    line := '{' + recSep;
    if operation = 'add' then
    begin
      CreateGUID(Uid);
      guidNoBraces := ':' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
    end
    else
      guidNoBraces :=  ExtractField(lbFacebookAccount.Items[lbFacebookAccount.ItemIndex], '"@id":"');

    line := line + indent + '"@id":"' + guidNoBraces + '", ' + recSep;
    line := line + indent + '"@type":"uco-observable:CyberItem", ' + recSep;
    line := line + indent + '"uco-core:facets":[' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":""uco-observable:Account", ' + recSep;
    line := line + RepeatString(indent, 2) + '""uco-observable:accountIssuer":"' + edIssuer.Text + '", ' + recSep;
    line := line + RepeatString(indent, 2) + '""uco-observable:isActive":"true"' + recSep ;
    line := line + indent + '},' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":"uco-observable:ApplicationAccount", ' + recSep;
		line := line + RepeatString(indent, 2) + '"uco-observable:application":"NOT_SET"' + recSep;
		line := line + indent + '},' + recSep;
		line := line + indent + '{' + recSep;
		line := line + RepeatString(indent, 2) + '"@type":"uco-observable:DigitalAccount", ' + recSep;
		line := line + RepeatString(indent, 2) + '"uco-observable:displayName":"' + edDisplayName.Text + '"' + recSep;
    line := line + indent + '}' + recSep;
    line := line + indent + ']' + recSep + '}';
  end;
  Result := line;

end;

procedure TformTraceAppAccount.btnCancelClick(Sender: TObject);
begin
  formTraceAppAccount.Close;
end;

procedure TformTraceAppAccount.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line:string;
  idx: integer;
begin
  AssignFile(fileJSON, FpathCase + FuuidCase + '-traceAPP_ACCOUNT.json');
  if lbFacebookAccount.Items.Count > 0 then
  begin
    idx := 0;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_APP_ACCOUNT":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbFacebookAccount.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbFacebookAccount.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbFacebookAccount.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceAPP_ACCOUNT.json');

  formTraceAppAccount.Close;
end;

procedure TformTraceAppAccount.btnAddPhoneAccountClick(Sender: TObject);
begin
    lbFacebookAccount.Items.Add(prepareTrace('add'));
    edID.Text := '';
    edIssuer.Text := '';
end;

procedure TformTraceAppAccount.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceAppAccount.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceAppAccount.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-tracePHONE_ACCOUNT.json
  if FileExists(FpathCase + FuuidCase + '-traceFACEBOOK_ACCOUNT.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceFACEBOOK_ACCOUNT.json');
    Reset(fileJSON);
    lbFacebookAccount.Items.Clear;
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

        lbFacebookAccount.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTraceAppAccount.ShowModal;
end;

end.
