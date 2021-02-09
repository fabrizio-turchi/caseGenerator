unit caseGenerator_trace_application_account;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.JSON, System.JSON.Types, System.JSON.Readers,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, caseGenerator_util;

type
  TformTraceApplicationAccount = class(TForm)
    Label1: TLabel;
    lbAppAccount: TListBox;
    btnClose: TButton;
    btnAddTrace: TButton;
    ù: TButton;
    btnCancel: TButton;
    btnModifyTrace: TButton;
    Label2: TLabel;
    edAppName: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edAppDisplayName: TEdit;
    Label6: TLabel;
    edAppIdentifier: TEdit;
    cbAppID: TComboBox;
    procedure btnAddTraceClick(Sender: TObject);
    procedure ùClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyTraceClick(Sender: TObject);
    procedure lbAppAccountChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function prepareTrace(operation: String): String;
    procedure readTraceApplication;
    function JsonTokenToString(const t: TJsonToken): string;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceApplicationAccount: TformTraceApplicationAccount;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceApplicationAccount.ùClick(Sender: TObject);
begin
  lbAppAccount.Items.Delete(lbAppAccount.ItemIndex);
end;


procedure TformTraceApplicationAccount.btnModifyTraceClick(Sender: TObject);
begin
  if lbAppAccount.ItemIndex > - 1 then
    lbAppAccount.Items[lbAppAccount.ItemIndex] := prepareTrace('modify');
end;

procedure TformTraceApplicationAccount.FormShow(Sender: TObject);
begin
  readTraceApplication;
end;

function TformTraceApplicationAccount.JsonTokenToString(
  const t: TJsonToken): string;
begin
 case t of
    TJsonToken.None: Result := 'None';
    TJsonToken.StartObject: Result := 'StartObject' ;
    TJsonToken.StartArray: Result := 'StartArray' ;
    TJsonToken.StartConstructor: Result := 'StartConstructor' ;
    TJsonToken.PropertyName: Result := 'PropertyName' ;
    TJsonToken.Comment: Result := 'Comment' ;
    TJsonToken.Raw: Result := 'Raw' ;
    TJsonToken.Integer: Result := 'Integer' ;
    TJsonToken.Float: Result := 'Float' ;
    TJsonToken.String: Result := 'String' ;
    TJsonToken.Boolean: Result := 'Boolean' ;
    TJsonToken.Null: Result := 'Null' ;
    TJsonToken.Undefined: Result := 'Undefined' ;
    TJsonToken.EndObject: Result := 'EndObject' ;
    TJsonToken.EndArray: Result := 'EndArray' ;
    TJsonToken.EndConstructor: Result := 'EndConstructor' ;
    TJsonToken.Date: Result := 'Date' ;
    TJsonToken.Bytes: Result := 'Bytes' ;
    TJsonToken.Oid: Result := 'Oid' ;
    TJsonToken.RegEx: Result := 'RegEx' ;
    TJsonToken.DBRef: Result := 'DBRef' ;
    TJsonToken.CodeWScope: Result := 'CodeWScope' ;
    TJsonToken.MinKey: Result := 'MinKey' ;
    TJsonToken.MaxKey: Result := 'MaxKey' ;
  end;
end;

procedure TformTraceApplicationAccount.lbAppAccountChange(Sender: TObject);
var
  line: String;
  appID: String;
  i: Integer;
begin
  if lbAppAccount.ItemIndex > - 1 then
  begin
    line := lbAppAccount.Items[lbAppAccount.ItemIndex];

    appID := ExtractField(line, '"uco-observable:accountID":"');
    for i := 0 to cbAppId.Count - 1 do
    begin
      if cbAppId.Items[i] = appID then
      begin
        cbAppId.ItemIndex := i;
        break;
      end;
    end;

    edAppName.Text := ExtractField(line, '"uco-observable:applicationIdentifier":"');
    edAppDisplayName.Text:=  ExtractField(line, '"uco-observable:displayName":"');
  end;
end;

function TformTraceApplicationAccount.prepareTrace(operation: String): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30; // record separator, not printable
  indent := '   ';

  if (cbAppID.ItemIndex = - 1)  then
    ShowMessage('Application ID is missing!')
  else
  begin
    line := '{' + recSep;
    if operation = 'add' then
    begin
      CreateGUID(Uid);
      guidNoBraces := ':' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
    end
    else
      guidNoBraces :=  ExtractField(lbAppAccount.Items[lbAppAccount.ItemIndex], '"@id":"');

    line := line + indent + '"@id":"' + guidNoBraces + '", ' + recSep;
    line := line + indent + '"@type":"uco-observable:CyberItem", ' + recSep;
    line := line + indent + '"uco-core:name":"' + edAppName.Text  + '",' + recSep;
    line := line + indent + '"uco-core:facets":[' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":""uco-observable:Account", ' + recSep;
    line := line + RepeatString(indent, 2) + '""uco-observable:accountIssuer":"' + edAppName.Text + '", ' + recSep;
    line := line + RepeatString(indent, 2) + '""uco-observable:applicationIdentifier":"' + edAppIdentifier.Text + '", ' + recSep;
    line := line + RepeatString(indent, 2) + '""uco-observable:isActive":"true"' + recSep ;
    line := line + indent + '},' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":"uco-observable:ApplicationAccount", ' + recSep;
		line := line + RepeatString(indent, 2) + '"uco-observable:application":"{' +
      cbAppId.Items[cbAppId.ItemIndex] + '"}' + recSep;
		line := line + indent + '},' + recSep;
		line := line + indent + '{' + recSep;
		line := line + RepeatString(indent, 2) + '"@type":"uco-observable:DigitalAccount", ' + recSep;
		line := line + RepeatString(indent, 2) + '"uco-observable:displayName":"' +
      edAppDisplayName.Text + '"' + recSep;
    line := line + indent + '}' + recSep;
    line := line + indent + ']' + recSep + '}';
  end;
  Result := line;

end;

procedure TformTraceApplicationAccount.readTraceApplication;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inAppID, inAppName: Boolean;
  appID, appName: string;
  listTrace: TStringList;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceAPPLICATION.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceAPPLICATION.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = '@id' then
            inAppID := True
          else
            inAppID := False;

          if jreader.Value.AsString = 'uco-core:name' then
            inAppName := True
          else
            inAppName := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inAppID then
            appID := Copy(jreader.Value.AsString, 1, 37);  // only the guuid

          if inAppName then begin
            appName := jreader.Value.AsString;
            cbAppID.Items.Add(appName + ' ' + '@' + appID);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformTraceApplicationAccount.btnCancelClick(Sender: TObject);
begin
  formTraceApplicationAccount.Close;
end;

procedure TformTraceApplicationAccount.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line:string;
  idx: integer;
begin
  //dir := GetCurrentDir;
  // create file JSON uuidCase-phone_account.json
  AssignFile(fileJSON, FpathCase + FuuidCase + '-traceAPPLICATION_ACCOUNT.json');
  if lbAppAccount.Items.Count > 0 then
  begin
    idx := 0;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_APPLICATION_ACCOUNT":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbAppAccount.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbAppAccount.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbAppAccount.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceAPPLICATION_ACCOUNT.json');

  formTraceApplicationAccount.Close;
end;

procedure TformTraceApplicationAccount.btnAddTraceClick(Sender: TObject);
begin
    lbAppAccount.Items.Add(prepareTrace('add'));
    cbAppID.ItemIndex := -1;
    edAppName.Text := '';
    edAppIdentifier.Text:= '';
end;

procedure TformTraceApplicationAccount.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceApplicationAccount.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceApplicationAccount.ShowWithParamater(pathCase: String; uuidCase: String);
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
    lbAppAccount.Items.Clear;
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

        lbAppAccount.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTraceApplicationAccount.ShowModal;
end;

end.
