unit caseGenerator_trace_email;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers, FMX.ScrollBox, FMX.Memo, caseGenerator_util,
  FMX.Memo.Types;

type
  TformTraceEmail = class(TForm)
    Label1: TLabel;
    lbMessage: TListBox;
    btnClose: TButton;
    btnAddMessage: TButton;
    btnRemoveMessage: TButton;
    Label6: TLabel;
    panelFrom: TPanel;
    Label5: TLabel;
    Label8: TLabel;
    panelTo: TPanel;
    cbEmailTo: TComboBox;
    Label11: TLabel;
    Label13: TLabel;
    edApplication: TEdit;
    memoMessageText: TMemo;
    cbMonth: TComboBox;
    emailTime: TTimeEdit;
    cbYear: TComboBox;
    Label3: TLabel;
    btnCancel: TButton;
    btnModifyMessage: TButton;
    cbDay: TComboBox;
    Label2: TLabel;
    cbEmailFrom: TComboBox;
    procedure btnAddMessageClick(Sender: TObject);
    procedure btnRemoveMessageClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbMessageChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyMessageClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    procedure readTraceEmailAccountFromFile;
    function  extractID(line: String): String;
    function extractLastID(line: String): String;
    function prepareItemMessage(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceEmail: TformTraceEmail;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceEmail.btnRemoveMessageClick(Sender: TObject);
begin
  if lbMessage.ItemIndex > -1 then
    lbMessage.Items.Delete(lbMessage.ItemIndex);
end;

function TformTraceEmail.extractID(line: String): String;
begin
  Result := Copy(line, Pos('@', line) + 1, Length(line));
end;



function TformTraceEmail.extractLastID(line: String): String;
begin
   Result := Copy(line, LastDelimiter('@', line) + 1, Length(line));
end;

procedure TformTraceEmail.FormShow(Sender: TObject);
var
  idx: Integer;
begin
  for idx:=2000 to 2020 do
    cbYear.Items.Add(IntToStr(idx));

  edApplication.Text := '';
  memoMessageText.Text := '';
  cbDay.ItemIndex := -1;
  cbMonth.ItemIndex := -1;
  cbYear.ItemIndex := -1;
  cbEmailFrom.ItemIndex := -1;
  cbEmailTo.ItemIndex := -1;
  readTraceEmailAccountFromFile;

end;

function TformTraceEmail.JsonTokenToString(const t: TJsonToken): string;
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


procedure TformTraceEmail.lbMessageChange(Sender: TObject);
var
  line, uuidTo, sentDate, sDay, sMonth, sYear, sDate, messageType: String;
  idx, posTo: Integer;
  mobileTo: TStringList;
begin
  if lbMessage.ItemIndex > - 1 then
  begin
    line := lbMessage.Items[lbMessage.ItemIndex];
    edApplication.Text := ExtractField(line, '"uco-observable:application":"');
    memoMessageText.Lines.Text := ExtractField(line, '"uco-observable:messageText":"');
    line := ExtractField(line, '"uco-observable:from":"');
    for idx:=0 to cbEmailFrom.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbEmailFrom.Items[idx], line) then
      begin
        cbEmailFrom.ItemIndex := idx;
        Break;
      end;
    end;

    posTo := Pos('"uco-observable:to":[', line);
    uuidTo := Copy(line, posTo + 22, Length(line));
    uuidTo := ExtractField(uuidTo, '@id":"');
    for idx:=0 to cbEmailTo.Count - 1 do
    begin
      if Pos(uuidTo, cbEmailTo.Items[idx]) > 0  then
      begin
        cbEmailTo.ItemIndex := idx;
        break;
      end;
    end;

    sentDate := ExtractField(lbMessage.Items[lbMessage.ItemIndex], '"@value":"');
    sDate := Copy(sentDate, 1, 10);
    sDay := Copy(sDate, 7, 2);
    for idx:=0 to cbDay.Items.Count - 1 do
    begin
      if cbDay.Items[idx] = sDay then
      begin
        cbDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 5, 2);
    for idx:=0 to cbMonth.Items.Count - 1 do
    begin
      if cbMonth.Items[idx] = sMonth then
      begin
        cbMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbYear.Items.Count - 1 do
    begin
      if cbYear.Items[idx] = sYear then
      begin
        cbYear.ItemIndex := idx;
        break;
      end;
    end;

    emailTime.Text := Copy(sentDate, 10, 8);

  end;

    // read trace-MOBILE fro extracting all ID with model and MSISDN


end;

function TformTraceEmail.prepareItemMessage(operation: String): String;
var
  line, recSep, idLine, indent, guidNoBraces, msgTime: string;
  Uid: TGUID;
  idx: Integer;
begin
  idx:= 0;
  recSep := #30 + #30;
  indent := '   ';

  line := '{' + recSep;

  if operation = 'add' then
  begin
    CreateGUID(Uid);
    guidNoBraces := 'kb:' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
  end
  else
    guidNoBraces :=  ExtractField(lbMessage.Items[lbMessage.ItemIndex], '"@id":"');

  line := line + indent + '"@id":"' + guidNoBraces + '",' + recSep;
  line := line + indent + '"@type":"uco-observable:CyberItem",' + recSep;
  line := line + indent + '"uco-core:facets":[' + recSep;
  line := line + indent + '{' + recSep;
  line := line + RepeatString(indent, 2) + '"@type":"uco-observable:EmailMessage",' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:application":"' + edApplication.Text + '",' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:sentTime":{' + recSep;
  line := line + '"@type":"xsd:dateTime",' + recSep;
  msgTime := cbYear.Items[cbYear.ItemIndex]  + '-';
  msgTime := msgTime +  cbMonth.Items[cbMonth.ItemIndex]  + '-';
  msgTime := msgTime +  cbDay.Items[cbDay.ItemIndex]  + 'T';
  msgTime := msgTime +  TimeToStr(emailTime.Time);
  line := line + '"@value":"' + msgTime + '"},' + recSep;
  idLine := ExtractID(cbEmailFrom.Items[cbEmailFrom.ItemIndex]);
  line := line + '"uco-observable:fromRef":"' + idLine + '",' + recSep;
  idLine := ExtractID(cbEmailTo.Items[cbEmailFrom.ItemIndex]);
  line := line + '"uco-observable:toRef":["' + idLine + '"],' + recSep;
  line := line + '"uco-observable:ccRefs":[],' + recSep;
  line := line + '"uco-observable:bccRefs":[],' + recSep;
  line := line + '"uco-observable:body":"' + memoMessageText.Text + '",' + recSep;

  line := line + RepeatString(indent, 2) + '"uco-observable:sentTime":"' + cbYear.Items[cbYear.ItemIndex];
  line := line + cbMonth.Items[cbMonth.ItemIndex];
  line := line + cbDay.Items[cbDay.ItemIndex] + 'T' + emailTime.Text + 'Z",' + recSep;
  line := line  + indent + '}]' + recSep + '}' + recSep;
  Result := line;
end;

procedure TformTraceEmail.readTraceEmailAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inEmail: Boolean;
  id, email: String;
  listTrace: TStringList;
  idx, nHypens, posField: integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceEMAIL_ACCOUNT.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceEMAIL_ACCOUNT.json');
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
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-observable:emailAddress' then
            inEmail := True
          else
            inEmail := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := Copy(jreader.Value.AsString, 1, 37);

          if inEmail then
          begin
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            email := jreader.Value.AsString;
            cbEmailFrom.Items.Add(email + StringOfChar(' ', 100) + '@' + id);
            cbEmailTo.Items.Add(email +  StringOfChar(' ', 100) + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;



procedure TformTraceEmail.btnCancelClick(Sender: TObject);
begin
  formTraceEmail.Close;
end;

procedure TformTraceEmail.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line: String;
  idx: integer;
begin
  if lbMessage.Items.Count > 0 then
  begin
    //dir := GetCurrentDir;
    idx := 0;
  // create file JSON uuidCase-traceMESSAGE.json
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceEMAIL.json', CP_UTF8);
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_EMAIL":[';
    WriteLn(fileJSON, UTF8Encode(line));


    for idx:= 0 to lbMessage.Items.Count - 2 do
      WriteLn(fileJSON, UTF8Encode(#9#9 + lbMessage.Items[idx] + ','));

    WriteLn(fileJSON, UTF8Encode(#9#9 + lbMessage.Items[idx]));
    WriteLn(fileJSON, UTF8Encode(#9#9 + ']'));
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceEMAIL.json');

  formTraceEmail.Close;
end;

procedure TformTraceEmail.btnModifyMessageClick(Sender: TObject);
begin
  if lbMessage.ItemIndex > - 1 then
    lbMessage.Items[lbMessage.ItemIndex] := prepareItemMessage('modify');
end;

procedure TformTraceEmail.btnAddMessageClick(Sender: TObject);
var
  line, recSep, idLine: string;
  Uid: TGUID;
  idx: Integer;
begin
  if (cbEmailFrom.ItemIndex = -1) or (cbEmailTo.ItemIndex = -1)  then
    ShowMessage('Email address FROM or TO is empty!')
  else
  begin
    lbMessage.Items.Add(prepareItemMessage('add'));
    edApplication.Text := '';
    memoMessageText.Lines.Clear;
    cbEmailFrom.ItemIndex := -1;
    cbEmailTo.ItemIndex := -1;
    cbDay.ItemIndex := -1;
    cbMonth.ItemIndex := -1;
    cbYear.ItemIndex := -1;
  end;
end;

procedure TformTraceEmail.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceEmail.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceEmail.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine: String;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-traceEMAIL.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceEMAIL.json', CP_UTF8);
    Reset(fileJSON);
    lbMessage.Items.Clear;
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

        lbMessage.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTraceEmail.ShowModal;
end;

end.
