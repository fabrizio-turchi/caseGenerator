unit caseGenerator_trace_phone_call;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers, FMX.ScrollBox, FMX.Memo, caseGenerator_util,
  FMX.Memo.Types;

type
  TformTracePhoneCall = class(TForm)
    Label1: TLabel;
    lbCalls: TListBox;
    btnClose: TButton;
    btnAddCall: TButton;
    btnRemoveCall: TButton;
    panelFrom: TPanel;
    Label5: TLabel;
    Label8: TLabel;
    cbMobileFrom: TComboBox;
    panelTo: TPanel;
    cbMobileTo: TComboBox;
    Label11: TLabel;
    Label13: TLabel;
    cbSentMonth: TComboBox;
    timeSent: TTimeEdit;
    cbSentYear: TComboBox;
    Label3: TLabel;
    btnCancel: TButton;
    btnModifyCall: TButton;
    cbSentDay: TComboBox;
    Label2: TLabel;
    edDuration: TEdit;
    procedure btnAddCallClick(Sender: TObject);
    procedure btnRemoveCallClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCallsChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyCallClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    procedure readTraceFromFile;
    procedure readTraceMobileFromFile;
    procedure readTraceFacebookAccountFromFile;
    procedure readTracePhoneAccountFromFile;
    function  extractID(line: String): String;
    function extractLastID(line: String): String;
    function prepareItemCall(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTracePhoneCall: TformTracePhoneCall;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTracePhoneCall.btnRemoveCallClick(Sender: TObject);
begin
  if lbCalls.ItemIndex > -1 then
    lbCalls.Items.Delete(lbCalls.ItemIndex);
end;

function TformTracePhoneCall.extractID(line: String): String;
begin
  Result := Copy(line, Pos('@', line) + 1, Length(line));
end;



function TformTracePhoneCall.extractLastID(line: String): String;
begin
   Result := Copy(line, LastDelimiter('@', line) + 1, Length(line));
end;

procedure TformTracePhoneCall.FormShow(Sender: TObject);
var
  idx: Integer;
begin
  for idx:=2000 to 2020 do
    cbSentYear.Items.Add(IntToStr(idx));

  cbSentDay.ItemIndex := -1;
  cbSentMonth.ItemIndex := -1;
  cbSentYear.ItemIndex := -1;
  readTraceFromFile;
  cbMobileFrom.ItemIndex := -1;
  cbMobileTo.ItemIndex := -1;

end;

function TformTracePhoneCall.JsonTokenToString(const t: TJsonToken): string;
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


procedure TformTracePhoneCall.lbCallsChange(Sender: TObject);
var
  line, field, sentDate, sDay, sMonth, sYear, sDate, messageType: String;
  idx: Integer;
begin
  if lbCalls.ItemIndex > - 1 then
  begin
    line := lbCalls.Items[lbCalls.ItemIndex];
    field := ExtractRefId(line, '"uco-observable:from":{');
    for idx:=0 to cbMobileFrom.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbMobileFrom.Items[idx], field) then
      begin
        cbMobileFrom.ItemIndex := idx;
        Break;
      end;
    end;

    field := ExtractRefId(line, '"uco-observable:to":{');
    for idx:=0 to cbMobileTo.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbMobileTo.Items[idx], field) then
      begin
        cbMobileTo.ItemIndex := idx;
        Break;
      end;
    end;

    sentDate := ExtractDateValue(line, '"uco-observable:sentTime":"');
    sDate := Copy(sentDate, 1, 10);
    sDay := Copy(sDate, 7, 2);
    for idx:=0 to cbSentDay.Items.Count - 1 do
    begin
      if cbSentDay.Items[idx] = sDay then
      begin
        cbSentDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 5, 2);
    for idx:=0 to cbSentMonth.Items.Count - 1 do
    begin
      if cbSentMonth.Items[idx] = sMonth then
      begin
        cbSentMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbSentYear.Items.Count - 1 do
    begin
      if cbSentYear.Items[idx] = sYear then
      begin
        cbSentYear.ItemIndex := idx;
        break;
      end;
    end;

    timeSent.Text := Copy(sentDate, 10, 8);
    edDuration.Text := ExtractNumericValue(line, '"uco-observable:duration":{');

  end;

end;

function TformTracePhoneCall.prepareItemCall(operation: String): String;
var
  line, recSep, idLine, indent, guidNoBraces: string;
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
    guidNoBraces :=  ExtractField(lbCalls.Items[lbCalls.ItemIndex], '"@id":"');

  line := line + indent + '"@id":"' + guidNoBraces + '",' + recSep;
  line := line + indent + '"@type":"uco-observable:CyberItem",' + recSep;
  line := line + indent + '"uco-core:facets":[' + recSep;
  line := line + indent + '{' + recSep;
  line := line + RepeatString(indent, 2) + '"@type":"uco-observable:PhoneCall",' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:sentTime":' + recSep;
  line := line +  RepeatString(indent, 3)  + '{' + recSep;
  line := line +  RepeatString(indent, 3)  + '"@type":"xsd:dateTime",' + recSep;
  line := line +  RepeatString(indent, 3)  + '"@value":"' + cbSentYear.Items[cbSentYear.ItemIndex];
  line := line + cbSentMonth.Items[cbSentMonth.ItemIndex];
  line := line + cbSentDay.Items[cbSentDay.ItemIndex] + 'T' + timeSent.Text + '"' + recSep;
  line := line +  RepeatString(indent, 3)  + '},' + recSep;
  idLine := cbMobileFrom.Items[cbMobileFrom.ItemIndex];
  line := line + RepeatString(indent, 2) + '"uco-observable:from":{' + recSep;
  line := line + RepeatString(indent, 3) + '"@id":"' + extractID(idLine) + '" ' + recSep;
  line := line + RepeatString(indent, 2) + '},' + recSep;
  idLine := cbMobileTo.Items[cbMobileTo.ItemIndex];
  line := line + RepeatString(indent, 2) + '"uco-observable:to":{' + recSep;
  line := line + RepeatString(indent, 3) + '"@id":"' + extractID(idLine) + '" ' + recSep;
  line := line + RepeatString(indent, 2) + '},' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:duration":{' + recSep;
  line := line +  RepeatString(indent, 3)  + '"@type":"xsd:long",' + recSep;
  line := line +  RepeatString(indent, 3)  + '"@value":' + edDuration.Text + recSep;
  line := line + RepeatString(indent, 2) + '},' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:drafting:outcome":"Established",' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:allocationStatus":"Intact"' + recSep;
  line := line  + indent + '}]' + recSep + '}' + recSep;
  Result := line;
end;

procedure TformTracePhoneCall.readTraceFromFile;

begin
  //readTraceMobileFromFile;
  readTracePhoneAccountFromFile;
  //readTraceFacebookAccountFromFile;
end;

procedure TformTracePhoneCall.readTraceMobileFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inModel, inMSISDN: Boolean;
  id, model, msisdn: string;
  listTrace: TStringList;
  idx, nHypens: integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceMOBILE.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceMOBILE.json');
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

          if jreader.Value.AsString = 'model' then
            inModel := True
          else
            inModel := False;

          if jreader.Value.AsString = 'MSISDN' then
            inMSISDN := True
          else
            inMSISDN := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inModel then
            model := jreader.Value.AsString;

          if inMSISDN then
          begin
            msisdn := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);
            cbMobileFrom.Items.Add(model + ' ' + msisdn +
                StringOfChar(' ', 100) + '@' + id);
            cbMobileTo.Items.Add(model + ' ' + msisdn +
                StringOfChar(' ', 100) +'@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;
end;


procedure TformTracePhoneCall.readTracePhoneAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inPhoneNumber, inPhoneName: Boolean;
  id, phoneNumber, phoneName: string;
  listTrace: TStringList;
  idx, nHypens: integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-tracePHONE_ACCOUNT.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-tracePHONE_ACCOUNT.json');
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

          if jreader.Value.AsString = 'uco-observable:phoneNumber' then
            inPhoneNumber := True
          else
            inPhoneNumber := False;

          if jreader.Value.AsString = 'uco-observable:name' then
            inPhoneName := True
          else
            inPhoneName := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inPhoneNumber then
            phoneNumber := jreader.Value.AsString;

          if inPhoneName then
          begin
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            phoneName := jreader.Value.AsString;
            cbMobileFrom.Items.Add(phoneName + ' ' + phoneNumber +
              StringOfChar(' ', 100) + '@' + id);
            cbMobileTo.Items.Add(phoneName + ' ' + phoneNumber +
              StringOfChar(' ', 100) + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformTracePhoneCall.readTraceFacebookAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inAccountID, inID: Boolean;
  accountID, id: string;
  listTrace: TStringList;
  idx, nHypens: integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceFACEBOOK_ACCOUNT.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceFACEBOOK_ACCOUNT.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'accountID' then
            inAccountID := True
          else
            inAccountID := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;
        end;

        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inAccountID then
          begin
            accountID := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);
            accountID := stringreplace(accountID, '@', '#',[rfReplaceAll]);
            cbMobileFrom.Items.Add('Facebook account ' + accountID + '@' + id);
            cbMobileTo.Items.Add('Facebook account ' + accountID + '@' + id);
          end;
        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformTracePhoneCall.btnCancelClick(Sender: TObject);
begin
  formTracePhoneCall.Close;
end;

procedure TformTracePhoneCall.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line: String;
  idx: integer;
begin
  if lbCalls.Items.Count > 0 then
  begin
    //dir := GetCurrentDir;
    idx := 0;
  // create file JSON uuidCase-tracePHONE_CALL.json
    AssignFile(fileJSON, FpathCase + FuuidCase + '-tracePHONE_CALL.json', CP_UTF8);
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_SMS":[';
    WriteLn(fileJSON, UTF8Encode(line));


    for idx:= 0 to lbCalls.Items.Count - 2 do
      WriteLn(fileJSON, UTF8Encode(#9#9 + lbCalls.Items[idx] + ','));

    WriteLn(fileJSON, UTF8Encode(#9#9 + lbCalls.Items[idx]));
    WriteLn(fileJSON, UTF8Encode(#9#9 + ']'));
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-tracePHONE_CALL.json');

  formTracePhoneCall.Close;
end;

procedure TformTracePhoneCall.btnModifyCallClick(Sender: TObject);
begin
  if lbCalls.ItemIndex > - 1 then
    lbCalls.Items[lbCalls.ItemIndex] := prepareItemCall('modify');
end;

procedure TformTracePhoneCall.btnAddCallClick(Sender: TObject);
var
  line, recSep, idLine: string;
  Uid: TGUID;
  idx: Integer;
begin
  if (cbMobileTo.ItemIndex = -1) or (cbMobileFrom.ItemIndex = -1)  then
    ShowMessage('Mobile FROM or/and Mobile Source empty!')
  else
  begin
    lbCalls.Items.Add(prepareItemCall('add'));
    cbMobileFrom.ItemIndex := -1;
    cbMobileTo.ItemIndex := -1;
    cbSentDay.ItemIndex := -1;
    cbSentMonth.ItemIndex := -1;
    cbSentYear.ItemIndex := -1;
  end;
end;

procedure TformTracePhoneCall.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTracePhoneCall.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTracePhoneCall.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine: String;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-tracePHONE_CALL.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-tracePHONE_CALL.json', CP_UTF8);
    Reset(fileJSON);
    lbCalls.Items.Clear;
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

        lbCalls.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTracePhoneCall.ShowModal;
end;

end.
