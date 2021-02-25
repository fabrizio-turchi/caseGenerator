
unit caseGenerator_trace_sms;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers, FMX.ScrollBox, FMX.Memo, caseGenerator_util,
  FMX.Memo.Types;

type
  TformTraceSMS = class(TForm)
    Label1: TLabel;
    lbMessage: TListBox;
    btnClose: TButton;
    btnAddMessage: TButton;
    btnRemoveMessage: TButton;
    Label6: TLabel;
    panelFrom: TPanel;
    Label5: TLabel;
    Label8: TLabel;
    cbMobileFrom: TComboBox;
    panelTo: TPanel;
    cbMobileTo: TComboBox;
    Label11: TLabel;
    Label13: TLabel;
    edApplication: TEdit;
    memoMessageText: TMemo;
    btnAddMobile: TButton;
    btnRemoveMobile: TButton;
    lbMobile: TListBox;
    cbSentMonth: TComboBox;
    timeSent: TTimeEdit;
    cbSentYear: TComboBox;
    Label3: TLabel;
    btnCancel: TButton;
    btnModifyMessage: TButton;
    cbSentDay: TComboBox;
    Label2: TLabel;
    procedure btnAddMessageClick(Sender: TObject);
    procedure btnRemoveMessageClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddMobileClick(Sender: TObject);
    procedure btnRemoveMobileClick(Sender: TObject);
    procedure lbMessageChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyMessageClick(Sender: TObject);
    procedure lbMobileChange(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    procedure readTracePhoneAccountFromFile;
    function  extractID(line: String): String;
    function extractLastID(line: String): String;
    function prepareItemMessage(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceSMS: TformTraceSMS;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceSMS.btnRemoveMessageClick(Sender: TObject);
begin
  if lbMessage.ItemIndex > -1 then
    lbMessage.Items.Delete(lbMessage.ItemIndex);
end;

procedure TformTraceSMS.btnRemoveMobileClick(Sender: TObject);
begin
  lbMobile.Items.Delete(lbMobile.ItemIndex);
end;


function TformTraceSMS.extractID(line: String): String;
begin
  Result := Copy(line, Pos('@', line) + 1, Length(line));
end;



function TformTraceSMS.extractLastID(line: String): String;
begin
   Result := Copy(line, LastDelimiter('@', line) + 1, Length(line));
end;

procedure TformTraceSMS.FormShow(Sender: TObject);
var
  idx: Integer;
begin
  for idx:=2019 to 2030 do
    cbSentYear.Items.Add(IntToStr(idx));

  edApplication.Text := '';
  memoMessageText.Text := '';
  cbSentDay.ItemIndex := -1;
  cbSentMonth.ItemIndex := -1;
  cbSentYear.ItemIndex := -1;
  readTracePhoneAccountFromFile;
  cbMobileFrom.ItemIndex := -1;
  cbMobileTo.ItemIndex := -1;
  lbMobile.Items.Clear;

end;

function TformTraceSMS.JsonTokenToString(const t: TJsonToken): string;
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


procedure TformTraceSMS.lbMessageChange(Sender: TObject);
var
  line, sentDate, sDay, sMonth, sYear, sDate, messageType, field: String;
  idx: Integer;
begin
  if lbMessage.ItemIndex > - 1 then
  begin
    lbMobile.Items.Clear;
    line := lbMessage.Items[lbMessage.ItemIndex];
    edApplication.Text := ExtractField(line, '"uco-observable:application":"');
    memoMessageText.Lines.Text := ExtractField(line, '"uco-observable:messageText":"');
    line := ExtractRefId(line, '"uco-observable:from":{');

    for idx:=0 to cbMobileFrom.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbMobileFrom.Items[idx], line) then
      begin
        cbMobileFrom.ItemIndex := idx;
        Break;
      end;
    end;

    field := ExtractRefId(lbMessage.Items[lbMessage.ItemIndex], '"uco-observable:to":[');
    {
    if mobileTo.Count > 0 then
    begin
      for idx:=0 to mobileTo.Count - 1 do
        lbMobile.Items.Add(mobileTo[idx]);
      lbMobile.ItemIndex := 0;
    end;
     }
    // only one single To phone number

    for idx:=0 to cbMobileTo.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbMobileTo.Items[idx], field) then
      begin
        cbMobileTo.ItemIndex := idx;
        Break;
      end;
    end;

    sentDate := ExtractDateValue(lbMessage.Items[lbMessage.ItemIndex], '"uco-observable:sentTime":"');
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
  end;

end;

procedure TformTraceSMS.lbMobileChange(Sender: TObject);
var
  line: String;
  idx: Integer;
begin
  if lbMobile.ItemIndex > - 1 then
  begin
    line := lbMobile.Items[lbMobile.ItemIndex];
    line := stringreplace(line, '"', '',[rfReplaceAll]);
    for idx:=0 to cbMobileTo.Count - 1 do
    begin
      if AnsiContainsStr(cbMobileTo.Items[idx], line) then
      begin
        cbMobileTo.ItemIndex := idx;
        break
      end;
    end;

  end;

end;

function TformTraceSMS.prepareItemMessage(operation: String): String;
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
    guidNoBraces :=  ExtractField(lbMessage.Items[lbMessage.ItemIndex], '"@id":"');

  line := line + indent + '"@id":"' + guidNoBraces + '",' + recSep;
  line := line + indent + '"@type":"uco-observable:CyberItem",' + recSep;
  line := line + indent + '"uco-core:facets":[' + recSep;
  line := line + indent + '{' + recSep;
  line := line + RepeatString(indent, 2) + '"@type":"uco-observable:Message",' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:application":"' + edApplication.Text + '",' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:messageText":"' + memoMessageText.Text + '", ' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:SMSmessage":"True",' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:allocationStatus":"Intact",' + recSep;
  idLine := cbMobileFrom.Items[cbMobileFrom.ItemIndex];
  line := line + RepeatString(indent, 2) + '"uco-observable:from":{' + recSep;
  line := line + RepeatString(indent, 3) + '"@id":"' + extractID(idLine) + '"' + recSep;
  line := line + RepeatString(indent, 2) + '},' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:to":[' + recSep;
  idx := 0;
  for idx:=0 to lbMobile.Items.Count - 2 do
    line := line  + RepeatString(indent, 2) + '{"@id":"' + lbMobile.Items[idx] + '"},';

  line := line  + RepeatString(indent, 2) + '{"@id":"' +lbMobile.Items[idx] + '"}],' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:sentTime":{' + recSep;
  line := line + RepeatString(indent, 3) +  '"@type": "xsd:dateTime", ' + recSep;
  line := line + RepeatString(indent, 3) +  '"@value": "' + cbSentYear.Items[cbSentYear.ItemIndex];
  line := line + cbSentMonth.Items[cbSentMonth.ItemIndex];
  line := line + cbSentDay.Items[cbSentDay.ItemIndex] + 'T' + timeSent.Text + '"' + recSep;
  line := line + RepeatString(indent, 2) +  '}' + recSep;
  line := line  + indent + '}]' + recSep + '}' + recSep;
  Result := line;
end;



procedure TformTraceSMS.readTracePhoneAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inPhoneNumber, inPhoneIssuer, inPhoneName: Boolean;
  id, phoneNumber, phoneIssuer, phoneName: string;
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

         if jreader.Value.AsString = 'uco-observable:accountIssuer' then
            inPhoneIssuer := True
          else
            inPhoneIssuer := False;

         if jreader.Value.AsString = 'uco-observable:name' then
            inPhoneName := True
          else
            inPhoneName := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inPhoneIssuer then
            phoneIssuer := jreader.Value.AsString;

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
            cbMobileFrom.Items.Add(phoneName + ' ' + phoneNumber + ' ' +
                phoneIssuer + StringOfChar(' ', 100) + '@' + id);
             cbMobileTo.Items.Add(phoneName + ' ' + phoneNumber + ' ' +
                phoneIssuer + StringOfChar(' ', 100) + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;



procedure TformTraceSMS.btnAddMobileClick(Sender: TObject);
var
  line: String;
begin
  if cbMobileFrom.ItemIndex = cbMobileTo.ItemIndex then
    ShowMessage('Mobile numbers FROM and TO are equal')
  else
  begin
    line := cbMobileTo.Items[cbMobileTo.ItemIndex];
    line := extractID(line);
    lbMobile.Items.Add(line);
  end;

end;

procedure TformTraceSMS.btnCancelClick(Sender: TObject);
begin
  formTraceSMS.Close;
end;

procedure TformTraceSMS.btnCloseClick(Sender: TObject);
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
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceSMS.json', CP_UTF8);
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_SMS":[';
    WriteLn(fileJSON, UTF8Encode(line));


    for idx:= 0 to lbMessage.Items.Count - 2 do
      WriteLn(fileJSON, UTF8Encode(#9#9 + lbMessage.Items[idx] + ','));

    WriteLn(fileJSON, UTF8Encode(#9#9 + lbMessage.Items[idx]));
    WriteLn(fileJSON, UTF8Encode(#9#9 + ']'));
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceSMS.json');

  formTraceSMS.Close;
end;

procedure TformTraceSMS.btnModifyMessageClick(Sender: TObject);
begin
  if lbMessage.ItemIndex > - 1 then
    lbMessage.Items[lbMessage.ItemIndex] := prepareItemMessage('modify');
end;

procedure TformTraceSMS.btnAddMessageClick(Sender: TObject);
var
  line, recSep, idLine: string;
  Uid: TGUID;
  idx: Integer;
begin
  if (lbMobile.Items.Count = 0) or (cbMobileFrom.ItemIndex = -1)  then
    ShowMessage('Mobile FROM or/and Mobile Source empty!')
  else
  begin
    lbMessage.Items.Add(prepareItemMessage('add'));
    edApplication.Text := '';
    memoMessageText.Lines.Clear;
    cbMobileFrom.ItemIndex := -1;
    cbMobileTo.ItemIndex := -1;
    cbSentDay.ItemIndex := -1;
    cbSentMonth.ItemIndex := -1;
    cbSentYear.ItemIndex := -1;
    lbMobile.Items.Clear;
  end;
end;

procedure TformTraceSMS.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceSMS.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceSMS.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine: String;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-traceSMS.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceSMS.json', CP_UTF8);
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

  formTraceSMS.ShowModal;
end;

end.
