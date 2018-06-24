unit caseGenerator_trace_message;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers, FMX.ScrollBox, FMX.Memo, caseGenerator_util;

type
  TformTraceMessage = class(TForm)
    Label1: TLabel;
    lbMessage: TListBox;
    Label2: TLabel;
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
    cbSentDay: TComboBox;
    cbSentMonth: TComboBox;
    timeSent: TTimeEdit;
    cbSentYear: TComboBox;
    Label3: TLabel;
    btnCancel: TButton;
    btnModifyMessage: TButton;
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
    procedure readTraceFromFile;
    procedure readTraceMobileFromFile;
    procedure readTracePhoneAccountFromFile;
    function  extractID(line: String): String;
    function prepareItemMessage(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceMessage: TformTraceMessage;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceMessage.btnRemoveMessageClick(Sender: TObject);
begin
  lbMessage.Items.Delete(lbMessage.ItemIndex);
end;

procedure TformTraceMessage.btnRemoveMobileClick(Sender: TObject);
begin
  lbMobile.Items.Delete(lbMobile.ItemIndex);
end;


function TformTraceMessage.extractID(line: String): String;
begin
  Result := Copy(line, Pos('@', line) + 1, Length(line));
end;



procedure TformTraceMessage.FormShow(Sender: TObject);
var
  idx: Integer;
begin
  for idx:=2000 to 2020 do
    cbSentYear.Items.Add(IntToStr(idx));

  readTraceFromFile;
end;

function TformTraceMessage.JsonTokenToString(const t: TJsonToken): string;
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


procedure TformTraceMessage.lbMessageChange(Sender: TObject);
var
  line, sentDate, sDay, sMonth, sYear, sDate: String;
  idx: Integer;
  mobileTo: TStringList;
begin
  if lbMessage.ItemIndex > - 1 then
  begin
    lbMobile.Items.Clear;
    line := lbMessage.Items[lbMessage.ItemIndex];
    edApplication.Text := ExtractField(line, '"application":"');
    memoMessageText.Lines.Text := ExtractField(line, '"messageText":"');
    line := ExtractField(line, '"from":"');
    for idx:=0 to cbMobileFrom.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbMobileFrom.Items[idx], line) then
      begin
        cbMobileFrom.ItemIndex := idx;
        Break;
      end;
    end;
    mobileTo := ExtractArray(lbMessage.Items[lbMessage.ItemIndex], '"to":[');
    if mobileTo.Count > 0 then
    begin
      for idx:=0 to mobileTo.Count - 1 do
        lbMobile.Items.Add('"' + mobileTo[idx] + '"');
      lbMobile.ItemIndex := 0;
    end;

    sentDate := ExtractField(lbMessage.Items[lbMessage.ItemIndex], '"sentTime":"');
    sDate := Copy(sentDate, 1, 10);
    sDay := Copy(sDate, 9, 2);
    for idx:=0 to cbSentDay.Items.Count - 1 do
    begin
      if cbSentDay.Items[idx] = sDay then
      begin
        cbSentDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 6, 2);
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

    timeSent.Text := Copy(sentDate, 12, 8);

  end;

    // read trace-MOBILE fro extracting all ID with model and MSISDN


end;

procedure TformTraceMessage.lbMobileChange(Sender: TObject);
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

function TformTraceMessage.prepareItemMessage(operation: String): String;
var
  line, recSep, idLine: string;
  Uid: TGUID;
  idx: Integer;
begin
  idx:= 0;
  recSep := #30 + #30;

  if operation = 'add' then
  begin
    CreateGUID(Uid);
    line := '{"@id":"' + GuidToString(Uid) + '",' + recSep;
  end
  else
  begin
    idx := lbMessage.ItemIndex;
    line := '{"@id":"' + ExtractField(lbMessage.Items[idx], '"@id":"') + '",' + recSep;
  end;

  line := line + '"@type":"Trace",' + recSep;
  line := line + '"propertyBundle":[' + recSep;
  line := line + '{"@type":"Message",' + recSep;
  line := line + '"application":"' + edApplication.Text + '",' + recSep;
  line := line + '"messageText":"' + memoMessageText.Text + '", ' + recSep;
  idLine := cbMobileFrom.Items[cbMobileFrom.ItemIndex];
  line := line + '"from":"' + extractID(idLine) + '", ' + recSep;
  line := line + '"to":[' + recSep;
  for idx:=0 to lbMobile.Items.Count - 2 do
    line := line  + lbMobile.Items[idx] + ',';

  line := line  + lbMobile.Items[idx] + '],' + recSep;
  line := line + '"sentTime":"' + cbSentYear.Items[cbSentYear.ItemIndex];
  line := line + cbSentMonth.Items[cbSentMonth.ItemIndex];
  line := line + cbSentDay.Items[cbSentDay.ItemIndex] + 'T' + timeSent.Text + 'Z"';
  line := line + '}]}' + recSep;
  Result := line;
end;

procedure TformTraceMessage.readTraceFromFile;

begin
  readTraceMobileFromFile;
  readTracePhoneAccountFromFile;
end;

procedure TformTraceMessage.readTraceMobileFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inModel, inMSISDN: Boolean;
  id, model, msisdn: string;
  listTrace: TStringList;
  idx:integer;
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
            cbMobileFrom.Items.Add(model + ' ' + msisdn + '@' + id);
            cbMobileTo.Items.Add(model + ' ' + msisdn + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;
end;


procedure TformTraceMessage.readTracePhoneAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inPhoneNumber: Boolean;
  id, phoneNumber: string;
  listTrace: TStringList;
  idx:integer;
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

          if jreader.Value.AsString = 'phoneNumber' then
            inPhoneNumber := True
          else
            inPhoneNumber := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inPhoneNumber then
          begin
            phoneNumber := jreader.Value.AsString;
            cbMobileFrom.Items.Add('Phone account ' + phoneNumber + '@' + id);
            cbMobileTo.Items.Add('Phone account ' + phoneNumber + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformTraceMessage.btnAddMobileClick(Sender: TObject);
var
  line: String;
begin
  if cbMobileFrom.ItemIndex = cbMobileTo.ItemIndex then
    ShowMessage('Mobile numbers FROM and TO are equal')
  else
  begin
    line := cbMobileTo.Items[cbMobileTo.ItemIndex];
    line := '"' + extractID(line) + '"';
    lbMobile.Items.Add(line);
  end;

end;

procedure TformTraceMessage.btnCancelClick(Sender: TObject);
begin
  formTraceMessage.Close;
end;

procedure TformTraceMessage.btnCloseClick(Sender: TObject);
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
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceMESSAGE.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_MESSAGE":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbMessage.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbMessage.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbMessage.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceMESSAGE.json');

  formTraceMessage.Close;
end;

procedure TformTraceMessage.btnModifyMessageClick(Sender: TObject);
begin
  if lbMessage.ItemIndex > - 1 then
    lbMessage.Items[lbMessage.ItemIndex] := prepareItemMessage('modify');
end;

procedure TformTraceMessage.btnAddMessageClick(Sender: TObject);
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

procedure TformTraceMessage.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceMessage.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceMessage.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine: String;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-traceMESSAGE.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceMESSAGE.json');
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

  formTraceMessage.ShowModal;
end;

end.
