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
    cbEmailFrom: TComboBox;
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
    procedure btnAddMessageClick(Sender: TObject);
    procedure btnRemoveMessageClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddEmailClick(Sender: TObject);
    procedure btnRemoveMobileClick(Sender: TObject);
    procedure lbMessageChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyMessageClick(Sender: TObject);
    procedure lbMessagesChange(Sender: TObject);
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

procedure TformTraceEmail.btnRemoveMobileClick(Sender: TObject);
begin
  lbMobile.Items.Delete(lbMobile.ItemIndex);
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
  readTraceEmailAccountFromFile;
  cbEmailFrom.ItemIndex := -1;
  cbEmailTo.ItemIndex := -1;
  lbMessage.Items.Clear;

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
  line, sentDate, sDay, sMonth, sYear, sDate, messageType: String;
  idx: Integer;
  mobileTo: TStringList;
begin
  if lbMessage.ItemIndex > - 1 then
  begin
    lbMobile.Items.Clear;
    line := lbMessage.Items[lbMessage.ItemIndex];
    edApplication.Text := ExtractField(line, '"uco-observable:application":"');
    memoMessageText.Lines.Text := ExtractField(line, '"uco-observable:messageText":"');
    line := ExtractField(line, '"uco-observable:from":"');
    for idx:=0 to cbMobileFrom.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbMobileFrom.Items[idx], line) then
      begin
        cbMobileFrom.ItemIndex := idx;
        Break;
      end;
    end;
    mobileTo := ExtractArray(lbMessage.Items[lbMessage.ItemIndex], '"uco-observable:to":[');
    if mobileTo.Count > 0 then
    begin
      for idx:=0 to mobileTo.Count - 1 do
        lbMobile.Items.Add('"' + mobileTo[idx] + '"');
      lbMobile.ItemIndex := 0;
    end;

    sentDate := ExtractField(lbMessage.Items[lbMessage.ItemIndex], '"uco-observable:sentTime":"');
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
    messageType := ExtractField(lbMessage.Items[lbMessage.ItemIndex], '"uco-observable:messageType":"');
    for idx:=0 to cbMessageType.Items.Count - 1 do
    begin
      if cbMessageType.Items[idx] = messageType then
      begin
        cbMessageType.ItemIndex := idx;
        break;
      end;
    end;

  end;

    // read trace-MOBILE fro extracting all ID with model and MSISDN


end;

procedure TformTraceEmail.lbMessagesChange(Sender: TObject);
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
    guidNoBraces := ':' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
  end
  else
    guidNoBraces :=  ExtractField(lbMessage.Items[lbMessage.ItemIndex], '"@id":"');

  line := line + indent + '"@id":"' + guidNoBraces + '",' + recSep;

		"uco-observable:body":" <!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN' 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'> <html xmlns='http://www.w3.org/1999/xhtml' dir='ltr'>  <head> <style type='text/css'>  .link:link, .link:active, .link:visited {        color:#2672ec !important;        text-decoration:none !important;  }   .link:hover {        color:#4284ee !important;        text-decoration:none !important;  } </style> <title></title> </head> <body> <table dir='ltr'>       <tr><td id='i1' style='padding:0; font-family:'Segoe UI Semibold', 'Segoe UI Bold', 'Segoe UI', 'Helvetica Neue Medium', Arial, sans-serif; font-size:17px; color:#707070;'>Microsoft account</td></tr>       <tr><td id='i2' style='padding:0; font-family:'Segoe UI Light', 'Segoe UI', 'Helvetica Neue Medium', Arial, sans-serif; font-size:41px; color:#2672ec;'>Verify your email address</td></tr>       <tr><td id='i4' style='padding:0; padding-top:25px; font-family:'Segoe UI', Tahoma, Verdana, Arial, sans-serif; font-size:14px; color:#2a2a2a;'>To finish setting up your Microsoft account, we just need to make sure this email address is yours.</td></tr>       <tr><td style='padding:0; padding-top:25px; font-family:'Segoe UI', Tahoma, Verdana, Arial, sans-serif; font-size:14px; color:#2a2a2a;'>To verify your email address use this security code: <span style='font-family:'Segoe UI Bold', 'Segoe UI Semibold', 'Segoe UI', 'Helvetica Neue Medium', Arial, sans-serif; font-size:14px; font-weight:bold; color:#2a2a2a;'>7182</span></td></tr>       <tr><td id='i6' style='padding:0; padding-top:25px; font-family:'Segoe UI', Tahoma, Verdana, Arial, sans-serif; font-size:14px; color:#2a2a2a;'>If you didn't request this code, you can safely ignore this email. Someone else might have typed your email address by mistake.</td></tr>       <tr><td style='padding:0; padding-top:25px; font-family:'Segoe UI', Tahoma, Verdana, Arial, sans-serif; font-size:14px; color:#2a2a2a;'>Thanks,</td></tr>       <tr><td id='i8' style='padding:0; font-family:'Segoe UI', Tahoma, Verdana, Arial, sans-serif; font-size:14px; color:#2a2a2a;'>The Microsoft account team</td></tr> </table> </body> </html>",
		"uco-observable:subject":"Verify your email address",
		"uco-observable:__status":"Intact"
		}
	]
},
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
  msgTime := msgTime +  TimeToStr(emailTimen.Time);
  line := line + '"@value":"' + msgTime + '"},' recSep;
  idLine := cbEmailFrom.Items[cbEmailFrom.ItemIndex];
  line := line + '"uco-observable:fromRef":"' + idLine + '",' + recSep;
  idLine := cbEmailTo.Items[cbEmailFrom.ItemIndex];
  line := line + '"uco-observable:toRef":["' + idLine + '"],' + recSep;
  line := line + '"uco-observable:ccRefs":[],' + recSep;
  line := line + '"uco-observable:bccRefs":[],' + recSep;
  line := line + '"uco-observable:body":"' + memoMessageText.Text + '",' + recSep;

  idx := 0;
  for idx:=0 to lbMobile.Items.Count - 2 do
    line := line  + RepeatString(indent, 2) + lbMobile.Items[idx] + ',';

  line := line  + RepeatString(indent, 2) + lbMobile.Items[idx] + '],' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:sentTime":"' + cbSentYear.Items[cbSentYear.ItemIndex];
  line := line + cbSentMonth.Items[cbSentMonth.ItemIndex];
  line := line + cbSentDay.Items[cbSentDay.ItemIndex] + 'T' + timeSent.Text + 'Z",' + recSep;
  line := line + '"uco-observable:messageType":"' + cbMessageType.Items[cbMessageType.ItemIndex] +'"' + recSep;
  line := line  + indent + '}]' + recSep + '}' + recSep;
  Result := line;
end;

procedure TformTraceEmail.readTraceEmailAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inEmailAddress: Boolean;
  id, emailAddress: string;
  listTrace: TStringList;
  idx, nHypens: integer;
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

          if jreader.Value.AsString = 'uco-observable:EmailAccount' then
            inEmailAddress := True
          else
            inEmailAddress := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := Copy(jreader.Value.AsString, 1, 37);

          if inPhoneNumber then
          begin
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            emailAddress := jreader.Value.AsString;
            cbEmailFrom.Items.Add('Email account ' + emailAddress + '@' + id);
            cbEmailTo.Items.Add('Email account ' + emailAddress + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;



procedure TformTraceEmail.btnAddEmailClick(Sender: TObject);
var
  line: String;
begin
  if cbMobileFrom.ItemIndex = cbMobileTo.ItemIndex then
    ShowMessage('Mobile numbers FROM and TO are equal')
  else
  begin
    line := cbMobileTo.Items[cbMobileTo.ItemIndex];
    line := '"' + extractLastID(line) + '"';
    lbMobile.Items.Add(line);
  end;

end;

procedure TformTraceEmail.btnCancelClick(Sender: TObject);
begin
  formTraceSMS.Close;
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
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceMESSAGE.json', CP_UTF8);
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_MESSAGE":[';
    WriteLn(fileJSON, UTF8Encode(line));


    for idx:= 0 to lbMessage.Items.Count - 2 do
      WriteLn(fileJSON, UTF8Encode(#9#9 + lbMessage.Items[idx] + ','));

    WriteLn(fileJSON, UTF8Encode(#9#9 + lbMessage.Items[idx]));
    WriteLn(fileJSON, UTF8Encode(#9#9 + ']'));
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceMESSAGE.json');

  formTraceSMS.Close;
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
  if (cbEmailFrom.ItemIndex = 0-1 or (cbEmailTo.ItemIndex = -1)  then
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
