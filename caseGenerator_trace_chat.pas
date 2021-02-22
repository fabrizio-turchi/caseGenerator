unit caseGenerator_trace_chat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers, FMX.ScrollBox, FMX.Memo, caseGenerator_util,
  FMX.Memo.Types;

type
  TformTraceChat = class(TForm)
    Label1: TLabel;
    lbMessages: TListBox;
    btnClose: TButton;
    btnAddMessage: TButton;
    btnRemoveMessage: TButton;
    Label6: TLabel;
    panelFrom: TPanel;
    Label5: TLabel;
    Label8: TLabel;
    panelTo: TPanel;
    cbAccountTo: TComboBox;
    Label11: TLabel;
    Label13: TLabel;
    memoMessageText: TMemo;
    btnAddAccountTo: TButton;
    btnRemoveAccountTo: TButton;
    lbAccountTo: TListBox;
    cbSentMonth: TComboBox;
    timeSent: TTimeEdit;
    cbSentYear: TComboBox;
    Label3: TLabel;
    btnCancel: TButton;
    btnModifyMessage: TButton;
    cbSentDay: TComboBox;
    Label2: TLabel;
    Label4: TLabel;
    cbMessageType: TComboBox;
    edNewChatName: TEdit;
    Label9: TLabel;
    cbChatName: TComboBox;
    btnNewChatName: TButton;
    edNumMessages: TEdit;
    cbAppName: TComboBox;
    Label7: TLabel;
    cbAccountFrom: TComboBox;
    procedure btnAddMessageClick(Sender: TObject);
    procedure btnRemoveMessageClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddAccountToClick(Sender: TObject);
    procedure btnRemoveAccountToClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyMessageClick(Sender: TObject);
    procedure lbAccountToChange(Sender: TObject);
    procedure btnNewChatNameClick(Sender: TObject);
    procedure cbChatNameChange(Sender: TObject);
    procedure lbMessagesClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    FindexChat: Integer;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    procedure SetindexChat(const Value: Integer);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    property indexChat: Integer read FindexChat write SetindexChat;
    function JsonTokenToString(const t: TJsonToken): string;
    procedure readTraceFromFile;
    procedure readTraceApplicationAccountFromFile;
    procedure readTraceChatFromFile;
    procedure readTraceChatMessagesFromFile;
    procedure readTraceChatMessagesFromFileOLD;
    procedure readTraceApplicationFromFile;
    function  extractID(line, charId: String): String;
    function extractLastID(line: String): String;
    function prepareItemMessage(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceChat: TformTraceChat;

implementation
{$R *.fmx}
uses StrUtils;

type TChatMessage = record
      id: String;
			text: string;
			appId: string;
			dateTime: string;
			fromId: string;
			toId: string;
			status: string;
			outcome: string;
			kind: string;
end;

const MAX_CHATS = 20;
const MAX_MSGS = 20;

  type
  // max 30 messages per Chat
  TChatArray = array [1..MAX_CHATS] of String;

var
  //max 20 Chats
  chatMatrix : array[1..MAX_MSGS] of TChatArray;



{ TForm1 }

procedure TformTraceChat.btnRemoveMessageClick(Sender: TObject);
var
  nMsg, idx: Integer;
begin
  if lbMessages.ItemIndex > -1 then
  begin


{    for idx := lbMessage.ItemIndex + 1 to MAX_MSGS do
      chatMatrix[cbChatName.ItemIndex + 1][idx + 1] :=
      chatMatrix[cbChatName.ItemIndex + 1][idx];

    chatMatrix[cbChatName.ItemIndex + 1][MAX_MSGS] := '';
}
    lbMessages.Items.Delete(lbMessages.ItemIndex);
    nMsg := StrToInt(edNumMessages.Text);
    Dec(nMsg);
    edNumMessages.Text := IntToStr(nMsg);

  end;
end;

procedure TformTraceChat.btnRemoveAccountToClick(Sender: TObject);
begin
  lbAccountTo.Items.Delete(lbAccountTo.ItemIndex);
end;


procedure TformTraceChat.btnNewChatNameClick(Sender: TObject);
var
  Uid: TGUID;
  guidNoBraces: String;
begin
  if (Trim(edNewChatName.Text) <> '')  then
  begin
    CreateGUID(Uid);
    guidNoBraces := ':' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
    cbChatName.Items.Add(edNewChatName.Text + StringofChar(' ', 100) + '@' +  guidNoBraces);
    cbChatName.ItemIndex := cbChatName.Count - 1;
    edNumMessages.Text := '0';
    edNewChatName.Text := '';
  end;
end;

procedure TformTraceChat.cbChatNameChange(Sender: TObject);
var
  idx: Integer;
begin
  if cbChatName.ItemIndex > -1 then
  begin
//  if the previous Index was linked to a Chat, I'll save the relative
//  messages in the chatMatrix
    if FIndexChat > - 1 then
    begin
      for idx := 0 to lbMessages.Count - 1 do
        chatMatrix[FIndexChat + 1][idx + 1] := lbMessages.Items[idx];
      for idx:= lbMessages.Count to MAX_MSGS do
        chatMatrix[FIndexChat + 1][idx] := '';
    end;
    SetIndexChat(cbChatName.ItemIndex);
    lbMessages.Items.Clear;
    for idx:= 1 to Length(chatMatrix[cbChatName.ItemIndex + 1]) do
    begin
      if chatMatrix[cbChatName.ItemIndex + 1][idx] = '' then
        break;
      lbMessages.Items.Add(chatMatrix[cbChatName.ItemIndex + 1][idx]);
    end;
    edNumMessages.Text := IntToStr(lbMessages.Items.Count);

  end;
end;

function TformTraceChat.extractID(line, charId: String): String;
begin
  Result := Copy(line, Pos(charId, line) + Length(charId), Length(line));
end;



function TformTraceChat.extractLastID(line: String): String;
begin
   Result := Copy(line, LastDelimiter('@', line) + 1, Length(line));
end;

procedure TformTraceChat.FormShow(Sender: TObject);
var
  idx, idy: Integer;
begin
  idy := 0;
  for idx := 2020 to 2030 do
  begin
    cbSentYear.Items.Add(IntToStr(idx));
    if (IntToStr(CurrentYear) = IntToStr(idx)) then
      cbSentYear.ItemIndex := idy;
    Inc(idy);
  end;

  cbAppName.Items.Clear;
  memoMessageText.Text := '';
  edNewChatName.Text := '';
  cbSentDay.ItemIndex := 0;
  cbSentMonth.ItemIndex := 0;
  cbAccountFrom.ItemIndex := -1;
  cbAccountTo.ItemIndex := -1;
  lbAccountTo.Items.Clear;
  cbChatName.ItemIndex := - 1;
  SetindexChat(cbChatName.ItemIndex);

  readTraceFromFile;

end;

function TformTraceChat.JsonTokenToString(const t: TJsonToken): string;
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


procedure TformTraceChat.lbMessagesClick(Sender: TObject);
var
  line, field, uuidTo, sentDate, sDay, sMonth, sYear, sDate, messageType: String;
  idx, posTo : Integer;
begin
  if lbMessages.ItemIndex > - 1 then
  begin
    line := lbMessages.Items[lbMessages.ItemIndex];
    field := ExtractRefID(line, '"uco-observable:application":"');
    for idx:=0 to cbAppName.Items.Count - 1 do
      if Pos(field, cbAppName.Items[idx]) > 0   then
      begin
        cbAppName.ItemIndex := idx;
        break;
      end;

    field := ExtractField(line, '"uco-observable:messageType":"');
    for idx := 0 to cbMessageType.Count - 1 do
    begin
      if cbMessageType.Items[idx] = field then
        cbMessageType.ItemIndex := idx;
    end;

    memoMessageText.Lines.Text := ExtractField(line, '"uco-observable:messageText":"');

    field := ExtractRefId(line, '"uco-observable:from":{');
    for idx:=0 to cbAccountFrom.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbAccountFrom.Items[idx], field) then
      begin
        cbAccountFrom.ItemIndex := idx;
        break;
      end;
    end;

    //accountTo := TStringList.Create;

    field := ExtractRefId(line, '"uco-observable:to":[');

{    accountTo := ExtractArray(lbMessages.Items[lbMessages.ItemIndex], '"uco-observable:to":[');
    if accountTo.Count > 0 then
    begin
      for idx:=0 to accountTo.Count - 1 do
        lbAccountTo.Items.Add('"' + accountTo[idx] + '"');
      lbAccountTo.ItemIndex := 0;
    end;
}
    lbAccountTo.Items.Clear;
    for idx:=0 to cbAccountTo.Count - 1 do
      if Pos(field, cbAccountTo.Items[idx]) > 0  then
      begin
        cbAccountTo.ItemIndex := idx;
        break;
      end;
    lbAccountTo.Items.Add(cbAccountTo.Items[cbAccountTo.ItemIndex]);

    sentDate := ExtractDateValue(lbMessages.Items[lbMessages.ItemIndex], '"uco-observable:sentTime":{');
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
    field := ExtractField(lbMessages.Items[lbMessages.ItemIndex], '"uco-observable:messageType":"');
    for idx:=0 to cbMessageType.Items.Count - 1 do
    begin
      if cbMessageType.Items[idx] = field then
      begin
        cbMessageType.ItemIndex := idx;
        break;
      end;
    end;
  end;
end;

procedure TformTraceChat.lbAccountToChange(Sender: TObject);
//var
  //line: String;
  //idx: Integer;
begin
  {
  if lbAccount.ItemIndex > - 1 then
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
  }

end;

function TformTraceChat.prepareItemMessage(operation: String): String;
var
  line, recSep, indent, guidNoBraces, sId, sValue: string;
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
    guidNoBraces :=  ExtractField(lbMessages.Items[lbMessages.ItemIndex], '"@id":"');

  line := line + indent + '"@id":"' + guidNoBraces + '",' + recSep;
  line := line + indent + '"@type":"uco-observable:CyberItem",' + recSep;
  line := line + indent + '"uco-core:facets":[' + recSep;
  line := line + indent + '{' + recSep;
  line := line + RepeatString(indent, 2) + '"@type":"uco-observable:Message",' + recSep;
  line := line + RepeatString(indent, 2) + '"uco-observable:messageText":"' + memoMessageText.Text + '", ' + recSep;
  sId := extractID(cbAppName.Items[cbAppName.ItemIndex], '@');
  line := line + RepeatString(indent, 2) + '"uco-observable:application":' + recSep;
  line := line + RepeatString(indent, 3) + '{' + recSep;
  line := line + RepeatString(indent, 3) + '"@id":"' + sId + '"' + recSep;
  line := line + RepeatString(indent, 3) + '},' + recSep;
  line := line + '"uco-observable:sentTime":{' + recSep;
  line := line + '"@type":"xsd:dateTime",' + recSep;
  line := line + '"@value":"' +  cbSentYear.Items[cbSentYear.ItemIndex];
  line := line + cbSentMonth.Items[cbSentMonth.ItemIndex];
  line := line + cbSentDay.Items[cbSentDay.ItemIndex] + 'T' + timeSent.Text + '"},' + recSep;
  sId := ExtractID(cbAccountFrom.Items[cbAccountFrom.ItemIndex], '@id');
  line := line + RepeatString(indent, 2) + '"uco-observable:from":{"' + sId + '"}, ' + recSep;
  sId := ExtractID(cbAccountTo.Items[cbAccountFrom.ItemIndex], '@id');
  line := line + RepeatString(indent, 2) + '"uco-observable:to":[' + recSep;
  idx := 0;
  for idx:=0 to lbAccountTo.Items.Count - 2 do
  begin
    sId := ExtractID(lbAccountTo.Items[idx], '@id');
    line := line  + RepeatString(indent, 2) + '{"@id":"' + sId + '"},';
  end;

  sId := ExtractID(lbAccountTo.Items[idx], '@id');
  line := line  + RepeatString(indent, 2) + '{"@id":"' + sId + '"}],' + recSep;
  line := line  + indent + '"uco-observable:allocationStatus":"Intact",' + recSep;
  line := line  + indent +  '"uco-observable:__outcome":"",' + recSep;
  sValue := cbMessageType.Items[cbMessageType.ItemIndex];
  line := line  + indent + 	'"uco-observable:messageType":"' + sValue + '"' + recSep;
  line := line  + indent + '}]' + recSep + '}' + recSep;
  Result := line;
end;

procedure TformTraceChat.readTraceFromFile;

begin
  readTraceApplicationFromFile;
  readTraceApplicationAccountFromFile;
  // readTraceChatFromFile;  traceCHAT is read in ShowWithParameter method
  readTraceChatMessagesFromFile;
end;

procedure TformTraceChat.readTraceApplicationAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inIssuer, inIdentifier, inDigitalAccount, firstId: Boolean;
  id, issuer, identifier, digitalAccount: string;
  listTrace: TStringList;
  idx, nHypens: integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  firstId := false;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceAPPLICATION_ACCOUNT.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceAPPLICATION_ACCOUNT.json');
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
          begin
            if not firstId then
            begin
              inID := True;
              firstId := true
            end
            else
              inID := False;
          end
          else
            inID := false;

          if jreader.Value.AsString = 'uco-observable:accountIssuer' then
            inIssuer := True
          else
            inIssuer := False;

          if jreader.Value.AsString = 'uco-observable:applicationIdentifier' then
            inIdentifier := True
          else
            inIdentifier := False;

          if jreader.Value.AsString = 'uco-observable:displayName' then
            inDigitalAccount := True
          else
            inDigitalAccount := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := Copy(jreader.Value.AsString, 1, 37);  // only the guuid

          if inIssuer then
            issuer := jreader.Value.AsString;

          if inIdentifier then
            identifier := jreader.Value.AsString;

          if inDigitalAccount then
          begin
            digitalAccount := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);
            cbAccountFrom.Items.Add(issuer + ' ' + identifier + ' ' + digitalAccount + '@id' + id);
            cbAccountTo.Items.Add(issuer + ' ' + identifier + ' ' + digitalAccount + '@id' + id);
            firstId := false;
            id := '';
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;
end;


procedure TformTraceChat.readTraceApplicationFromFile
;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inAppName: Boolean;
  id, appName: string;
  listTrace: TStringList;
  idx, nHypens: Integer;
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
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-core:name' then
            inAppName := True
          else
            inAppName := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := Copy(jreader.Value.AsString, 1, 37);

          if inAppName then
          begin
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            appName := jreader.Value.AsString;
            cbAppName.Items.Add(appName + StringofChar(' ', 100) +  '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformTraceChat.readTraceChatFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inChatName, inChatNumMsg, inID: Boolean;
  chatName, chatNumMsg, id: string;
  listTrace: TStringList;
  idx, nHypens: integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceCHAT.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceCHAT.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'uco-observable:displayName' then
            inChatName := True
          else
            inChatName := False;

        if jreader.Value.AsString = 'olo:length' then
            inChatNumMsg := True
          else
            inChatNumMsg := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;
        end;

        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := Copy(jreader.Value.AsString, 1, 37);

          if inChatName then
            chatName := jreader.Value.AsString;

          if inChatNumMsg then
          begin
            chatNumMsg := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);
            cbChatName.Items.Add(chatName + stringOfChar(' ', 30)+ ' @' + id);
          end;
        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformTraceChat.readTraceChatMessagesFromFile;
var
  listMessages: TStringList;
  i, j, nMsg: integer;
  chatFile, chatNum, line: String;
begin
  for i := 1 to cbChatName.Count do
  begin
    if i < 10 then
      chatNum := '0' + IntToStr(i)
    else
      chatNum := IntToStr(i);

    chatFile :=  FpathCase + FuuidCase + '-traceCHAT_MESSAGES_' + chatNum + '.json';

    if FileExists(chatFile) then
    begin
      listMessages := TStringList.Create;
      listMessages.LoadFromFile(chatFile);
      // iterate over messages of the Chat n. i
      nMsg := 0;
      for j:=0 to  listMessages.Count - 1 do
      begin
        line := listMessages[j];
        if (line = '{') or (line = '}') or  (line = ']') or (AnsiContainsStr(line, 'OBJECTS_'))  then  // first or last line or root element
          continue;
        Inc(nMsg);
        chatMatrix[i][nMsg] := listMessages[j];
      end;

    end;
  end;
{
  if cbChatName.Count > 0  then
    for i := 1 to Length(chatMatrix[1]) do
      lbMessages.Items.Add(chatMatrix[1][i]);
}
end;

procedure TformTraceChat.readTraceChatMessagesFromFileOLD;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inChatText, inChatAppId, inChat, inChatDateTime, inChatFromId: Boolean;
  inChatToId, inChatKind, inChatStatus, inChatOutcome, inId: Boolean;
  chatText, chatAppId, chatDateTime, chatFromId, chatToId: String;
  chatKind, chatStatus, chatOutcome, id, sChat: string;
  listTrace: TStringList;
  i, idx, nHypens, numMessage: integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read all files JSON related to chat messages of each Chat,
  // each file corresponds to the messages of a Chat
  for i:=0 to cbChatName.Count - 1 do
  begin
    if i < 10 then
      sChat := '0' + IntToStr(i)
    else
      sChat := IntToSTr(i);

    if FileExists(FpathCase + FuuidCase + '-traceCHAT_MESSAGES_' + sChat + '.json') then
    begin
      listTrace := TStringList.Create;
      listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceCHAT_MESSAGES_' + sChat + '.json');
      numMessage := 0;
      //JSON string here
      json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
      try
        sreader := TStringReader.Create(json);
        jreader := TJsonTextReader.Create(sreader);

        while jreader.Read do
        begin
          if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
          begin
            if jreader.Value.AsString = 'uco-observable:messageText' then
              inChatText := True
            else
              inChatText := False;

            if jreader.Value.AsString = 'uco-observable:application' then
              inChatAppId := True
            else
              inChatAppId := False;

            if jreader.Value.AsString = '@value' then
              inChatDateTime := True
            else
              inChatDateTime := False;

            if jreader.Value.AsString = 'uco-observable:from' then
              inChatFromId := True
            else
              inChatFromId := False;

            if jreader.Value.AsString = 'uco-observable:to' then
              inChatToId := True
            else
              inChatToId := False;

            if jreader.Value.AsString = 'uco-observable:allocationStatus' then
              inChatStatus := True
            else
              inChatStatus := False;

            if jreader.Value.AsString = 'uco-observable:messageType' then
              inChatKind := True
            else
              inChatKind := False;

            if jreader.Value.AsString = '@id' then
              inID := True
            else
              inID := False;
          end;

          if JsonTokenToString(jreader.TokenType) = 'String' then
          begin
            if inID then
              id := Copy(jreader.Value.AsString, 1, 37);
            if inChatText then
              chatText := jreader.Value.AsString;
            if inChatAppId then
              chatAppId := jreader.Value.AsString;
            if inChatDateTime then
              chatDateTime := jreader.Value.AsString;
            if inChatFromId then
              chatFromId := jreader.Value.AsString;
            if inChatToId then
              chatToId := jreader.Value.AsString;
            if inChatStatus then
              chatStatus := jreader.Value.AsString;
            if inChatKind then
            begin
              chatKind := jreader.Value.AsString;
              nHypens := CountOccurrences('-', id);
              (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                    for instance for Identity it can be @id:"...-...-SimpleName" ---*)
              if nHypens > 4  then
                id := Copy(id, 1, LastDelimiter('-', id) - 1);
              chatMatrix[i][numMessage] := chatText + ' ' + chatAppId +
                chatDatetime + chatFromId + chatToId + chatStatus + chatKind + '@' + id;
              Inc(numMessage);
            end;
          end;
        end;
      finally
        jreader.Free;
        sreader.Free;
      end;
    end;
  end;

  if cbChatName.Count > 0 then
  begin
    for i:=0 to  length(chatMatrix[1]) do
      lbMessages.Items.Add(chatMatrix[1][i+1]);
  end;


end;

procedure TformTraceChat.btnAddAccountToClick(Sender: TObject);
var
  line: String;
begin

  if cbAccountFrom.ItemIndex = cbAccountTo.ItemIndex then
    if cbAccountFrom.ItemIndex >  -1 then
      ShowMessage('Application account FROM and TO are equal')
  else
  begin
    line := cbAccountTo.Items[cbAccountTo.ItemIndex];
    lbAccountTo.Items.Add(line);
  end;

end;

procedure TformTraceChat.btnCancelClick(Sender: TObject);
begin
  formTraceChat.Close;
end;

procedure TformTraceChat.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line, chatThreadId, chatThreadName, chatId, recSep, crlf: String;
  chatFromId, chatToId, chatNum, chatMessagesFile: String;
  idx, idy, idz, nMsgs, idPos: integer;
begin
  // save current Chat messages into chatMatrix
  for idx:=0 to lbMessages.Count - 1 do
  begin
    chatMatrix[cbChatName.ItemIndex + 1][idx + 1] :=
      lbMessages.Items[idx];
  end;

  recSep := #30 + #30;
  crlf := #13 + #10;

  AssignFile(fileJSON, FpathCase + FuuidCase + '-traceCHAT.json', CP_UTF8);
  Rewrite(fileJSON);  // create new file
  WriteLn(fileJSON, '{');
  line := #9 + '"OBJECTS_CHAT":[';
  WriteLn(fileJSON, line);

  if cbChatName.Items.Count > 0 then
  begin
    idx := 0;
    for idx:=0 to  cbChatName.Count -1 do
    begin
      idPos := Pos('@', cbChatName.Items[idx]);
      chatThreadName := Trim(Copy(cbChatName.Items[idx], 1, idPos - 1));
      chatThreadId := Copy(cbChatName.Items[idx], idPos + 1, Length(cbChatName.Items[idx]));
      line := '{"@id":"' + chatThreadId + '",' + recSep;
      line := line + '"@type":"uco-observable:CyberItem", ' + recSep;
      line := line + '"uco-core:facets":[' + recSep;
      line := line + stringOfChar(#9, 2) + '{' + recSep;
      line := line + stringOfChar(#9, 2) + '"@type":"uco-observable:MessageThread",' + recSep;
      line := line + stringOfChar(#9, 2) + '"uco-observable:displayName":"' + chatThreadName + '",' + recSep;
      line := line + stringOfChar(#9, 2) + '"uco-observable:messages":{' + recSep;
      nMsgs := 0;
      for idz:= 1 to Length(chatMatrix[idx + 1]) do
      begin
        if chatMatrix[idx + 1][idz] <> '' then
          Inc(nMsgs)
        else
          break;
      end;

      line := line + stringOfChar(#9, 2) + '"olo:length":"' +
              IntToStr(nMsgs) + '",' + recSep;
      line := line + stringOfChar(#9, 2) + '"olo:slot":[' + recSep;
      for idy:=1 to Length(chatMatrix[idx + 1]) do
      begin
        if chatMatrix[idx + 1][idy] = '' then
          break;
        line := line + stringOfChar(#9, 3) + '{' + recSep;
        line := line + stringOfChar(#9, 3) + '"olo:index":"' + IntToStr(idy) + '",' + recSep;
        line := line + stringOfChar(#9, 3) + '"olo:item": {' + recSep;
        chatId := ExtractField(chatMatrix[idx + 1][idy], '@id":"');
        line := line + stringOfChar(#9, 4) + '"@id":"' + chatId + '"}'+ recSep;
        line := line + stringOfChar(#9, 3) + '},' + recSep;
      end;
      // get rid of last comma
      line := Copy(line, 1, Length(line) - 3) + recSep;

      line := line + stringOfChar(#9, 2) + ']' + recSep;
      line := line + #9 + '},' + recSep;
      line := line + '"uco-observable:participants":[' + recSep;
      chatFromId := ExtractField(chatMatrix[idx +1][1], '"uco-observable:from":{"');

      chatToId := ExtractField(chatMatrix[idx +1][1], '"uco-observable:to":[' + recSep + '{"');
      line := line + '{"@id":"' +  chatFromId + '"},' + recSep;
      line := line + '{"@id":"' +  chatToId + '"}]' + recSep;
      line := line + stringOfChar(#9, 3) + '}'  + recSep;
      line := line + stringOfChar(#9, 2) + ']' + recSep;
      line := line + #9 + '}' + recSep;
      WriteLn(fileJSON, UTF8Encode(line));
    end;
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceCHAT.json');

    // write traceCHAT_MESSAGES_xx files
  if cbChatName.Items.Count > 0 then
  begin
    for idx:=1 to cbChatName.Count do
    begin
      if idx < 10 then
        chatNum := '0' + IntToStr(idx)
      else
        chatNum := IntToStr(idx);
      chatMessagesFile := FpathCase + FuuidCase + '-traceCHAT_MESSAGES_' + chatNum + '.json';
      AssignFile(fileJSON, chatMessagesFile, CP_UTF8);
      Rewrite(fileJSON);  // create new file
      WriteLn(fileJSON, '{');
      line := #9 + '"OBJECTS_CHAT_MESSAGES":[';
      WriteLn(fileJSON, line);
      for idy := 1 to Length(chatMatrix[idx]) do
      begin
         if chatMatrix[idx][idy] = '' then
          break;
         WriteLn(fileJSON, chatMatrix[idx][idy]);
      end;
      CloseFile(fileJSON);
    end;

  end;

  formTraceChat.Close;
end;

procedure TformTraceChat.btnModifyMessageClick(Sender: TObject);
begin
  if lbMessages.ItemIndex > - 1 then
    lbMessages.Items[lbMessages.ItemIndex] := prepareItemMessage('modify');
end;

procedure TformTraceChat.btnAddMessageClick(Sender: TObject);
var
  line, recSep, idLine: string;
  Uid: TGUID;
  idx: Integer;
begin
  if (lbAccountTo.Items.Count = -1) or (cbAccountFrom.ItemIndex = -1)  then
  begin
    ShowMessage('Mobile FROM or/and Mobile Source empty!');
    Exit;
  end;

  if cbAppName.ItemIndex = -1 then
  begin
    ShowMessage('Application name is not selected!');
    Exit;
  end;

  if cbMessageType.ItemIndex = -1 then
  begin
    ShowMessage('Message type is not selected!');
    Exit;
  end;


  lbMessages.Items.Add(prepareItemMessage('add'));
  idx := StrToInt(edNumMessages.Text);
  Inc(idx);
  edNumMessages.Text := IntToStr(idx);
  cbAppName.ItemIndex := -1;
  memoMessageText.Lines.Clear;
  cbAccountFrom.ItemIndex := -1;
  cbAccountTo.ItemIndex := -1;
  lbAccountTo.Items.Clear;
end;

procedure TformTraceChat.SetindexChat(const Value: Integer);
begin
  FindexChat := Value;
end;

procedure TformTraceChat.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceChat.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceChat.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, chatThreadName, chatThreadId: String;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-traceCHAT.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceCHAT.json', CP_UTF8);
    Reset(fileJSON);
    cbChatName.Items.Clear;
    while not Eof(fileJSON) do
    begin
      ReadLn(fileJSON, line);
      line := Trim(line);
      if (line = '{') or (line = '}') or  (line = ']') or (AnsiContainsStr(line, 'OBJECTS_'))  then  // first or last line or root element
      else
      begin
        chatThreadName := ExtractField(line, '"uco-observable:displayName":"');
        chatThreadId := ExtractField(line, '"@id":"');
        cbChatName.Items.Add(chatThreadName + stringOfChar(' ', 100) + '@' + chatThreadId);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTraceChat.ShowModal;
end;

end.
