unit caseGenerator_provenance_record;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers, caseGenerator_util;

type
  TformProvenanceRecord = class(TForm)
    Label1: TLabel;
    lbProvenanceRecord: TListBox;
    btnClose: TButton;
    btnAddPR: TButton;
    btnDeletePR: TButton;
    edDescription: TEdit;
    panelPR: TPanel;
    cbObject: TComboBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edExhibitNumber: TEdit;
    Label2: TLabel;
    cbPRDay: TComboBox;
    cbPRMonth: TComboBox;
    cbPRYear: TComboBox;
    timePR: TTimeEdit;
    btnCancel: TButton;
    btnModifyPR: TButton;
    lbObjects: TListBox;
    Label3: TLabel;
    btnAddObject: TButton;
    btnModifyObject: TButton;
    btnRemoveObkect: TButton;
    procedure btnAddPRClick(Sender: TObject);
    procedure btnDeletePRClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbProvenanceRecordChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyPRClick(Sender: TObject);
    procedure btnAddObjectClick(Sender: TObject);
    procedure btnModifyObjectClick(Sender: TObject);
    procedure btnRemoveObkectClick(Sender: TObject);
    procedure lbObjectsChange(Sender: TObject);
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
    procedure readTraceFromComputer;
    procedure readTraceFromDiskPartition;
    procedure readTraceSIMFromFile;
    procedure readTraceFileFromFile;
    procedure readTraceMessageFromFile;
    procedure readTraceEmailAccountFromFile;
    procedure readTracePhoneAccountFromFile;
    function prepareProvenanceRecord(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formProvenanceRecord: TformProvenanceRecord;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformProvenanceRecord.btnDeletePRClick(Sender: TObject);
begin
  lbProvenanceRecord.Items.Delete(lbProvenanceRecord.ItemIndex);
end;


procedure TformProvenanceRecord.btnModifyObjectClick(Sender: TObject);
var
  line: String;
begin
  if (lbObjects.ItemIndex > - 1) and (cbObject.ItemIndex > - 1) then
  begin
    line := cbObject.Items[cbObject.ItemIndex];
    lbObjects.Items[lbObjects.ItemIndex] := '"' + Copy(line, Pos('@', line) + 1, Length(line)) + '"';
  end;
end;

procedure TformProvenanceRecord.btnModifyPRClick(Sender: TObject);
begin
  if lbProvenanceRecord.ItemIndex > - 1 then
    lbProvenanceRecord.Items[lbProvenanceRecord.ItemIndex] := prepareProvenanceRecord('modify');
end;

procedure TformProvenanceRecord.btnRemoveObkectClick(Sender: TObject);
begin
  if lbObjects.ItemIndex > - 1 then
    lbObjects.Items.Delete(lbObjects.ItemIndex);
end;

procedure TformProvenanceRecord.FormShow(Sender: TObject);
var
  idx: Integer;
begin
{
  The Provenance Record object take the object property values from all Trace Objects
}
  cbObject.Items.Clear;
  lbObjects.Items.Clear;
  edDescription.Text := '';
  edExhibitNumber.Text := '';
  for idx:=2000 to  2020 do
    cbPRYear.Items.Add(IntToStr(idx));
  // Fill in the combo box cbObject with Trace Object read from files (MOBILE, SIM, FILE)
  readTraceFromFile;
end;

function TformProvenanceRecord.JsonTokenToString(const t: TJsonToken): string;
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
procedure TformProvenanceRecord.lbObjectsChange(Sender: TObject);
var
  line: String;
  idx: Integer;
begin
  if lbObjects.ItemIndex > - 1 then
  begin
    idx := 0;
    line := lbObjects.Items[lbObjects.ItemIndex];
    line := stringreplace(line, '"', '',[rfReplaceAll]);
    for idx := 0 to cbObject.Count - 1 do
    begin
      if AnsiContainsStr(cbObject.Items[idx], line) then
      begin
        cbObject.ItemIndex := idx;
        break;
      end;
    end;
  end;
end;

procedure TformProvenanceRecord.lbProvenanceRecordChange(Sender: TObject);
var
  line, creationDate, sDate, sDay, sMonth, sYear, idObject: string;
  idx: Integer;
  objectList: TStringList;
  exitLoop: Boolean;
begin
  if lbProvenanceRecord.ItemIndex > - 1 then
  begin
    lbObjects.Items.Clear;
    line := lbProvenanceRecord.Items[lbProvenanceRecord.ItemIndex];
    edDescription.Text := ExtractField(line, '"uco-investigation:description":"');
    edExhibitNumber.Text := ExtractField(line, '"uco-investigation:exhibitNumber":"');
    creationDate := ExtractDataField(line, '"uco-investigation:createdTime":');
    sDate := Copy(creationDate, 1, 10);
    sDay := Copy(sDate, 9, 2);
    for idx:=0 to cbPRDay.Items.Count - 1 do
    begin
      if cbPRDay.Items[idx] = sDay then
      begin
        cbPRDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 6, 2);
    for idx:=0 to cbPRMonth.Items.Count - 1 do
    begin
      if cbPRMonth.Items[idx] = sMonth then
      begin
        cbPRMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbPRYear.Items.Count - 1 do
    begin
      if cbPRYear.Items[idx] = sYear then
      begin
        cbPRYear.ItemIndex := idx;
        break;
      end;
    end;

    timePR.Text := Copy(creationDate, 12, 8);
    objectList := ExtractArray(line, '"uco-investigation:object":[');
    exitLoop := false;

    for idx := 0 to objectList.Count -1 do
      lbObjects.Items.Add('"' + objectList[idx] + '"');

    lbObjects.ItemIndex := 0; //slect the first object of the list that activates the OnChange event on lbObjects

  end;
end;

function TformProvenanceRecord.prepareProvenanceRecord(operation: String): String;
var
  line, recSep, indent, sObject, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30;
  indent := '   ';

  if operation = 'add' then
  begin
    CreateGUID(Uid);
    guidNoBraces := ':' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
  end
  else
    guidNoBraces :=  ExtractField(lbProvenanceRecord.Items[lbProvenanceRecord.ItemIndex], '"@id":"');

  line := '{' + recSep + indent + '"@id":"' + guidNoBraces + '",' + recSep;
  line := line + indent + '"@type":"uco-investigation:ProvenanceRecord",' + recSep;
  line := line + indent + '"uco-investigation:createdTime":' + recSep;
  line := line + '{"@type":"xsd:dateTime",' + recSep;
  line := line + '"@value":"' +  cbPRYear.Items[cbPRYear.ItemIndex] + '-';
  line := line +  cbPRMonth.Items[cbPRMonth.ItemIndex] + '-';
  line := line +  cbPRDay.Items[cbPRDay.ItemIndex] + 'T';
  line := line +  TimeToStr(timePR.Time) + 'Z"}, ' + recSep;
  line := line +  indent + '"uco-investigation:description":"' + edDescription.Text + '", ' + recSep;
  line := line +  indent + '"uco-investigation:exhibitNumber":"' + edExhibitNumber.Text + '", ' + recSep;
  line := line +  indent + '"uco-investigation:object":[';
  idx := 0;
  for idx:=0 to lbObjects.Count - 2 do
    line := line +  RepeatString(indent, 2)  + lbObjects.Items[idx] + ',';

  line := line +  RepeatString(indent, 2)  + lbObjects.Items[idx] + recSep + indent + ']' + recSep + '}';
  Result := line;
end;

procedure TformProvenanceRecord.readTraceEmailAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inEmailAddress, inID: Boolean;
  id, emailAddress: string;
  listTrace: TStringList;
  idx:integer;
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
          if jreader.Value.AsString = 'uco-observable:emailAddress' then
            inEmailAddress := True
          else
            inEmailAddress := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inEmailAddress then
          begin
            emailAddress := stringreplace(jreader.Value.AsString, '@', 'AT', [rfReplaceAll]);
            cbObject.Items.Add(emailAddress + ' '  + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformProvenanceRecord.readTraceFileFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inName, inPath: Boolean;
  id, name, path: string;
  listTrace: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceFILE.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceFILE.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'uco-observable:fileName' then
            inName := True
          else
            inName := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-observable:filePath' then
            inPath := True
          else
            inPath := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inName then
            name := jreader.Value.AsString;

          if inPath then
          begin
            path := jreader.Value.AsString;
            cbObject.Items.Add(name + ' ' + path + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformProvenanceRecord.readTraceFromComputer;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inManufacturer, inModel: Boolean;
  id, manufacturer, model: string;
  listTrace: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceCOMPUTER.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceCOMPUTER.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'uco-observable:manufacturer' then
            inManufacturer := True
          else
            inManufacturer := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-observable:model' then
            inModel := True
          else
            inModel := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inManufacturer then
            manufacturer := jreader.Value.AsString;

          if inModel then
          begin
            model := jreader.Value.AsString;
            cbObject.Items.Add('computer ' + manufacturer + '/' + model + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformProvenanceRecord.readTraceFromDiskPartition;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inDiskPartitionType, inPartitionLength: Boolean;
  id, diskPartitionType, partitionLength: string;
  listTrace: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceDISK_PARTITION.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceDISK_PARTITION.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'uco-observable:diskPartitionType' then
            inDiskPartitionType := True
          else
            inDiskPartitionType := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-observable:partitionLength' then
            inPartitionLength := True
          else
            inPartitionLength := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inDiskPartitionType then
            diskPartitionType := jreader.Value.AsString;

          if inPartitionLength then
          begin
            partitionLength := jreader.Value.AsString;
            cbObject.Items.Add('DISK ' + diskPartitionType + '/' + partitionLength + ' ' + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformProvenanceRecord.readTraceFromFile;

begin
  readTraceMobileFromFile;
  readTraceSIMFromFile;
  readTraceFileFromFile;
  readTraceMessageFromFile;
  readTraceEmailAccountFromFile;
  readTracePhoneAccountFromFile;
  readTraceFromComputer;
  readTraceFromDiskPartition;
end;

procedure TformProvenanceRecord.readTraceMessageFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inApplication, inMessageText: Boolean;
  id, application, messageText: string;
  listTrace: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceMESSAGE.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceMESSAGE.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'uco-observable:application' then
            inApplication := True
          else
            inApplication := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-observable:messageText' then
            inMessageText := True
          else
            inMessageText := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inApplication then
            application := jreader.Value.AsString;

          if inMessageText then
          begin
            messageText := jreader.Value.AsString;
            cbObject.Items.Add(application + ' ' + messageText + ' ' + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformProvenanceRecord.readTraceMobileFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inManufacturer, inModel, inSerial: Boolean;
  id, manufacturer, model, serial: string;
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
          if jreader.Value.AsString = 'uco-observable:manufacturer' then
            inManufacturer := True
          else
            inManufacturer := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-observable:model' then
            inModel := True
          else
            inModel := False;

          if jreader.Value.AsString = 'uco-observable:serialNumber' then
            inSerial := True
          else
            inSerial := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inManufacturer then
            manufacturer := jreader.Value.AsString;

          if inModel then
            model := jreader.Value.AsString;

          if inSerial then
          begin
            serial := jreader.Value.AsString;
            cbObject.Items.Add(manufacturer + ' ' + model + ' ' + serial + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;
end;

procedure TformProvenanceRecord.readTracePhoneAccountFromFile;
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

          if jreader.Value.AsString = 'uco-observable:phoneNumber' then
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
            cbObject.Items.Add('Phone account ' + phoneNumber + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformProvenanceRecord.readTraceSIMFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inSimType, inICCID: Boolean;
  id, simType, ICCID: string;
  listTrace: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceSIM.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceSIM.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'SIMType' then
            inSimType := True
          else
            inSimType := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'ICCID' then
            inICCID := True
          else
            inICCID := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inSIMType then
            simType := jreader.Value.AsString;

          if inICCID then
          begin
            ICCID := jreader.Value.AsString;
            cbObject.Items.Add(simType + ' ' + ICCID + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;
end;

procedure TformProvenanceRecord.btnCancelClick(Sender: TObject);
begin
  formProvenanceRecord.Close;
end;

procedure TformProvenanceRecord.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line, dir:string;
  idx: integer;
begin
  if lbProvenanceRecord.Items.Count > 0 then
  begin
    //dir := GetCurrentDir;
    idx := 0;
    AssignFile(fileJSON, FpathCase + FuuidCase + '-provenance_record.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_PROVENANCE_RECORD":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbProvenanceRecord.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbProvenanceRecord.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbProvenanceRecord.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-provenance_record.json');

  formProvenanceRecord.Close;
end;

procedure TformProvenanceRecord.btnAddObjectClick(Sender: TObject);
var
  line : String;
begin
  if cbObject.ItemIndex > - 1 then
  begin
    line := cbObject.Items[cbObject.ItemIndex];
    lbObjects.Items.Add('"' + Copy(line, Pos('@', line) + 1, Length(line)) + '"');
  end;
end;

procedure TformProvenanceRecord.btnAddPRClick(Sender: TObject);
begin
  if (edDescription.Text = '') or (lbObjects.Items.Count = 0)  then
    ShowMessage('Description and/or list Objects are missing!')
  else
  begin
    lbProvenanceRecord.Items.Add(prepareProvenanceRecord('add'));

    edDescription.Text := '';
    edExhibitNumber.Text := '';
    cbPRDay.ItemIndex := 0;
    cbPRMonth.ItemIndex := 0;
    cbPRYear.ItemIndex := 0;
    cbObject.ItemIndex := -1;
    lbObjects.Items.Clear;
  end;
end;

procedure TformProvenanceRecord.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformProvenanceRecord.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformProvenanceRecord.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine, dir:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-provenance_record.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-provenance_record.json');
    Reset(fileJSON);
    lbProvenanceRecord.Items.Clear;
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

        lbProvenanceRecord.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formProvenanceRecord.ShowModal;
end;

end.
