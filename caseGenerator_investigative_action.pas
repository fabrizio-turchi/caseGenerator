{
The property of an InvestigativeAction are:

  - Instrument: @id from Warrant (serach and seizure) and Tool (acquisition, analysis)
  - Performer: @id from Role
  - Location: @id from Location
  - Object/Input: @id from Trace and Provencance_Record
  - Result: from Provenance_Record
}

unit caseGenerator_investigative_action;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers;

type
  TformInvestigativeAction = class(TForm)
    Label1: TLabel;
    lbInvestigativeAction: TListBox;
    Label2: TLabel;
    btnClose: TButton;
    btnAddAction: TButton;
    btnDeleteAction: TButton;
    Label6: TLabel;
    panelSource: TPanel;
    Label5: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    cbPerformer: TComboBox;
    cbInstrument: TComboBox;
    panelTarget: TPanel;
    Label10: TLabel;
    cbLocation: TComboBox;
    cbObject: TComboBox;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edInvestigativeAction: TEdit;
    cbStartDay: TComboBox;
    cbStartMonth: TComboBox;
    cbStartYear: TComboBox;
    timeStart: TTimeEdit;
    Label4: TLabel;
    cbEndDay: TComboBox;
    cbEndMonth: TComboBox;
    cbEndYear: TComboBox;
    timeEnd: TTimeEdit;
    cbProvenanceRecord: TComboBox;
    lbProvenanceRecords: TListBox;
    Label3: TLabel;
    btnAddPR: TButton;
    Button1: TButton;
    edArgumentName: TEdit;
    panelArguments: TPanel;
    edArgumentValue: TEdit;
    Label8: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    lbArguments: TListBox;
    btnArgumentAdd: TButton;
    btnArgumentRemove: TButton;
    procedure btnAddActionClick(Sender: TObject);
    procedure btnDeleteActionClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddPRClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnArgumentAddClick(Sender: TObject);
    procedure btnArgumentRemoveClick(Sender: TObject);
  private
    FuuidCase: string;
    FPathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property UuidCase: string read FuuidCase write SetuuidCase;
    property PathCase: String read FPathCase write SetPathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    procedure readRoleFromFile;
    procedure readLocationFromFile;
    procedure readToolFromFile;
    procedure readTraceFromFile;
    procedure readTraceMobileFromFile;
    procedure readTraceSIMFromFile;
    procedure readTraceFileFromFile;
    procedure readProvenanceRecordFromFile;
    procedure readWarrantFromFile;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase:String; uuidCase: String; investigativeAction:String);
    { Public declarations }
  end;

var
  formInvestigativeAction: TformInvestigativeAction;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformInvestigativeAction.btnDeleteActionClick(Sender: TObject);
begin
  if lbInvestigativeAction.ItemIndex > -1 then
  begin
    lbInvestigativeAction.Items.Delete(lbInvestigativeAction.ItemIndex);
    // if the list is empty it will be possible to add an Investigative Action
    lbInvestigativeAction.Enabled := True;
  end;

end;


procedure TformInvestigativeAction.Button1Click(Sender: TObject);
begin
  lbProvenanceRecords.Items.Delete(lbProvenanceRecords.ItemIndex);
end;

procedure TformInvestigativeAction.FormShow(Sender: TObject);
var
  idx: Integer;
begin
  cbStartYear.Items.Clear;
  cbEndYear.Items.Clear;
  for idx:=2000 to 2020 do
  begin
    cbStartYear.Items.Add(IntToStr(idx));
    cbEndYear.Items.Add(IntToStr(idx));
  end;

  cbStartYear.ItemIndex := 0;
  cbEndYear.ItemIndex := 0;

  cbInstrument.Items.Clear;
  lbArguments.Items.Clear;
  edArgumentName.Text := '';
  edArgumentValue.Text := '';
  cbPerformer.Items.Clear;
  cbLocation.Items.Clear;
  cbObject.Items.Clear;
  cbProvenanceRecord.Items.Clear;
  lbProvenanceRecords.Items.Clear;
  readToolFromFile;
  readWarrantFromFile;
  readRoleFromFile;
  readLocationFromFile;
  readTraceFromFile;
  readProvenanceRecordFromFile;
end;

function TformInvestigativeAction.JsonTokenToString(const t: TJsonToken): string;
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


procedure TformInvestigativeAction.readLocationFromFile;
var
  json, dir, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inID, inLocality, inRegion, inStreet: Boolean;
  id, locality, region, street: string;
  listLocation: TStringList;
  idx:integer;
begin
  dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(dir + '\' + FuuidCase + '-location.json') then
  begin
    listLocation := TStringList.Create;
    listLocation.LoadFromFile(dir + '\' + FuuidCase + '-location.json');
    //JSON string here
    json := stringreplace(listLocation.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'name' then
            inName := True
          else
            inName := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'locality' then
            inLocality := True
          else
            inLocality := False;

          if jreader.Value.AsString = 'region' then
            inRegion := True
          else
            inRegion := False;

          if jreader.Value.AsString = 'street' then
            inStreet := True
          else
            inStreet := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inName then
            name := jreader.Value.AsString;

          if inID then
            id := jreader.Value.AsString;

          if inLocality then
            locality := jreader.Value.AsString;

          if inRegion then
            region := jreader.Value.AsString;

          if inStreet then
          begin
            street := jreader.Value.AsString;
            cbLocation.Items.Add(street + ' ' + locality + ' ' + region + '@' + id);
            street := '';
            locality := '';
            region := '';
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

procedure TformInvestigativeAction.readProvenanceRecordFromFile;
var
  json, dir, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inDescription, inExhibitNumber, inID: Boolean;
  description, exhibitNumber, ID: string;
  listLocation: TStringList;
  idx:integer;
begin
  dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-provenance_record.json: fill in cbSourceIdentity component
  if FileExists(dir + '\' + FuuidCase + '-provenance_record.json') then
  begin
    listLocation := TStringList.Create;
    listLocation.LoadFromFile(dir + '\' + FuuidCase + '-provenance_record.json');
    //JSON string here
    json := stringreplace(listLocation.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'description' then
            inDescription := True
          else
            inDescription := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'exhibitNumber' then
            inExhibitNumber := True
          else
            inExhibitNumber := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inDescription then
            description := jreader.Value.AsString;

          if inID then
            id := jreader.Value.AsString;

          if inExhibitNumber then begin
            exhibitNumber := jreader.Value.AsString;
            cbObject.Items.Add(description + ' ' + exhibitNumber + ' ' + '@' + id);
            cbProvenanceRecord.Items.Add(description + ' ' + exhibitNumber + ' ' + '@' + id);
          end;


        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformInvestigativeAction.readRoleFromFile;
var
  json, dir, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inFamilyName, inID: Boolean;
  id, name: string;
  listRole: TStringList;
  idx:integer;
begin
  dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(dir + '\' + FuuidCase + '-role.json') then
  begin
    listRole := TStringList.Create;
    listRole.LoadFromFile(dir + '\' + FuuidCase + '-role.json');
    //JSON string here
    json := stringreplace(listRole.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);
      inName := False;
      inFamilyName := False;
      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'name' then
            inName := True
          else
            inName := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inName then begin
            name := jreader.Value.AsString;
            cbPerformer.Items.Add(name + ' ' + '@' + id);
            name := '';
            id := '';
          end;

          if inID then
            id := jreader.Value.AsString;;
        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformInvestigativeAction.readToolFromFile;
var
  json, dir, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inName, inVersion, inToolType: Boolean;
  id, name, version, toolType: string;
  listTrace: TStringList;
  idx:integer;
begin
  dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(dir + '\' + FuuidCase + '-tool.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(dir + '\' + FuuidCase + '-tool.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'name' then
            inName := True
          else
            inName := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'toolType' then
            inToolType := True
          else
            inToolType := False;

          if jreader.Value.AsString = 'version' then
            inVersion := True
          else
            inVersion := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inName then
            name := jreader.Value.AsString;

          if inToolType then
            toolType := jreader.Value.AsString;

          if inVersion then
          begin
            version := jreader.Value.AsString;
            //  the character # is to extract the name of the tool for setting the property "@type:name:ToolArguments" of
            //  the InvestigativeAction Object in CASE
            cbInstrument.Items.Add(name + '#' + toolType + ' ' + version + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformInvestigativeAction.readTraceFileFromFile;
var
  json, dir, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inName, inPath: Boolean;
  id, name, path: string;
  listTrace: TStringList;
  idx:integer;
begin
  dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-traceFILE.json: fill in cbObject component
  if FileExists(dir + '\' + FuuidCase + '-traceFILE.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(dir + '\' + FuuidCase + '-traceFILE.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'fileName' then
            inName := True
          else
            inName := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'filePath' then
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

          if inPath then begin
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

procedure TformInvestigativeAction.readTraceFromFile;

begin
{
  the Object/Input values of the InvestigativeAction are picked up by Trace only in
  case of search and seizure (preserved) in all other case the values must come from
  ProvenanceRecord
}
  if edInvestigativeAction.Text = 'preserved' then
  begin
    readTraceMobileFromFile;
    readTraceSIMFromFile;
    readTraceFileFromFile;
  //readTraceFromComputer;
  // readTraceDiskPartitionFromFile;
  end;


end;

procedure TformInvestigativeAction.readTraceMobileFromFile;
var
  json, dir, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inManufacturer, inModel, inSerial: Boolean;
  id, manufacturer, model, serial: string;
  listTrace: TStringList;
  idx:integer;
begin
  dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(dir + '\' + FuuidCase + '-traceMOBILE.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(dir + '\' + FuuidCase + '-traceMOBILE.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'manufacturer' then
            inManufacturer := True
          else
            inManufacturer := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'model' then
            inModel := True
          else
            inModel := False;

          if jreader.Value.AsString = 'serialNumber' then
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

procedure TformInvestigativeAction.readTraceSIMFromFile;
var
  json, dir, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inSimType, inICCID: Boolean;
  id, simType, ICCID: string;
  listTrace: TStringList;
  idx:integer;
begin
  dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(dir + '\' + FuuidCase + '-traceSIM.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(dir + '\' + FuuidCase + '-traceSIM.json');
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



procedure TformInvestigativeAction.readWarrantFromFile;
var
  json, dir, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inType, inIssuedDate: Boolean;
  id, typeWarrant, issuedDate: string;
  listTrace: TStringList;
  idx:integer;
begin
  dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(dir + '\' + FuuidCase + '-warrant.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(dir + '\' + FuuidCase + '-warrant.json');
    //JSON string here
    json := stringreplace(listTrace.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);

      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = '@type' then
            inType := True
          else
            inType := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'IssuedDate' then
            inIssuedDate := True
          else
            inIssuedDate := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inType then
            typeWarrant := jreader.Value.AsString;

          if inIssuedDate then
          begin
            issuedDate := jreader.Value.AsString;
            cbInstrument.Items.Add(typeWarrant + ' ' + issuedDate + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformInvestigativeAction.btnAddPRClick(Sender: TObject);
begin
  if cbProvenanceRecord.ItemIndex > -1 then
    lbProvenanceRecords.Items.Add(cbProvenanceRecord.Items[cbProvenanceRecord.ItemIndex]);
end;

procedure TformInvestigativeAction.btnArgumentAddClick(Sender: TObject);
begin
  if (Trim(edArgumentValue.Text) = '') or (Trim(edArgumentName.Text) = '') then
    ShowMessage('Name and/or Value Argument are empty')
  else
  begin
    lbArguments.Items.Add('"' + edArgumentName.Text + '":"' + edArgumentValue.Text + '"');
    edArgumentName.Text := '';
    edArgumentValue.Text := '';
  end;
end;

procedure TformInvestigativeAction.btnArgumentRemoveClick(Sender: TObject);
begin
  if lbArguments.ItemIndex > -1 then
    lbArguments.Items.Delete(lbArguments.ItemIndex);
end;

procedure TformInvestigativeAction.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line, dir:string;
  idx: integer;
begin
  if lbInvestigativeAction.Items.Count > 0 then
  begin
    //dir := GetCurrentDir;
    idx := 0;
    AssignFile(fileJSON, FPathCase + FUuidCase + '-investigative_action.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_INVESTIGATIVE_ACTION":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbInvestigativeAction.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbInvestigativeAction.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbInvestigativeAction.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    btnAddAction.Enabled := True;
    CloseFile(fileJSON);
  end;

  formInvestigativeAction.Close;
end;

procedure TformInvestigativeAction.btnAddActionClick(Sender: TObject);
var
  line, recSep, nameTool, idValue: string;
  Uid: TGUID;
  idx: Integer;
begin
    if (cbInstrument.ItemIndex = -1) then
    begin
      ShowMessage('Instrument is missing');
      Exit;
    end;
    if (cbPerformer.ItemIndex = -1) then
    begin
      ShowMessage('Performer is missing');
      Exit;
    end;
    if (cbLocation.ItemIndex = -1) then
    begin
      ShowMessage('Location is missing');
      Exit;
    end;
    if (cbObject.ItemIndex = -1) then
    begin
      ShowMessage('Object is missing');
      Exit;
    end;
    if (lbProvenanceRecords.Items.Count = 0) then
    begin
      ShowMessage('No prevenance record has been inserted in the list');
      Exit;
    end;

    idx := 0;

    CreateGUID(Uid);
    recSep := #30 + #30;
    line := '{"@id":"' + GuidToString(Uid) + '",';
    line := line + '"@type":"InvestigativeAction",';
    line := line + '"name":"' + edInvestigativeAction.Text + '",';
    line := line + '"startTime":"' + cbStartYear.Items[cbStartYear.ItemIndex] + '-';
    line := line + cbStartMonth.Items[cbStartMonth.ItemIndex] + '-';
    line := line + cbStartDay.Items[cbStartDay.ItemIndex] + 'T';
    line := line + TimeToStr(timeStart.Time) + 'Z", ' + recSep;
    line := line + '"endTime":"' + cbEndYear.Items[cbEndYear.ItemIndex] + '-';
    line := line + cbEndMonth.Items[cbEndMonth.ItemIndex] + '-';
    line := line + cbEndDay.Items[cbEndDay.ItemIndex] + 'T';
    line := line + TimeToStr(timeEnd.Time) + 'Z", ';
    line := line + '"propertyBundle":[';
    line := line + '{"@type":"ActionReferences",';
    idValue :=  cbInstrument.Items[cbInstrument.ItemIndex];
    idValue := Copy(idValue, Pos('@', idValue) + 1, Length(idValue));
    line := line + '"instrument":"' + idValue + '",';
    idValue := cbLocation.Items[cbLocation.ItemIndex];
    idValue := Copy(idValue, Pos('@', idValue) + 1, Length(idValue));
    line := line + '"location":"' + idValue + '",';
    idValue :=  cbPerformer.Items[cbPerformer.ItemIndex];
    idValue := Copy(idValue, Pos('@', idValue) + 1, Length(idValue));
    line := line + '"performer":"' + idValue + '",';
    idValue := cbObject.Items[cbObject.ItemIndex];
    idValue := Copy(idValue, Pos('@', idValue) + 1, Length(idValue));
    line := line + '"object":[' + recSep + '"' + idValue + '"],';
    line := line + '"result":[';


    for idx:= 0 to lbProvenanceRecords.Items.Count - 2 do
    begin
      idValue := lbProvenanceRecords.Items[idx];
      idValue := Copy(idValue, Pos('@', idValue) + 1, Length(idValue));
      line := line + '"' + idValue + '",';
    end;

    if lbProvenanceRecords.Items.Count > 0 then
    begin
      idValue := lbProvenanceRecords.Items[idx];
      idValue := Copy(idValue, Pos('@', idValue) + 1, Length(idValue));
      line := line + '"' + idValue + '"]}';
    end;

    if lbArguments.Items.Count > 0 then
    begin
      idx := 0;
      nameTool := cbInstrument.Items[cbInstrument.ItemIndex];
      nameTool := Copy(nameTool, 1, Pos('#', nameTool) - 1);
      line := line + ',{' + '"@type":"' + nameTool + ':ToolArguments",' + recSep;
      for idx := 0 to lbArguments.Items.Count - 2 do
        line := line + lbArguments.Items[idx] + ',' + recSep;

      line := line + lbArguments.Items[idx] + '}'
    end;
    //else
      //line := line + '}' + recSep;

    line := line + ']}';
    lbInvestigativeAction.Items.Add(line);
    btnAddAction.Enabled := False;  // only one single InbestigativeAction can be added to the list
end;

procedure TformInvestigativeAction.SetPathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformInvestigativeAction.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformInvestigativeAction.ShowWithParamater(pathCase:String; uuidCase: String; investigativeAction: String);
var
  fileJSON: TextFile;
  line, subLine, dir:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-investigative_action.json
  if FileExists(FPathCase + FUuidCase + '-investigative_action.json') then
  begin
    AssignFile(fileJSON,  FPathCase + FUuidCase + '-investigative_action.json');
    Reset(fileJSON);
    lbInvestigativeAction.Items.Clear;
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

        lbInvestigativeAction.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');
  edInvestigativeAction.Text := investigativeAction;
  formInvestigativeAction.ShowModal;
end;

end.
