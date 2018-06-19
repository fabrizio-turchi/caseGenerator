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
  System.JSON.Readers, caseGenerator_util;

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
    btnCancel: TButton;
    btnModifyInvestigativeAction: TButton;
    btnModifyProvenanceRecord: TButton;
    btnModify: TButton;
    cbActions: TComboBox;
    cbActionsName: TComboBox;
    Label16: TLabel;
    edDescription: TEdit;
    Label17: TLabel;
    procedure btnAddActionClick(Sender: TObject);
    procedure btnDeleteActionClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddPRClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnArgumentAddClick(Sender: TObject);
    procedure btnArgumentRemoveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lbInvestigativeActionChange(Sender: TObject);
    procedure btnModifyInvestigativeActionClick(Sender: TObject);
    procedure btnModifyProvenanceRecordClick(Sender: TObject);
    procedure btnModifyClick(Sender: TObject);
    procedure lbArgumentsChange(Sender: TObject);
    procedure lbProvenanceRecordsChange(Sender: TObject);
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
    procedure extractProvenanceRecordDescription(ListIdProvenance: TStringList);
    procedure readWarrantFromFile;
    function prepareItemInvestigativeAction: String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase:String; uuidCase: String);
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


procedure TformInvestigativeAction.btnModifyClick(Sender: TObject);
begin
  if lbArguments.ItemIndex > - 1  then
  begin
      if (Trim(edArgumentValue.Text) = '') or (Trim(edArgumentName.Text) = '') then
        ShowMessage('Name and/or Value Argument are empty')
      else
      begin
        lbArguments.Items[lbArguments.ItemIndex] := '"' + edArgumentName.Text + '":"' + edArgumentValue.Text + '"';
        edArgumentName.Text := '';
        edArgumentValue.Text := '';
      end;
  end;

end;

procedure TformInvestigativeAction.btnModifyInvestigativeActionClick(
  Sender: TObject);
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

  if lbInvestigativeAction.ItemIndex > -1 then
  begin
    lbInvestigativeAction.Items[lbInvestigativeAction.ItemIndex] := prepareItemInvestigativeAction();
    btnAddAction.Enabled := False;  // only one single InbestigativeAction can be added to the list
  end;

end;

procedure TformInvestigativeAction.btnModifyProvenanceRecordClick(
  Sender: TObject);
begin
  if cbProvenanceRecord.ItemIndex > -1 then
    lbProvenanceRecords.Items[lbProvenanceRecords.ItemIndex] := cbProvenanceRecord.Items[cbProvenanceRecord.ItemIndex];

end;

procedure TformInvestigativeAction.Button1Click(Sender: TObject);
begin
  lbProvenanceRecords.Items.Delete(lbProvenanceRecords.ItemIndex);
end;

procedure TformInvestigativeAction.extractProvenanceRecordDescription(ListIdProvenance: TStringList);
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inDescription, inExhibitNumber, inID: Boolean;
  description, exhibitNumber, ID: string;
  listLocation: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-provenance_record.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-provenance_record.json') then
  begin
    listLocation := TStringList.Create;
    listLocation.LoadFromFile(FpathCase + FuuidCase + '-provenance_record.json');
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
            for idx:= 0 to  ListIdProvenance.Count - 1 do
              if AnsiContainsStr(ListIdProvenance[idx], id) then
                lbProvenanceRecords.Items.Add(description + ' ' + exhibitNumber + ' ' + '@' + id);
          end;
        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

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

  cbActionsName.Visible := False;
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


procedure TformInvestigativeAction.lbArgumentsChange(Sender: TObject);
var
  line : String;
  colonPos: Integer;
begin
  if lbArguments.ItemIndex > - 1 then
  begin
      line := lbArguments.Items[lbArguments.ItemIndex];
      colonPos := Pos(':', line);
      edArgumentName.Text := Copy(line, 1, colonPos - 1);
      edArgumentName.Text := stringreplace(edArgumentName.Text, '"', '',[rfReplaceAll]);
      edArgumentValue.Text := Copy(line, colonPos + 1, Length(line));
      edArgumentValue.Text := stringreplace(edArgumentValue.Text, '"', '',[rfReplaceAll]);
  end;

end;

procedure TformInvestigativeAction.lbInvestigativeActionChange(Sender: TObject);
var
  line, recSep, startTime, endTime, sDate, sDay, sMonth, sYear, sField: String;
  idx, idy, commaPos: Integer;
  provenanceStringList, objectList: TStringList;
  exitLoop: Boolean;
begin
  if lbInvestigativeAction.ItemIndex > -1  then
  begin
    line := lbInvestigativeAction.Items[lbInvestigativeAction.ItemIndex];
    sField := ExtractField(line, '"name":"');
    for idx:= 0 to cbActions.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbActionsName.Items[idx], sField) then
      begin
        cbActions.ItemIndex := idx;
        break;
      end;
    end;

    edDescription.Text := ExtractField(line, '"description":"');

    startTime := ExtractField(line, '"startTime":"');
    sDate := Copy(startTime, 1, 10);
    sDay := Copy(sDate, 9, 2);
    for idx:=0 to cbStartDay.Items.Count - 1 do
    begin
      if cbStartDay.Items[idx] = sDay then
      begin
        cbStartDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 6, 2);
    for idx:=0 to cbStartMonth.Items.Count - 1 do
    begin
      if cbStartMonth.Items[idx] = sMonth then
      begin
        cbStartMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbStartYear.Items.Count - 1 do
    begin
      if cbStartYear.Items[idx] = sYear then
      begin
        cbStartYear.ItemIndex := idx;
        break;
      end;
    end;

    timeStart.Text := Copy(startTime, 12, 8);

    endTime := ExtractField(line, '"endTime":"');
    sDate := Copy(endTime, 1, 10);
    sDay := Copy(sDate, 9, 2);
    for idx:=0 to cbEndDay.Items.Count - 1 do
    begin
      if cbEndDay.Items[idx] = sDay then
      begin
        cbEndDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 6, 2);
    for idx:=0 to cbEndMonth.Items.Count - 1 do
    begin
      if cbEndMonth.Items[idx] = sMonth then
      begin
        cbEndMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbEndYear.Items.Count - 1 do
    begin
      if cbEndYear.Items[idx] = sYear then
      begin
        cbEndYear.ItemIndex := idx;
        break;
      end;
    end;

    timeEnd.Text := Copy(endTime, 12, 8);

    sField := ExtractField(line, '"instrument":"');
    for idx := 0 to cbInstrument.Items.Count -1 do
    begin
      if AnsiContainsStr(cbInstrument.Items[idx], sField) then
      begin
        cbInstrument.ItemIndex := idx;
        break;
      end;
    end;

    lbArguments.Items.Clear;
    recSep := #30 + #30;
    if (Pos('"@type":"ConfigurationSetting",', line) > 0) then
    begin
      sField := Copy(line, Pos('"@type":"ConfigurationSetting",', line) + 31, Length(line));
      sField := stringreplace(sField, recSep, '',[rfReplaceAll]);
      commaPos := Pos(',', sField);
      while commaPos > 0 do
      begin
        lbArguments.Items.Add(Copy(sField, 1, commaPos - 1));
        sField := Copy(sField, commaPos + 1, Length(sField));
        commaPos := Pos(',', sField);
      end;
      sField := stringreplace(sField, ']', '',[rfReplaceAll]);
      sField := stringreplace(sField, '}', '',[rfReplaceAll]);
      lbArguments.Items.Add(sField);
    end;



    sField := ExtractField(line, '"performer":"');
    for idx := 0 to cbPerformer.Items.Count -1 do
    begin
      if AnsiContainsStr(cbPerformer.Items[idx], sField) then
      begin
        cbPerformer.ItemIndex := idx;
        break;
      end;
    end;

    sField := ExtractField(line, '"location":"');
    for idx := 0 to cbLocation.Items.Count -1 do
    begin
      if AnsiContainsStr(cbLocation.Items[idx], sField) then
      begin
        cbLocation.ItemIndex := idx;
        break;
      end;
    end;

    objectList := ExtractArray(line, '"object":[');
    exitLoop := false;

    for idx := 0 to cbObject.Items.Count -1 do
    begin
      for idy:=0 to objectList.Count - 1 do
      begin
        if AnsiContainsStr(cbObject.Items[idx], objectList[idy]) then
        begin
          cbObject.ItemIndex := idx;
          exitLoop :=  True;
          break;
        end;
        if exitLoop then
          break;
      end;
    end;

    provenanceStringList := TStringList.Create;
    provenanceStringList := ExtractArrayID(line, '"result":[');
    extractProvenanceRecordDescription(provenanceStringList);

  end;
end;

procedure TformInvestigativeAction.lbProvenanceRecordsChange(Sender: TObject);
var
  line, idValue: String;
  idPos, idx: Integer;
begin
  if lbProvenanceRecords.ItemIndex > - 1 then
  begin
    line := lbProvenanceRecords.Items[lbProvenanceRecords.ItemIndex];
    idPos := Pos('@', line);
    idValue := Copy(line, idPos + 1, Length(line));
    for idx:= 0 to cbProvenanceRecord.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbProvenanceRecord.Items[idx], idValue) then
      begin
        cbProvenanceRecord.ItemIndex := idx;
        break;
      end;
    end;
  end;
end;

function TformInvestigativeAction.prepareItemInvestigativeAction: String;
var
  line, recSep, nameTool, idValue: string;
  Uid: TGUID;
  idx: Integer;
begin

    idx := 0;
    CreateGUID(Uid);
    recSep := #30 + #30;
    line := '{"@id":"' + GuidToString(Uid) + '",';
    line := line + '"@type":"InvestigativeAction",';
    line := line + '"name":"' + cbActionsName.Items[cbActions.ItemIndex] + '",';
    line := line + '"description":"' + edDescription.Text + '",';
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
      line := line + ',{' + '"@type":"ConfigurationSetting",' + recSep;
      for idx := 0 to lbArguments.Items.Count - 2 do
        line := line + lbArguments.Items[idx] + ',' + recSep;

      line := line + lbArguments.Items[idx] + '}'
    end;
    //else
      //line := line + '}' + recSep;

    line := line + ']}';
    Result := line;
end;

procedure TformInvestigativeAction.readLocationFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inID, inLocality, inRegion, inStreet: Boolean;
  id, locality, region, street: string;
  listLocation: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-location.json') then
  begin
    listLocation := TStringList.Create;
    listLocation.LoadFromFile(FpathCase + FuuidCase + '-location.json');
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
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inDescription, inExhibitNumber, inID: Boolean;
  description, exhibitNumber, ID: string;
  listLocation: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-provenance_record.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-provenance_record.json') then
  begin
    listLocation := TStringList.Create;
    listLocation.LoadFromFile(FpathCase + FuuidCase + '-provenance_record.json');
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
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inFamilyName, inID: Boolean;
  id, name: string;
  listRole: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-role.json') then
  begin
    listRole := TStringList.Create;
    listRole.LoadFromFile(FpathCase + FuuidCase + '-role.json');
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
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inName, inVersion, inToolType: Boolean;
  id, name, version, toolType: string;
  listTrace: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-tool.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-tool.json');
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
  // read file JSON uuidCase-traceFILE.json: fill in cbObject component
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
//  if cbActionsName.Items[cbActions.ItemIndex] = 'preserved' then
//  begin
    readTraceMobileFromFile;
    readTraceSIMFromFile;
    readTraceFileFromFile;
  //readTraceFromComputer;
  // readTraceDiskPartitionFromFile;
//  end;


end;

procedure TformInvestigativeAction.readTraceMobileFromFile;
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



procedure TformInvestigativeAction.readWarrantFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inType, inIssuedDate: Boolean;
  id, typeWarrant, issuedDate: string;
  listTrace: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-warrant.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-warrant.json');
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

procedure TformInvestigativeAction.btnCancelClick(Sender: TObject);
begin
  formInvestigativeAction.Close;
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
  end
  else
    deleteFile(FpathCase + FuuidCase + '-investigative_action.json');

  formInvestigativeAction.Close;
end;

procedure TformInvestigativeAction.btnAddActionClick(Sender: TObject);
begin
    if (cbActions.ItemIndex = -1) then
    begin
      ShowMessage('Type of investigative action is missing');
      Exit;
    end;

    if (Trim(edDescription.Text) = '') then
    begin
      ShowMessage('Description of investigative action is missing');
      Exit;
    end;

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

    lbInvestigativeAction.Items.Add(prepareItemInvestigativeAction());
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

procedure TformInvestigativeAction.ShowWithParamater(pathCase:String; uuidCase: String);
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
  //edInvestigativeAction.Text := investigativeAction;
  formInvestigativeAction.ShowModal;
end;

end.
