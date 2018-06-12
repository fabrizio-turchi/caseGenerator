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
    procedure btnAddPRClick(Sender: TObject);
    procedure btnDeletePRClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbProvenanceRecordChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
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
    procedure readTraceSIMFromFile;
    procedure readTraceFileFromFile;
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


procedure TformProvenanceRecord.FormShow(Sender: TObject);
var
  idx: Integer;
begin
{
  The Provenance Record object take the object property values from all Trace Objects
}
  cbObject.Items.Clear;
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
procedure TformProvenanceRecord.lbProvenanceRecordChange(Sender: TObject);
var
  line, creationDate, sDate, sDay, sMonth, sYear, idObject: string;
  idx: Integer;
begin
  if lbProvenanceRecord.ItemIndex > - 1 then
  begin
    line := lbProvenanceRecord.Items[lbProvenanceRecord.ItemIndex];
    edDescription.Text := ExtractField(line, '"description":"');
    edExhibitNumber.Text := ExtractField(line, '"exhibitNumber":"');
    creationDate := ExtractField(line, '"createdTime":"');
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
  end;
  idObject := ExtractField(line, '"object":"');
  for idx:=0 to cbObject.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbObject.Items[idx], idObject) then
      begin
        cbObject.ItemIndex := idx;
        break;
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

procedure TformProvenanceRecord.readTraceFromFile;

begin
  readTraceMobileFromFile;
  readTraceSIMFromFile;
  readTraceFileFromFile;
  //readTraceFromComputer;
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
    line := #9 + '"OBJECTS_PROVENANCERECORD":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbProvenanceRecord.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbProvenanceRecord.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbProvenanceRecord.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end;

  formProvenanceRecord.Close;
end;

procedure TformProvenanceRecord.btnAddPRClick(Sender: TObject);
var
  line, recSep, sObject: string;
  Uid: TGUID;
begin
  if (edDescription.Text = '') or (cbObject.ItemIndex = -1)  then
    ShowMessage('Description and/or Object are missing!')
  else
  begin
    CreateGUID(Uid);
    recSep := #30 + #30;
    line := '{"@id":"' + GuidToString(Uid) + '",' + recSep;
    line := line + '"@type":"ProvenanceRecord",' + recSep;
    line := line + '"createdTime":"' + cbPRYear.Items[cbPRYear.ItemIndex] + '-';
    line := line +  cbPRMonth.Items[cbPRMonth.ItemIndex] + '-';
    line := line +  cbPRDay.Items[cbPRDay.ItemIndex] + 'T';
    line := line +  TimeToStr(timePR.Time) + 'Z", ' + recSep;
    line := line +  '"description":"' + edDescription.Text + '", ' + recSep;
    line := line +  '"exhibitNumber":"' + edExhibitNumber.Text + '", ' + recSep;
    line := line +  '"object":"';
    sObject := cbObject.Items[cbObject.ItemIndex];
    line := line + Copy(sObject, Pos('@', sObject) +1, Length(sObject)) + '"}';

    lbProvenanceRecord.Items.Add(line);

    edDescription.Text := '';
    edExhibitNumber.Text := '';
    cbPRDay.ItemIndex := 0;
    cbPRMonth.ItemIndex := 0;
    cbPRYear.ItemIndex := 0;
    cbObject.ItemIndex := -1;

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
