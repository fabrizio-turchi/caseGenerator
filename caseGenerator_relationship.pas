unit caseGenerator_relationship;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers, caseGenerator_util;

type
  TformRelationship = class(TForm)
    Label1: TLabel;
    lbRelationship: TListBox;
    edNameRelationship: TEdit;
    edSourceID: TEdit;
    Label2: TLabel;
    btnClose: TButton;
    btnAddRelationship: TButton;
    btnDeleteRelationship: TButton;
    cbDefaultKinds: TComboBox;
    Label6: TLabel;
    edTargetID: TEdit;
    panelSource: TPanel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    cbSourceIdentity: TComboBox;
    cbSourceRole: TComboBox;
    cbSourceTrace: TComboBox;
    panelTarget: TPanel;
    Label10: TLabel;
    cbTargetLocation: TComboBox;
    cbtargetRole: TComboBox;
    cbTargetTrace: TComboBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label3: TLabel;
    Label14: TLabel;
    Label4: TLabel;
    cbDirectional: TComboBox;
    btnModifyRelationship: TButton;
    btnCancel: TButton;
    procedure btnAddRelationshipClick(Sender: TObject);
    procedure btnDeleteRelationshipClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cbDefaultKindsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbSourceIdentityChange(Sender: TObject);
    procedure cbSourceRoleChange(Sender: TObject);
    procedure cbSourceTraceChange(Sender: TObject);
    procedure cbtargetRoleChange(Sender: TObject);
    procedure cbTargetLocationChange(Sender: TObject);
    procedure cbTargetTraceChange(Sender: TObject);
    procedure lbRelationshipChange(Sender: TObject);
    procedure btnModifyRelationshipClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    procedure readIdentityFromFile;
    procedure readRoleFromFile;
    procedure readAppAccountFromFile;
    procedure readLocationFromFile;
    procedure readTraceFromFile;
    procedure readTraceMobileFromFile;
    procedure readTraceSIMFromFile;
    procedure readTraceComputerFromFile;
    procedure readTraceDiskFromFile;
    procedure readTraceDiskPartitionFromFile;
    procedure readTraceEmailAccountFromFile;
    procedure readTraceFileFromFile;
    procedure readTraceMessageFromFile;
    function prepareObjectCaseLine(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formRelationship: TformRelationship;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformRelationship.btnDeleteRelationshipClick(Sender: TObject);
begin
  lbRelationship.Items.Delete(lbRelationship.ItemIndex);
end;

procedure TformRelationship.btnModifyRelationshipClick(Sender: TObject);
begin
  if lbRelationship.ItemIndex > -1 then
    lbRelationship.Items[lbRelationship.ItemIndex] := prepareObjectCaseLine('modify');
end;

procedure TformRelationship.cbDefaultKindsChange(Sender: TObject);
begin
  if cbDefaultKinds.ItemIndex = 0   then // has_role
  begin
    cbSourceIdentity.Enabled := True;
    cbSourceRole.Enabled := False;
    cbSourceTrace.Enabled := False;
    cbTargetRole.Enabled  := True;
    cbTargetTrace.Enabled  := False;
    cbTargetLocation.Enabled  := False;
  end;

  if cbDefaultKinds.ItemIndex = 1   then // has_device
  begin
    cbSourceIdentity.Enabled := True;
    cbSourceRole.Enabled := False;
    cbSourceTrace.Enabled := False;
    cbTargetRole.Enabled  := False;
    cbTargetTrace.Enabled  := True;
    cbTargetLocation.Enabled  := False;
  end;

  if cbDefaultKinds.ItemIndex = 2   then // location_at
  begin
    cbSourceIdentity.Enabled := False;
    cbSourceRole.Enabled := False;
    cbSourceTrace.Enabled := True;
    cbTargetRole.Enabled  := False;
    cbTargetTrace.Enabled  := False;
    cbTargetLocation.Enabled  := True;
  end;

  if cbDefaultKinds.ItemIndex = 3   then // contained_within
  begin
    cbSourceIdentity.Enabled := False;
    cbSourceRole.Enabled := False;
    cbSourceTrace.Enabled := True;
    cbTargetRole.Enabled  := False;
    cbTargetTrace.Enabled  := True;
    cbTargetLocation.Enabled  := False;
  end;

  if cbDefaultKinds.ItemIndex = 4   then // has_account
  begin
    cbSourceIdentity.Enabled := True;
    cbSourceRole.Enabled := False;
    cbSourceTrace.Enabled := False;
    cbTargetRole.Enabled  := True;
    cbTargetTrace.Enabled  := True;
    cbTargetLocation.Enabled  := False;
  end;

  if cbDefaultKinds.ItemIndex > -1 then
    edNameRelationship.Text := cbDefaultKinds.Items[cbDefaultKinds.ItemIndex];
end;

procedure TformRelationship.cbSourceIdentityChange(Sender: TObject);
var
  line: string;
begin
  if cbSourceIdentity.ItemIndex > -1 then
  begin
    line := cbSourceIdentity.Items[cbSourceIdentity.ItemIndex];
    line := Copy(line, Pos('@', line) +1, Length(line));
    edSourceID.Text :=  line;
  end;

end;

procedure TformRelationship.cbSourceRoleChange(Sender: TObject);
var
  line: String;
begin
  if cbSourceRole.ItemIndex > -1 then
  begin
    line := cbSourceRole.Items[cbSourceRole.ItemIndex];
    line := Copy(line, Pos('@', line) +1, Length(line));
    edSourceID.Text :=  line;
  end;
end;

procedure TformRelationship.cbSourceTraceChange(Sender: TObject);
var
  line: String;
begin
  if cbSourceTrace.ItemIndex > - 1 then
  begin
    line := cbSourceTrace.Items[cbSourceTrace.ItemIndex];
    line := Copy(line, Pos('@', line) +1, Length(line));
    edSourceID.Text :=  line;
  end;
end;

procedure TformRelationship.cbTargetLocationChange(Sender: TObject);
var
  line: String;
begin
  if cbTargetLocation.ItemIndex >-1 then
  begin
    line := cbTargetLocation.Items[cbTargetLocation.ItemIndex];
    line := Copy(line, Pos('@', line) +1, Length(line));
    edTargetID.Text :=  line;
  end;
end;

procedure TformRelationship.cbtargetRoleChange(Sender: TObject);
var
  line: String;
begin
  if cbTargetRole.ItemIndex > -1 then
  begin
    edTargetID.Text := '';
    line := cbTargetRole.Items[cbTargetRole.ItemIndex];
    line := Copy(line, Pos('@', line) + 1, Length(line));
    edTargetID.Text :=  line;
  end;
end;

procedure TformRelationship.cbTargetTraceChange(Sender: TObject);
var
  line: String;
begin
  if cbTargetTrace.ItemIndex > -1 then
  begin
    line := cbTargetTrace.Items[cbTargetTrace.ItemIndex];
    line := Copy(line, Pos('@', line) +1, Length(line));
    edTargetID.Text :=  line;
  end;
end;

procedure TformRelationship.FormShow(Sender: TObject);
begin
  cbSourceIdentity.Items.Clear;
  cbSourceRole.Items.Clear;
  cbSourceTrace.Items.Clear;
  cbTargetRole.Items.Clear;
  cbTargetLocation.Items.Clear;
  cbTargetTrace.Items.Clear;
  readIdentityFromFile;
  readRoleFromFile;
  readAppAccountFromFile;
  readLocationFromFile;
  readTraceFromFile;
  cbSourceIdentity.Enabled := False;
  cbSourceRole.Enabled := False;
  cbSourceTrace.Enabled := False;
  cbTargetRole.Enabled  := False;
  cbTargetTrace.Enabled  := False;
  cbTargetLocation.Enabled  := False;
end;

function TformRelationship.JsonTokenToString(const t: TJsonToken): string;
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

procedure TformRelationship.lbRelationshipChange(Sender: TObject);
var
  line, propertyValue: String;
  idx: Integer;
begin
  if lbRelationship.ItemIndex > - 1 then
  begin
    line := lbRelationship.Items[lbRelationship.ItemIndex];
    edNameRelationShip.Text := ExtractField(line, '"uco-observable:kindOfRelationship":"');
    for idx:=0 to cbDefaultKinds.Items.Count - 1 do
    begin
      if cbDefaultKinds.Items[idx] =  edNameRelationShip.Text then
      begin
        cbDefaultKinds.ItemIndex := idx;
        break;
      end;
    end;
    propertyValue := ExtractField(line, '"uco-observable:isDirectional":"');
    for idx:=0 to cbDirectional.Items.Count - 1 do
    begin
      if cbDirectional.Items[idx] =  propertyValue then
      begin
        cbDirectional.ItemIndex := idx;
        break;
      end;
    end;
    edSourceID.Text := ExtractField(line, '"uco-observable:source":"');

    // to be optimized: when a cb Sourcexxx is enable the other two must not be
    // processed
    if cbSourceIdentity.Enabled then
    begin
      for idx:=0 to cbSourceIdentity.Items.Count - 1 do
      begin
        if AnsiContainsStr(cbSourceIdentity.Items[idx], edSourceID.Text) then
        begin
          cbSourceIdentity.ItemIndex := idx;
          break;
        end;
      end;
    end;

    if cbSourceRole.Enabled then
    begin
      for idx:=0 to cbSourceRole.Items.Count - 1 do
      begin
        if AnsiContainsStr(cbSourceRole.Items[idx], edSourceID.Text) then
        begin
          cbSourceRole.ItemIndex := idx;
          break;
        end;
      end;
    end;

    if cbSourceTrace.Enabled then
    begin
      for idx:=0 to cbSourceTrace.Items.Count - 1 do
      begin
        if AnsiContainsStr(cbSourceTrace.Items[idx], edSourceID.Text) then
        begin
          cbSourceTrace.ItemIndex := idx;
          break;
        end;
      end;
    end;

    edTargetID.Text := ExtractField(line, '"uco-observable:target":"');

    // to be optimized: when a cb Sourcexxx is enable the other two must not be
    // processed
    if cbTargetRole.Enabled then
    begin
      for idx:=0 to cbTargetRole.Items.Count - 1 do
      begin
        if AnsiContainsStr(cbTargetRole.Items[idx], edTargetID.Text) then
        begin
          cbTargetRole.ItemIndex := idx;
          break;
        end;
      end;
    end;

    if cbTargetLocation.Enabled then
    begin
      for idx:=0 to cbTargetLocation.Items.Count - 1 do
      begin
        if AnsiContainsStr(cbTargetLocation.Items[idx], edTargetID.Text) then
        begin
          cbTargetLocation.ItemIndex := idx;
          break;
        end;
      end;
    end;

    if cbTargetTrace.Enabled then
    begin
      for idx:=0 to cbTargetTrace.Items.Count - 1 do
      begin
        if AnsiContainsStr(cbTargetTrace.Items[idx], edTargetID.Text) then
        begin
          cbTargetTrace.ItemIndex := idx;
          break;
        end;
      end;
    end;

  end;

end;

function TformRelationship.prepareObjectCaseLine(operation: String): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30;
  indent := '   ';

  line := '{' + recSep;

  if operation = 'add' then
  begin
    CreateGUID(Uid);
    guidNoBraces := ':' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
  end
  else
    guidNoBraces :=  ExtractField(lbRelationship.Items[lbRelationship.ItemIndex], '"@id":"');

  line := line + indent + '"@id":"' + guidNoBraces + '",' + recSep;
  line := line + indent + '"@type":"uco-observable:Relationship",' + recSep;
  line := line + indent + '"uco-observable:source":"' + edSourceID.Text + '",' + recSep;
  line := line + indent + '"uco-observable:target":"' + edTargetID.Text + '",' + recSep;
  line := line + indent + '"uco-observable:kindOfRelationship":"' + edNameRelationship.Text + '",' + recSep;
  line := line + indent + '"uco-observable:isDirectional":"' + cbDirectional.Items[cbDirectional.ItemIndex]+ '"' + recSep;
  line := line + '}';
  Result := line;
end;



procedure TformRelationship.readAppAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inAccountIssuer, inAccountID, inID: Boolean;
  id, accountIssuer, accountID: string;
  listAccount: TStringList;
  idx:integer;
begin
  recSep := #30 + #30;
  crlf := #13 + #10;
  (* read file JSON uuidCase-identity.json: fill in cbSourceIdentity component *)
  if FileExists(FpathCase + FuuidCase + '-traceAPP_ACCOUNT.json') then
  begin
    listAccount := TStringList.Create;
    listAccount.LoadFromFile(FpathCase + FuuidCase + '-traceAPP_ACCOUNT.json');
    //JSON string here
    json := stringreplace(listAccount.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);
      inAccountIssuer := False;
      inAccountID := False;
      inID := False;
      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'uco-observable:accountIssuer' then
            inAccountIssuer := True
          else
            inAccountIssuer := False;

          if jreader.Value.AsString = 'uco-observable:applicationIdentifier' then
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
          if inAccountID then begin
            accountID := jreader.Value.AsString;
            cbTargetRole.Items.Add(accountIssuer + ' ' + accountID + ' ' + '@' + id);
            accountIssuer := '';
            accountID := '';
            id := '';
          end;

          if inID then
            id := jreader.Value.AsString;

          if inAccountIssuer then
            accountIssuer := jreader.Value.AsString;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformRelationship.readIdentityFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inFamilyName, inID: Boolean;
  id, name, familyName: string;
  listIdentity: TStringList;
  idx, nHypens:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-identity.json') then
  begin
    listIdentity := TStringList.Create;
    listIdentity.LoadFromFile(FpathCase + FuuidCase + '-identity.json');
    //JSON string here
    json := stringreplace(listIdentity.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);
      inName := False;
      inFamilyName := False;
      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin

        //locality := jreader.ReadAsString;
          if jreader.Value.AsString = 'uco-identity:givenName' then
            inName := True
          else
            inName := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-identity:familyName' then
            inFamilyName := True
          else
            inFamilyName := False;

          //ShowMessage('locality:' + locality + ', street:' + street);
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inName then
            name := jreader.Value.AsString;

          if inID then
            id := jreader.Value.AsString;;

          if inFamilyName then
          begin
            familyName := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            cbSourceIdentity.Items.Add(name + ' ' + familyName + '@' + id);
            name := '';
            id:= '';
          end;
        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformRelationship.readLocationFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inID, inLocality, inRegion, inStreet: Boolean;
  id, locality, region, street: string;
  listLocation: TStringList;
  idx, nHypens:integer;
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
          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-location:locality' then
            inLocality := True
          else
            inLocality := False;

          if jreader.Value.AsString = 'uco-location:region' then
            inRegion := True
          else
            inRegion := False;

          if jreader.Value.AsString = 'uco-location:street' then
            inStreet := True
          else
            inStreet := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin

          if inID then
            id := jreader.Value.AsString;

          if inLocality then
            locality := jreader.Value.AsString;

          if inRegion then
            region := jreader.Value.AsString;

          if inStreet then
          begin
            street := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);
            cbTargetLocation.Items.Add(street + ' ' + locality + ' ' + region + '@' + id);
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

procedure TformRelationship.readRoleFromFile;
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
          if jreader.Value.AsString = 'uco-role:name' then
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
            cbSourceRole.Items.Add(name + ' ' + '@' + id);
            cbTargetRole.Items.Add(name + ' ' + '@' + id);
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

procedure TformRelationship.readTraceComputerFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inManufacturer, inModel: Boolean;
  id, cManufacturer, cModel: string;
  listTrace: TStringList;
  idx, nHypens:integer;
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
            cManufacturer := jreader.Value.AsString;

          if inModel then
          begin
            cModel := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            cbSourceTrace.Items.Add('Computer ' + cManufacturer + ' ' + cModel + '@' + id);
            cbTargetTrace.Items.Add('Computer ' + cManufacturer + ' ' + cModel + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformRelationship.readTraceDiskFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inModel, inManufacturer, inSN, inCapacity: Boolean;
  id, dManufacturer, dModel, dSN, dCapacity: string;
  listTrace: TStringList;
  idx, nHypens:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-traceDISK.json') then
  begin
    listTrace := TStringList.Create;
    listTrace.LoadFromFile(FpathCase + FuuidCase + '-traceDISK.json');
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
            inModel := False;

          if jreader.Value.AsString = 'uco-observable:model' then
            inModel := True
          else
            inModel := False;

        if jreader.Value.AsString = 'uco-observable:capacity' then
            inCapacity := True
          else
            inCapacity := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-observable:serialNumber' then
            inSN := True
          else
            inSN := False;

        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inManufacturer then
            dManufacturer := jreader.Value.AsString;

          if inModel then
            dModel := jreader.Value.AsString;

          if inCapacity then
          begin
            dCapacity := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            cbSourceTrace.Items.Add('Hard Disk ' + dManufacturer + ' ' + dModel + ' ' + dSN + '@' + id);
            cbTargetTrace.Items.Add('Hard Disk ' + dManufacturer + ' ' + dModel + ' ' + dSN + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformRelationship.readTraceDiskPartitionFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inPartitionType, inPartitionID, inPartitionSize: Boolean;
  id, pType, pID, pSize: string;
  listTrace: TStringList;
  idx, nHypens:integer;
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
            inPartitionType := True
          else
            inPartitionType := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-observable:partitionID' then
            inPartitionID := True
          else
            inPartitionID := False;

          if jreader.Value.AsString = 'uco-observable:partitionLength' then
            inPartitionSize := True
          else
            inPartitionSize := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inPartitionType then
            pType := jreader.Value.AsString;

          if inPartitionID then
            pID := jreader.Value.AsString;

          if inPartitionSize then
          begin
            pSize := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            cbSourceTrace.Items.Add('Disk Partition ' + pType + ' ' + pID + ' ' + pSize + '@' + id);
            cbTargetTrace.Items.Add('Disk Partition ' + pType + ' ' + pID + ' ' + pSize + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformRelationship.readTraceEmailAccountFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inEmailAddress, inID: Boolean;
  id, emailAddress: string;
  listTrace: TStringList;
  idx, nHypens:integer;
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
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            cbTargetTrace.Items.Add(emailAddress + ' '  + '@' + id);
            cbSourceTrace.Items.Add(emailAddress + ' '  + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformRelationship.readTraceFileFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inFileName, inID: Boolean;
  id, fileName: string;
  listTrace: TStringList;
  idx, nHypens:integer;
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
            inFileName := True
          else
            inFileName := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then
            id := jreader.Value.AsString;

          if inFileName then
          begin
            fileName := jreader.Value.AsString;
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            cbSourceTrace.Items.Add(fileName + ' '  + '@' + id);
            cbTargetTrace.Items.Add(fileName + ' '  + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformRelationship.readTraceFromFile;

begin
  readTraceMobileFromFile;
  readTraceComputerFromFile;
  readTraceDiskFromFile;
  readTraceDiskPartitionFromFile;
  readTraceEmailAccountFromFile;
  readTraceFileFromFile;
  readTraceMessageFromFile;
  readTraceSIMFromFile;
end;

procedure TformRelationship.readTraceMessageFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inApplication, inMessageText: Boolean;
  id, application, messageText: string;
  listTrace: TStringList;
  idx, nHypens:integer;
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
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            cbTargetTrace.Items.Add(application + ' ' + messageText + ' ' + '@' + id);
            cbSourceTrace.Items.Add(application + ' ' + messageText + ' ' + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

end;

procedure TformRelationship.readTraceMobileFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inManufacturer, inModel, inSerial: Boolean;
  id, manufacturer, model, serial: string;
  listTrace: TStringList;
  idx, nHypens:integer;
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
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            cbSourceTrace.Items.Add(manufacturer + ' ' + model + ' ' + serial + '@' + id);
            cbTargetTrace.Items.Add(manufacturer + ' ' + model + ' ' + serial + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;
end;

procedure TformRelationship.readTraceSIMFromFile;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inID, inSimType, inICCID: Boolean;
  id, simType, ICCID: string;
  listTrace: TStringList;
  idx, nHypens:integer;
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
          if jreader.Value.AsString = 'uco-observable:SIMType' then
            inSimType := True
          else
            inSimType := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;

          if jreader.Value.AsString = 'uco-observable:ICCID' then
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
            nHypens := CountOccurrences('-', id);
            (*--- if nHypens > 4 then it is the case of id related to @type inside an Object,
                  for instance for Identity it can be @id:"...-...-SimpleName" ---*)
            if nHypens > 4  then
              id := Copy(id, 1, LastDelimiter('-', id) - 1);

            cbSourceTrace.Items.Add(simType + ' ' + ICCID + '@' + id);
            cbTargetTrace.Items.Add(simType + ' ' + ICCID + '@' + id);
          end;

        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;
end;

procedure TformRelationship.btnCancelClick(Sender: TObject);
begin
  formRelationship.Close;
end;

procedure TformRelationship.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line:string;
  idx: integer;
begin
  if lbRelationship.Items.Count > 0 then
  begin
    //dir := GetCurrentDir;
    idx := 0;
    AssignFile(fileJSON, FpathCase + FuuidCase + '-relationship.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_RELATIONSHIP":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbRelationship.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbRelationship.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbRelationship.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-relationship.json');

  formRelationship.Close;
end;

procedure TformRelationship.btnAddRelationshipClick(Sender: TObject);
begin
  if (edSourceID.Text = '') or (edTargetID.Text = '')  then
    ShowMessage('ID Source and/or Target are missing!')
  else
  begin
    lbRelationship.Items.Add(prepareObjectCaseLine
    ('add'));
    edNameRelationship.Text := '';
    edSourceID.Text := '';
    edTargetID.Text := '';
    cbSourceIdentity.Enabled := False;
    cbSourceIdentity.ItemIndex := -1;
    cbSourceRole.Enabled := False;
    cbSourceRole.ItemIndex := -1;
    cbSourceTrace.Enabled := False;
    cbSourceTrace.ItemIndex := -1;
    cbTargetRole.Enabled := False;
    cbTargetRole.ItemIndex := -1;
    cbTargetLocation.Enabled := False;
    cbTargetLocation.ItemIndex := -1;
    cbTargetTrace.Enabled := False;
    cbTargetTrace.ItemIndex := -1;
    cbDefaultKinds.ItemIndex := -1;
  end;
end;

procedure TformRelationship.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformRelationship.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformRelationship.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine: String;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-relationship.json
  if FileExists(FpathCase + FuuidCase + '-relationship.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-relationship.json');
    Reset(fileJSON);
    lbRelationship.Items.Clear;
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

        lbRelationship.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;

  formRelationship.ShowModal;
end;

end.
