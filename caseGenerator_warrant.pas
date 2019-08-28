unit caseGenerator_warrant;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers, caseGenerator_util;

type
  TformWarrant = class(TForm)
    Label1: TLabel;
    lbWarrant: TListBox;
    btnClose: TButton;
    btnAddWarrant: TButton;
    btnRemoveWarrant: TButton;
    panelWarrant: TPanel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    cbAuthority: TComboBox;
    edAuthorizationType: TEdit;
    Label2: TLabel;
    cbDay: TComboBox;
    cbMonth: TComboBox;
    cbYear: TComboBox;
    Label3: TLabel;
    edIdentifier: TEdit;
    btnModify: TButton;
    btnCancel: TButton;
    procedure btnAddWarrantClick(Sender: TObject);
    procedure btnRemoveWarrantClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbWarrantChange(Sender: TObject);
    procedure btnModifyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    function readIdRoleFromFile: TStringList;
    function readIdentityFromRelationshipFile(idRoles: TStringList): TStringList;
    function extractID(line: String): String;
    function prepareItemWarrant(operation: String): String;
    procedure readIdentityFromFile(idValues: TStringList);
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formWarrant: TformWarrant;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformWarrant.btnRemoveWarrantClick(Sender: TObject);
begin
  lbWarrant.Items.Delete(lbWarrant.ItemIndex);
end;


function TformWarrant.extractID(line: String): String;
begin
  Result := Copy(line, Pos('@', line) + 1, Length(line));
end;

procedure TformWarrant.FormShow(Sender: TObject);
var
  idx: Integer;
  idValues: TStringList;
begin
  cbYear.Items.Clear;
  for idx:=2000 to 2020 do
    cbYear.Items.Add(IntToStr(idx));
{
  - the cbAuthority values must be taken combining Role and Relationship:
    1.  extracting of the idRoles with name="Judge" or "Judicial Authority"
    2.  extracting idIdentities as value of the "source" property where the above
        idRoles[idx] appears in the "target" property
    3.  extracting name and familyName of the idIdentities[idx] above identified
}
    idValues := TStringList.Create;
    // idRole Judge/Authority
    idValues := readIdRoleFromFile;
    //idIdentities of Judge/Authority
    idValues := readIdentityFromRelationshipFile(idValues);
    readIdentityFromFile(idValues);


end;

function TformWarrant.JsonTokenToString(const t: TJsonToken): string;
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


procedure TformWarrant.lbWarrantChange(Sender: TObject);
var
  line, issuedDate, sDate, sDay, sMonth, sYear: String;
  idx: Integer;
begin
  if lbWarrant.ItemIndex > - 1 then
  begin
    line := lbWarrant.Items[lbWarrant.ItemIndex];
    edAuthorizationType.Text := ExtractField(line, '"@authorizationType":"');
    edIdentifier.Text := ExtractField(line, '"authorizationIdentifier":"');
    issuedDate := ExtractField(line, '"authorizationIssuedDate":"');
    sDate := Copy(issuedDate, 1, 10);
    sDay := Copy(sDate, 7, 2);
    for idx:=0 to cbDay.Items.Count - 1 do
    begin
      if cbDay.Items[idx] = sDay then
      begin
        cbDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 5, 2);
    for idx:=0 to cbMonth.Items.Count - 1 do
    begin
      if cbMonth.Items[idx] = sMonth then
      begin
        cbMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbYear.Items.Count - 1 do
    begin
      if cbYear.Items[idx] = sYear then
      begin
        cbYear.ItemIndex := idx;
        break;
      end;
    end;

    line := ExtractField(line, '"authorizationAuthority":"');
    for idx:=0 to cbAuthority.Items.Count - 1 do
    begin
      if AnsiContainsStr(cbAuthority.Items[idx],  line) then
      begin
        cbAuthority.ItemIndex := idx;
        break;
      end;
    end;

  end;
end;

function TformWarrant.prepareItemWarrant(operation: String): String;
var
  line, recSep, lineID, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30;
  indent := '   ';

  line := '{' + recSep;
  if operation = 'add' then
  begin
    CreateGUID(Uid);
    guidNoBraces := Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
  end
  else
    guidNoBraces :=  ExtractField(lbWarrant.Items[lbWarrant.ItemIndex], '"@id":"');

  line := line + indent + '"@id":"' + guidNoBraces + '",' + recSep;
  line := line + indent + '"@type":"Authorization",' + recSep;
  line := line + indent + '"propertyBundle":[' + recSep;
  line := line + indent + '{' + recSep;
  line := line + RepeatString(indent, 2) + '"@type":"' + guidNoBraces + '-authorizationType",' + recSep;
  line := line + RepeatString(indent, 2) + '"@authorizationType":"' + edAuthorizationType.Text + '",' + recSep;
  line := line + RepeatString(indent, 2) + '"authorizationIdentifier":"' + edIdentifier.Text + '",' + recSep;
  lineID := extractID(cbAuthority.Items[cbAuthority.ItemIndex]);
  line := line + RepeatString(indent, 2) + '"authorizationAuthority":"' + lineID + '",' + recSep;
  line := line + RepeatString(indent, 2) + '"authorizationIssuedDate":"' + cbYear.Items[cbYear.ItemIndex];
  line := line + cbMonth.Items[cbMonth.ItemIndex];
  line := line + cbDay.Items[cbDay.ItemIndex] + '"' + recSep;
  line := line +   indent + '}' + recSep;
  line := line + indent + ']' + recSep + '}' + recSep;
  Result := line;
end;

procedure TformWarrant.readIdentityFromFile(idValues: TStringList);
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inFamilyName, inID: Boolean;
  name, familyName, idIdentity: string;
  idIdentities: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-identity.json') then
  begin
    idIdentities := TStringList.Create;
    idIdentities.LoadFromFile(FpathCase + FuuidCase + '-identity.json');
    //JSON string here
    json := stringreplace(idIdentities.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);
      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'givenName' then
            inName := True
          else
            inName := False;

          if jreader.Value.AsString = 'familyName' then
            inFamilyName := True
          else
            inFamilyName := False;

          if jreader.Value.AsString = '@id' then
            inID := True
          else
            inID := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inID then begin
            idIdentity := jreader.Value.AsString;
          end;

          if inName then
            name := jreader.Value.AsString;

          if inFamilyName then
          begin
            familyName := jreader.Value.AsString;
            for idx:=0 to idValues.Count - 1 do
              if idValues[idx] = idIdentity then
                cbAuthority.Items.Add(name + ' ' + familyName + '@' + idIdentity)
          end;
        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;
end;

function TformWarrant.readIdentityFromRelationshipFile(idRoles: TStringList): TStringList;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inSource, inTarget: Boolean;
  source, target: string;
  listRelationship, idIdentities: TStringList;
  idx, nHypens:integer;
begin
  //dir := GetCurrentDir;
  recSep := #30 + #30;
  crlf := #13 + #10;
  idIdentities := TStringList.Create;
  // read file JSON uuidCase-identity.json: fill in cbSourceIdentity component
  if FileExists(FpathCase + FuuidCase + '-relationship.json') then
  begin
    listRelationship := TStringList.Create;
    listRelationship.LoadFromFile(FpathCase + FuuidCase + '-relationship.json');
    //JSON string here
    json := stringreplace(listRelationship.Text, recSep, crlf,[rfReplaceAll]);
    try
      sreader := TStringReader.Create(json);
      jreader := TJsonTextReader.Create(sreader);
      inSource := False;
      while jreader.Read do
      begin
        if JsonTokenToString(jreader.TokenType) = 'PropertyName' then
        begin
          if jreader.Value.AsString = 'source' then
            inSource := True
          else
            inSource := False;

          if jreader.Value.AsString = 'target' then
            inTarget := True
          else
            inTarget := False;
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inTarget then begin
            target := jreader.Value.AsString;

            for idx:=0 to idRoles.Count - 1 do
              if idRoles[idx] =  target then
                idIdentities.Add(source);
          end;

          if inSource then
            source := jreader.Value.AsString;;
        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;

  Result := idIdentities;

end;

function TformWarrant.readIdRoleFromFile: TSTringList;
var
  json, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inID: Boolean;
  id, name: string;
  listRole, idRoles: TStringList;
  idx:integer;
begin
  //dir := GetCurrentDir;
  idRoles := TStringList.Create;
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
            if (name = 'Judge') or (name='Judicial Authority') then
              //cbAuthority.Items.Add(name + ' ' + '@' + id);
              idRoles.Add(id);
            //if (name = 'Investigator') then
                //cbApplicant.Items.Add(name + ' ' + '@' + id);
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
  Result := idRoles;
end;


procedure TformWarrant.btnCancelClick(Sender: TObject);
begin
  formWarrant.Close;
end;

procedure TformWarrant.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line: String;
  idx: Integer;
begin
  if lbWarrant.Items.Count > 0 then
  begin
    //dir := GetCurrentDir;
    idx := 0;
    AssignFile(fileJSON, FpathCase + FuuidCase + '-warrant.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_WARRANT":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbWarrant.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbWarrant.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbWarrant.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-warrant.json');

  formWarrant.Close;
end;

procedure TformWarrant.btnModifyClick(Sender: TObject);
begin
  if lbWarrant.ItemIndex > - 1 then
    lbWarrant.Items[lbWarrant.ItemIndex] := prepareItemWarrant('modify');
end;

procedure TformWarrant.btnAddWarrantClick(Sender: TObject);
begin
  if (cbAuthority.Items.Count = 0) or (edAuthorizationType.Text = '')  then
    ShowMessage('Authority and/or AuthorizationType are empty')
  else
  begin
    lbWarrant.Items.Add(prepareItemWarrant('add'));
    cbAuthority.ItemIndex := -1;
    cbDay.ItemIndex := -1;
    cbMonth.ItemIndex := -1;
    cbYear.ItemIndex := -1;
  end;
end;


procedure TformWarrant.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformWarrant.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformWarrant.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine: String;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-warrant.json
  if FileExists(FpathCase + FuuidCase + '-warrant.json') then
  begin
    AssignFile(fileJSON, Fpathcase + FuuidCase + '-warrant.json');
    Reset(fileJSON);
    lbWarrant.Items.Clear;
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

        lbWarrant.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formWarrant.ShowModal;
end;

end.
