unit caseGenerator_warrant;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers;

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
    procedure btnAddWarrantClick(Sender: TObject);
    procedure btnRemoveWarrantClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    procedure readRoleFromFile;
    function extractID(line: String): String;
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
begin
  cbYear.Items.Clear;
  for idx:=2000 to 2020 do
    cbYear.Items.Add(IntToStr(idx));
{
  - Judicial Authorities values/ID are taken from Role with "name":"judge"
  - Applicant values/ID are taken from Role with "name":"investogator"
}
  readRoleFromFile;
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


procedure TformWarrant.readRoleFromFile;
var
  json, dir, recSep, crlf: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inID: Boolean;
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
            if (name = 'Judge') then
              cbAuthority.Items.Add(name + ' ' + '@' + id);
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
  end;

  formWarrant.Close;
end;

procedure TformWarrant.btnAddWarrantClick(Sender: TObject);
var
  line, recSep, lineID: string;
  Uid: TGUID;
begin
  if (cbAuthority.Items.Count = 0) or (edAuthorizationType.Text = '')  then
    ShowMessage('Authority and/or AuthorizationType are empty')
  else
  begin
    CreateGUID(Uid);
    recSep := #30 + #30;
    line := '{"@id":"' + GuidToString(Uid) + '",' + recSep;
    line := line + '"@type":"Authorization",' + recSep;
    line := line + '"propertyBundle":[' + recSep;
    line := line + '{"@authorizationType":"' + edAuthorizationType.Text + '",' + recSep;
    line := line + '"authorizationIdentifier":"' + edIdentifier.Text + '",' + recSep;
    lineID := extractID(cbAuthority.Items[cbAuthority.ItemIndex]);
    line := line + '"authorizationAuthority":"' + lineID + '",' + recSep;
    line := line + '"authorizationIssuedDate":"' + cbYear.Items[cbYear.ItemIndex];
    line := line + cbMonth.Items[cbMonth.ItemIndex];
    line := line + cbDay.Items[cbDay.ItemIndex] + '"}' + recSep;
    line := line + ']}' + recSep;
    lbWarrant.Items.Add(line);
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
