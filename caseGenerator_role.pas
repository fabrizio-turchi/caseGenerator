unit caseGenerator_role;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, System.JSON, System.JSON.Types,
  System.JSON.Readers, caseGenerator_util;

type
  TformRole = class(TForm)
    Label1: TLabel;
    lbRole: TListBox;
    edName: TEdit;
    Label2: TLabel;
    btnClose: TButton;
    btnAddRole: TButton;
    btnDeleteRole: TButton;
    Label3: TLabel;
    cbDefaultNames: TComboBox;
    btnModifyRole: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnAddRoleClick(Sender: TObject);
    procedure btnDeleteRoleClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cbDefaultNamesChange(Sender: TObject);
    procedure lbRoleChange(Sender: TObject);
    procedure btnModifyRoleClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    function prepareObjectCaseLine(operation: String): String;
    //function ExtractField(line: String; subLine: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formRole: TformRole;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformRole.btnDeleteRoleClick(Sender: TObject);
begin
  lbRole.Items.Delete(lbRole.ItemIndex);
end;

procedure TformRole.btnModifyRoleClick(Sender: TObject);
begin
  if lbRole.ItemIndex > -1 then
    lbRole.Items[lbRole.ItemIndex] := prepareObjectCaseLine('modify')
  else
    ShowMessage('No item selected in the Role list!');
end;

procedure TformRole.cbDefaultNamesChange(Sender: TObject);
begin
  if cbDefaultNames.ItemIndex > -1 then
    edName.Text := cbDefaultNames.Items[cbDefaultNames.ItemIndex];
end;

procedure TformRole.btnCancelClick(Sender: TObject);
begin
  formRole.Close;
end;

procedure TformRole.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line :string;
  idx: integer;
begin
  if lbRole.Items.Count > 0 then
  begin
    //dir := GetCurrentDir;
    AssignFile(fileJSON, FpathCase + FuuidCase + '-role.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_ROLE":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbRole.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbRole.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbRole.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-role.json');

  formRole.Close;
end;

procedure TformRole.btnAddRoleClick(Sender: TObject);

begin

  if Trim(edName.Text) = ''  then
    ShowMessage('Role name is missing!')
  else
  begin
    lbRole.Items.Add(prepareObjectCaseLine('add'));
    edName.Text := '';
    cbDefaultNames.ItemIndex := -1;
  end;
end;

procedure TformRole.FormShow(Sender: TObject);
{var
  json, dir: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inName, inFamilyName: Boolean;
  name, familyName: string;
  listIdentity: TStringList;
  idx:integer; }
begin
  {dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(dir + '\' + FuuidCase + '-identity.json') then
  begin
    cbIdentity.Items.Clear;
    listIdentity := TStringList.Create;
    listIdentity.LoadFromFile(dir + '\' + FuuidCase + '-identity.json');
    //json := lbJSON.Items.Text;
    json := listIdentity.Text; // <-- your JSON string here
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
          if jreader.Value.AsString = 'name' then
            inName := True
          else
            inName := False;

          if jreader.Value.AsString = 'familyName' then
            inFamilyName := True
          else
            inFamilyName := False;

          //ShowMessage('locality:' + locality + ', street:' + street);
        end;
        if JsonTokenToString(jreader.TokenType) = 'String' then
        begin
          if inName then
            name := jreader.Value.AsString;

          if inFamilyName then
          begin
            familyName := jreader.Value.AsString;
            idx := cbIdentity.Items.Add(name + ' ' + familyName);
          end;
          //ShowMessage('locality:' + locality + ', street:' + street);
        end;
      end;
    finally
      jreader.Free;
      sreader.Free;
    end;
  end;  }
end;



function TformRole.JsonTokenToString(const t: TJsonToken): string;
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


procedure TformRole.lbRoleChange(Sender: TObject);
var
  line: String;
  idx: Integer;
begin
 if lbRole.ItemIndex > - 1 then
  begin
    line := lbRole.Items[lbRole.ItemIndex];
    edName.Text := ExtractField(line, '"name":"');
    for idx:=0 to cbDefaultNames.Items.Count - 1 do
    begin
      if cbDefaultNames.Items[idx] =  edName.Text then
      begin
        cbDefaultNames.ItemIndex := idx;
        break;
      end;
    end;
  end;

end;

function TformRole.prepareObjectCaseLine(operation: String): String;
var
  line, recSep: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30;

  if operation = 'add' then
  begin
    CreateGUID(Uid);
    line := '{"@id":"' +  GuidToString(Uid) + '", ' + recSep;
  end
  else
  begin
    idx := lbRole.ItemIndex;
    line := '{"@id":"' + ExtractField(lbRole.Items[idx], '"@id":"') + '",'+ recSep;
  end;

  line := line + #9 + '"@type":"Role",' + recSep;
  line := line + #9 + '"name":"' + edName.Text + '"' + recSep;
  line := line + '}';
end;

procedure TformRole.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformRole.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformRole.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine, dir:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-role.json
  if FileExists(FpathCase + FuuidCase + '-role.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-role.json');
    Reset(fileJSON);
    lbRole.Items.Clear;
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

        lbRole.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formRole.ShowModal;
end;

end.
