unit caseGenerator_tool;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.JSON.Readers, System.JSON.Types, System.JSON, caseGenerator_util;

type
  TformTool = class(TForm)
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    btnClose: TButton;
    btnAddTool: TButton;
    btnDeleteTool: TButton;
    cbToolType: TComboBox;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    panelConfigurationSetting: TPanel;
    edItemName: TEdit;
    edItemValue: TEdit;
    Label5: TLabel;
    edCreator: TEdit;
    edVersion: TEdit;
    Label8: TLabel;
    lbConfigurationSettingTool: TListBox;
    Label9: TLabel;
    btnAddItem: TButton;
    lbTool: TListBox;
    btnModifyItem: TButton;
    btnDeleteItem: TButton;
    procedure btnAddToolClick(Sender: TObject);
    procedure btnDeleteToolClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnAddItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbToolChange(Sender: TObject);
    procedure btnDeleteItemClick(Sender: TObject);
    procedure btnModifyItemClick(Sender: TObject);
    procedure lbConfigurationSettingToolChange(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    function prepareItemSettingTool: String;
    //function ExtractField(line, subLine: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    procedure ShowWithParamaterOld(uuidCase: String);
    { Public declarations }
  end;

var
  formTool: TformTool;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTool.btnDeleteItemClick(Sender: TObject);
begin
  if lbConfigurationSettingTool.ItemIndex > -1 then
    lbConfigurationSettingTool.Items.Delete(lbConfigurationSettingTool.ItemIndex);

end;

procedure TformTool.btnDeleteToolClick(Sender: TObject);
begin
  lbTool.Items.Delete(lbTool.ItemIndex);
end;


procedure TformTool.btnModifyItemClick(Sender: TObject);
begin
  if lbConfigurationSettingTool.ItemIndex > - 1 then
    lbConfigurationSettingTool.Items.Add(prepareItemSettingTool());
end;

procedure TformTool.FormShow(Sender: TObject);
begin
  //SendMessage(lbTool.Handle, LB_SETHORIZONTALEXTENT, 1000, 0);
end;

function TformTool.JsonTokenToString(const t: TJsonToken): string;
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

procedure TformTool.lbConfigurationSettingToolChange(Sender: TObject);
var
line: String;
begin
  line := lbConfigurationSettingTool.Items[lbConfigurationSettingTool.ItemIndex];

  edItemName.Text := ExtractField(line, '"itemName":"');
  edItemValue.Text := ExtractField(line, '"itemValue":"');

end;

procedure TformTool.lbToolChange(Sender: TObject);
var
  line, cbValue, itemName, itemValue, itemTool, recSep: String;
  idx: Integer;
  itemFound: Boolean;
begin

  if lbTool.ItemIndex > -1 then
  begin
    line := lbTool.Items[lbTool.ItemIndex];
    edName.Text := ExtractField(line, '"name":"');
    cbValue  := ExtractField(line, '"toolType":"');
    for idx := 0 to cbToolType.Count-1 do
    begin
      if AnsiContainsStr(cbToolType.Items[idx], cbValue) then
      begin
        cbToolType.ItemIndex := idx;
        break;
      end;
    end;
    edCreator.Text := ExtractField(line, '"creator":"');
    edVersion.Text := ExtractField(line, '"version":"');

    lbConfigurationSettingTool.Items.Clear;
    itemFound :=  AnsiContainsStr(line, '"itemName":"');
    recSep := #30 + #30;
    while itemFound do
    begin
      itemName := ExtractField(line, '"itemName":"');
      itemValue := ExtractField(line, '"itemValue":"');
      itemTool := '{' + recSep + #9 + '"@type":"ConfigurationSetting", ' + #9;
      itemTool := itemTool + '"itemName":"' + itemName + '",' + recSep;
      itemTool := itemTool + '"itemValue":"' + itemValue + '"' + #9 + '}';
      lbConfigurationSettingTool.Items.Add(itemTool);
      line := Copy(line, Pos('"itemValue":"', line) + 12, Length(line));
      itemFound :=  AnsiContainsStr(line, '"itemName":"');
    end;
    if lbConfigurationSettingTool.Items.Count > 0 then
      lbConfigurationSettingTool.ItemIndex := 0;
  end;
end;

function TformTool.prepareItemSettingTool: String;
var
  recSep, itemTool: string;
begin
  recSep := #30 + #30;
  itemTool := '{' + recSep + #9 + '"@type":"ConfigurationSetting", ' + #9;
  itemTool := itemTool + '"itemName":"' + edItemName.Text + '",' + recSep;
  itemTool := itemTool + '"itemValue":"' + edItemValue.Text + '"' + #9 + '}';
  Result := itemTool;
end;

procedure TformTool.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line, recSep, crlf:string;
  idx: integer;
begin
  //dir := GetCurrentDir;
  AssignFile(fileJSON, FpathCase + FuuidCase + '-tool.json');
  if lbTool.Items.Count > 0 then
  begin
    crlf := #13 + #10;
    recSep := #30 + #30;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_TOOL":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbTool.Items.Count - 2 do
      WriteLn(fileJSON, lbTool.Items[idx] + ',');

    if lbTool.Items.Count > 0 then
      WriteLn(fileJSON, lbTool.Items[idx]);

    WriteLn(fileJSON, #9#9 + ']');  // it's important write in separate lines
    WriteLn(fileJSON, #9#9 + '}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-tool.json');


  formTool.Close;
end;

procedure TformTool.btnAddItemClick(Sender: TObject);
var
  itemTool: string;
begin
  //cr := #13 + #10;
  itemTool := prepareItemSettingTool();
  lbConfigurationSettingTool.Items.Add(itemTool);
  edItemName.Text := '';
  edItemValue.Text := '';
end;

procedure TformTool.btnAddToolClick(Sender: TObject);
var
  line, recSep: string;
  Uid: TGUID;
  idx: integer;
begin
  if (Trim(edName.Text) = '') or (cbToolType.ItemIndex = -1)  then
    ShowMessage('Name and/or type tool is missing!')
  else
  begin
    //cr := #13  +#10;
    recSep := #30 + #30;
    CreateGUID(Uid);
    line := '{"@id":"' + GuidToString(Uid) + '", "@type":"Tool", "name":"' + edName.Text;
    line := line +  '", "toolType":"' + cbToolType.Items[cbToolType.ItemIndex] + '", ';
    line := line + '"creator":"' + edCreator.Text + '", "version":"' + edVersion.Text + '"';
    if lbConfigurationSettingTool.Items.Count > 0 then
    begin
      line := line + ',' + recSep + '"propertyBundle":[' + recSep + '{' + recSep;
      line := line + #9 + '"@type":"ToolConfiguration",' + recSep + #9 + '"configurationSetting":[' + recSep;
      for idx:=0 to  lbConfigurationSettingTool.Items.Count - 2 do
        line := line + #9 + lbConfigurationSettingTool.Items[idx] + ',' + recSep;

      line := line + #9 + lbConfigurationSettingTool.Items[idx] + recSep;
      line := line + recSep + #9 + ']' + recSep + '}' + recSep + ']' + recSep + '}';
    end
    else
      line := line + '}';

    lbTool.Items.Add(line);
  end;
  edName.Text := '';
  edCreator.Text := '';
  edVersion.Text := '';
  lbConfigurationSettingTool.Items.Clear;
end;

procedure TformTool.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTool.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTool.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine: String;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  lbTool.Items.Clear;
  edName.Text := '';
  cbToolType.ItemIndex := -1;
  edCreator.Text := '';
  edVersion.Text := '';
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-tool.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-tool.json');
    Reset(fileJSON);

    while not Eof(fileJSON) do
    begin
      ReadLn(fileJSON, line);
      line := Trim(line);
      if (line = '{') or (line = '}') or  (line = ']') or (AnsiContainsStr(line, 'OBJECTS_TOOL'))  then  // first or last line or root element
      else
      begin
        subLine := Copy(line, Length(line), 1); // rule out of comma

        if subLine = ',' then
          line := Copy(line, 1, Length(line) - 1);

        lbTool.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTool.ShowModal;

end;

procedure TformTool.ShowWithParamaterOld(uuidCase: String);
var
  json, dir, arrayTool: string;
  sreader: TStringReader;
  jreader: TJsonTextReader;
  inTool: Boolean;
  listTool: TStringList;
  idx:integer;
  LJsonArr   : TJSONArray;
  LJsonValue : TJSONValue;
  LItem     : TJSONValue;
begin
  FuuidCase :=  uuidCase;
  dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(dir + '\' + FuuidCase + '-tool.json') then
  begin
    lbTool.Items.Clear;
    listTool := TStringList.Create;
    listTool.LoadFromFile(dir + '\' + FuuidCase + '-tool.json');
    for idx :=1 to  listTool.Count -1 do
    begin
      if AnsiContainsStr(listTool[idx], '"TOOL":')  then
        arrayTool := '['
      else
        arrayTool := arrayTool + listTool[idx];
    end;

    arrayTool := Copy(arrayTool, 1, Length(arrayTool) - 1); // get rid of last {
    LJsonArr    := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(arrayTool),0) as TJSONArray;
   for LJsonValue in LJsonArr do
   begin
      for LItem in TJSONArray(LJsonValue) do
        ShowMessage(TJSONPair(LItem).JsonString.Value + ' ' +  TJSONPair(LItem).JsonValue.Value);
        //Writeln(Format('%s : %s',[TJSONPair(LItem).JsonString.Value, TJSONPair(LItem).JsonValue.Value]));
   end;

  end;formTool.ShowModal;
  end;



end.