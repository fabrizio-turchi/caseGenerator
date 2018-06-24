unit caseGenerator_trace_file;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.JSON.Readers, System.JSON.Types, System.JSON, caseGenerator_util;

type
  TformTraceFile = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    btnClose: TButton;
    btnAddTool: TButton;
    btnDeleteTool: TButton;
    edPath: TEdit;
    lbTrace: TListBox;
    edSystemType: TEdit;
    panelFile: TPanel;
    Label6: TLabel;
    Label4: TLabel;
    edExtension: TEdit;
    Label3: TLabel;
    Label7: TLabel;
    cbDirectory: TComboBox;
    Label8: TLabel;
    edSize: TEdit;
    panelHash: TPanel;
    Label9: TLabel;
    Label10: TLabel;
    cbHashMethod: TComboBox;
    Label11: TLabel;
    edHashValue: TEdit;
    Label12: TLabel;
    edHashSize: TEdit;
    rbPath: TRadioButton;
    rbURL: TRadioButton;
    cbCreationDay: TComboBox;
    cbCreationMonth: TComboBox;
    cbCreationYear: TComboBox;
    timeCreation: TTimeEdit;
    Label5: TLabel;
    btnModifyTrace: TButton;
    btnCancel: TButton;
    memoName: TMemo;
    procedure btnAddToolClick(Sender: TObject);
    procedure btnDeleteToolClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbTraceChange(Sender: TObject);
    procedure btnModifyTraceClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    //function ExtractField(line, subLine: String): String;
    function PrepareItemTrace(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceFile: TformTraceFile;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceFile.btnDeleteToolClick(Sender: TObject);
begin
  lbTrace.Items.Delete(lbTrace.ItemIndex);
end;


procedure TformTraceFile.btnModifyTraceClick(Sender: TObject);
begin
  if lbTrace.ItemIndex > - 1 then
    lbTrace.Items[lbTrace.ItemIndex] := prepareItemTrace('modify');
end;

procedure TformTraceFile.FormShow(Sender: TObject);
var
  idx: Integer;
begin
  cbCreationYear.Items.Clear;
  //SendMessage(lbTool.Handle, LB_SETHORIZONTALEXTENT, 1000, 0);
  for idx:=2000 to 2020 do
    cbCreationYear.Items.Add(IntToStr(idx));

  memoName.Lines.Clear;
  edPath.Text := '';
  edExtension.Text := '';
  edSystemType.Text := '';
  cbDirectory.ItemIndex := 0;
  cbCreationDay.ItemIndex := -1;
  cbCreationMonth.ItemIndex := -1;
  cbCreationYear.ItemIndex := -1;
  edSize.Text :=   '';
  edHashValue.Text :=  '';
  cbHashMethod.ItemIndex := -1;
  edHashSize.Text :=   '';
end;

function TformTraceFile.JsonTokenToString(const t: TJsonToken): string;
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

procedure TformTraceFile.lbTraceChange(Sender: TObject);
var
  line, cbValue, creationDate: String;
  sDate, sDay, sMonth, sYear: String;
  idx: Integer;
begin
  if lbTrace.ItemIndex > - 1 then
  begin
    line := lbTrace.Items[lbTrace.ItemIndex];
    memoName.Lines.Text := ExtractField(line, '"fileName":"');
    edPath.Text := ExtractField(line, '"filePath":"');
    edExtension.Text := ExtractField(line, '"extension":"');
    edSystemType.Text := ExtractField(line, '"fileSystemType":"');
    cbDirectory.Items.Text :=  ExtractField(line, '"isDirectory":"');
    cbDirectory.ItemIndex := 0;
    edSize.Text :=   ExtractField(line, '"sizeInBytes":"');
    edHashValue.Text :=  ExtractField(line, '"hashValue":"');
    cbValue := ExtractField(line, '"hashMethod":"');

    creationDate := ExtractField(line, '"createdTime":"');
    sDate := Copy(creationDate, 1, 10);
    sDay := Copy(sDate, 9, 2);
    for idx:=0 to cbCreationDay.Items.Count - 1 do
    begin
      if cbCreationDay.Items[idx] = sDay then
      begin
        cbCreationDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 6, 2);
    for idx:=0 to cbCreationMonth.Items.Count - 1 do
    begin
      if cbCreationMonth.Items[idx] = sMonth then
      begin
        cbCreationMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbCreationYear.Items.Count - 1 do
    begin
      if cbCreationYear.Items[idx] = sYear then
      begin
        cbCreationYear.ItemIndex := idx;
        break;
      end;
    end;

    timeCreation.Text := Copy(creationDate, 12, 8);

    for idx:=0 to cbHashMethod.Count - 1 do
    begin
      if AnsiContainsStr(cbHashMethod.Items[idx], cbValue) then
      begin
        cbHashMethod.ItemIndex := idx;
        break;
      end;
    end;
  end;

  //line := Copy(line, Pos('"sizeInBytes', line) + 10,Length(line));
  edHashSize.Text :=   ExtractField(line, '"SizeInBytes":"');
end;

function TformTraceFile.PrepareItemTrace(operation: String): String;
var
  line, recSep: string;
  Uid: TGUID;
  idx: integer;
begin
  if (Trim(memoName.Text) = '') or (Trim(edPath.Text) = '')  then
    ShowMessage('Name and/or Path are missing!')
  else
  begin
    //cr := #13  +#10;
    recSep := #30 + #30;
    if operation = 'add' then
    begin
      CreateGUID(Uid);
      line := '{"@id":"' + GuidToString(Uid) + '", "@type":"Trace",';
    end
    else
    begin
      idx := lbTrace.ItemIndex;
      line := '{"@id":"' + ExtractField(lbTrace.Items[idx], '"@id":"') + '", "@type":"Trace",';
    end;

    line := line +  recSep + '"propertyBundle":[' + recSep + '{' + recSep;
    line := line + #9 + '"@type":"File",' + recSep;
    line := line + #9 + '"fileName":"' + memoName.Text + '",' + recSep;
    line := line + #9 + '"filePath":"' + edPath.Text + '",' + recSep;
    line := line + #9 + '"extension":"' + edExtension.Text + '",' + recSep;
    line := line + #9 + '"fileSystemType":"' + edSystemType.Text + '",' + recSep;
    line := line + #9 + '"isDirectory":"' + cbDirectory.Items[cbDirectory.ItemIndex] + '",' + recSep;
    line := line + #9 + '"sizeInBytes":"' + edSize.Text + '",' + recSep;
    line := line + #9 + '"createdTime":"';
    line := line + cbCreationYear.Items[cbCreationYear.ItemIndex] + '-';
    line := line + cbCreationMonth.Items[cbCreationMonth.ItemIndex] + '-';
    line := line + cbCreationDay.Items[cbCreationDay.ItemIndex];
    line := line + 'T' + TimeToStr(timeCreation.Time) + 'Z"},' + recSep;
    line := line + #9 + '{"type":"ContentData",' + recSep;
    line := line + '"hash":[' + recSep;
    line := line + '{"@type":"Hash",' + recSep;
    line := line + '"hashMethod":"' + cbHashMethod.Items[cbHashMethod.ItemIndex] + '",' + recSep;
    line := line + '"hashValue":"' + edHashValue.Text + '"}' + recSep;
    line := line + '], ' + recSep;
    line := line + '"SizeInBytes":"' + edHashSize.Text + '"' + recSep;
    line := line + '}]}';

    Result := line;
  end;

end;

procedure TformTraceFile.btnCancelClick(Sender: TObject);
begin
   formTraceFile.Close;
end;

procedure TformTraceFile.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line, recSep:string;
  idx: integer;
begin
  //dir := GetCurrentDir;
  // create file JSON uuidCase-traceFILE.json
  AssignFile(fileJSON, FpathCase + FuuidCase + '-traceFILE.json');
  if lbTrace.Items.Count > 0 then
  begin
    recSep := #30 + #30;
    idx:= 0;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_FILE":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbTrace.Items.Count - 2 do
      WriteLn(fileJSON, lbTrace.Items[idx] + ',');

    if lbTrace.Items.Count > 0 then
      WriteLn(fileJSON, lbTrace.Items[idx]);

    WriteLn(fileJSON, #9#9 + ']');  // it's important write in separate lines
    WriteLn(fileJSON, #9#9 + '}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceFILE.json');

  formTraceFile.Close;
end;

procedure TformTraceFile.btnAddToolClick(Sender: TObject);
begin
  if (Trim(memoName.Text) = '') or (Trim(edPath.Text) = '')  then
    ShowMessage('Name and/or Path are missing!')
  else
  begin
    lbTrace.Items.Add(prepareItemTrace('add'));
    memoName.Text := '';
    edSystemType.Text := '';
    edExtension.Text := '';
    edSize.Text := '';
    edHashValue.Text := '';
    edHashSize.Text := '';
    edPath.Text := '';
    cbHashMethod.ItemIndex := 0;
    cbDirectory.ItemIndex := 0;

  end;


end;

procedure TformTraceFile.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceFile.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceFile.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine: String;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  lbTrace.Items.Clear;
  // read file JSON uuidCase-traceFILE.json
  if FileExists(FpathCase + FuuidCase + '-traceFILE.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceFILE.json');
    Reset(fileJSON);
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

        lbTrace.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTraceFile.ShowModal;

end;

end.
