unit caseGenerator_trace_file;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.JSON.Readers, System.JSON.Types, System.JSON, caseGenerator_util,
  FMX.Memo.Types;

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
    edExtractionPath: TEdit;
    Label13: TLabel;
    cbTag: TComboBox;
    Label14: TLabel;
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
  sDate, sDay, sMonth, sYear, sTag, sHashMethod: String;
  idx: Integer;
begin
  if lbTrace.ItemIndex > - 1 then
  begin
    line := lbTrace.Items[lbTrace.ItemIndex];
    memoName.Lines.Text := ExtractField(line, '"uco-observable:fileName":"');
    edPath.Text := ExtractField(line, '"uco-observable:filePath":"');
    edExtractionPath.Text := ExtractField(line, '"uco-observable:drafting:fileLocalPath":"');
    edExtension.Text := ExtractField(line, '"uco-observable:extension":"');

    sTag :=  ExtractField(line, '"tag":["');
    for idx:=0 to cbTag.Items.Count - 1 do
    begin
      if cbTag.Items[idx] = sTag then
      begin
        cbTag.ItemIndex := idx;
        break;
      end;
    end;

    edSystemType.Text := ExtractField(line, '"uco-observable:fileSystemType":"');
    cbDirectory.Items.Text :=  ExtractField(line, '"uco-observable:isDirectory":"');
    cbDirectory.ItemIndex := 0;
    edSize.Text :=   ExtractDataField(line, '"uco-observable:sizeInBytes":');
    edHashValue.Text :=  ExtractDataField(line, '"uco-types:hashValue":');

    creationDate := ExtractDataField(line, '"uco-observable:createdTime":');
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

    sHashMethod := ExtractDataField(line,'"uco-types:hashMethod":');

    for idx:=0 to cbHashMethod.Count - 1 do
    begin
      if AnsiContainsStr(cbHashMethod.Items[idx], sHashMethod) then
      begin
        cbHashMethod.ItemIndex := idx;
        break;
      end;
    end;

    edHashSize.Text :=   ExtractField(line, '"uco-observable:SizeInBytes":"');
  end;

end;

function TformTraceFile.PrepareItemTrace(operation: String): String;
var
  line, recSep, indent, guidNoBraces, cTime: string;
  Uid: TGUID;
  idx: integer;
begin
  if (Trim(memoName.Text) = '') or (Trim(edPath.Text) = '')  then
    ShowMessage('Name and/or Path are missing!')
  else
  begin
    recSep := #30 + #30;
    indent := '   ';

    line := '{' + recSep;

    if operation = 'add' then
    begin
      CreateGUID(Uid);
      guidNoBraces := 'kb:' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
    end
    else
      guidNoBraces :=  ExtractField(lbTrace.Items[lbTrace.ItemIndex], '"@id":"');

    line := line + indent + '"@id":"' + guidNoBraces + '", ' + recSep;
    line := line + indent + '"@type":"uco-observable:CyberItem",' + recSep;
    line := line + indent + '"uco-core:tag":["' + cbTag.Items[cbTag.ItemIndex] + '"],' + recSep;
    line := line +  indent + '"uco-core:facets":[' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":"uco-observable:File",' + recSep;
    line := line + RepeatString(indent, 2) + '"uco-observable:fileName":"' + memoName.Text + '",' + recSep;
    line := line + RepeatString(indent, 2) + '"uco-observable:filePath":"' + edPath.Text + '",' + recSep;
    line := line + RepeatString(indent, 2) + '"uco-observable:drafting:fileLocalPath":"' + edExtractionPath.Text + '",' + recSep;
    line := line + RepeatString(indent, 2) + '"uco-observable:extension":"' + edExtension.Text + '",' + recSep;
    line := line + RepeatString(indent, 2) + '"uco-observable:fileSystemType":"' + edSystemType.Text + '",' + recSep;
    line := line + RepeatString(indent, 2) + '"uco-observable:isDirectory":"' + cbDirectory.Items[cbDirectory.ItemIndex] + '",' + recSep;
    line := line + RepeatString(indent, 2) + '"uco-observable:sizeInBytes": {'+ recSep;
    line := line + RepeatString(indent, 3) + '"@type":"xsd:long",' + recSep;
    line := line + RepeatString(indent, 3) + '"@value":"' + edSize.Text + '"' + recSep;
    line := line + RepeatString(indent, 2) + '},' + recSep;
    line := line + '"uco-observable:createdTime":' + recSep;
    cTime := cbCreationYear.Items[cbCreationYear.ItemIndex] + '-';
    cTime := cTime + cbCreationMonth.Items[cbCreationMonth.ItemIndex] + '-';
    cTime := cTime + cbCreationDay.Items[cbCreationDay.ItemIndex] + 'T';
    line := line + '{"@type":"xsd:dateTime",' + recSep;
    line := line + '"@value":"' + cTime + TimeToStr(timeCreation.Time) + 'Z"}' + recSep;
    line := line + RepeatString(indent, 2) + '},' + recSep;
    line := line + RepeatString(indent, 2) + '{' + recSep;
    line := line + RepeatString(indent, 3) + '"type":"uco-observable:ContentData",' + recSep;
    line := line + RepeatString(indent, 3) + '"uco-observable:hash":[' + recSep;
    line := line + RepeatString(indent, 3) + '{' + recSep;
    line := line + RepeatString(indent, 4) + '"@type":"uco-types:Hash",' + recSep;
    line := line + RepeatString(indent, 4) + '"uco-types:hashMethod":' + recSep;
    line := line + RepeatString(indent, 4) + '{' + recSep;
    line := line + RepeatString(indent, 5) + '"@type": "uco-vocabulary:HashNameVocab",' + recSep;
    line := line + RepeatString(indent, 5) + '"@value":"' + cbHashMethod.Items[cbHashMethod.ItemIndex] + '"' + recSep;
    line := line + RepeatString(indent, 4) + '},' + recSep;
    line := line + RepeatString(indent, 4) + '"uco-types:hashValue":{' + recSep;
    line := line + RepeatString(indent, 5) + '"@type": "xsd:hexBinary",' + recSep;
    line := line + RepeatString(indent, 5) + '"@value":"' + edHashValue.Text  + '"' + recSep;
    line := line + RepeatString(indent, 4) + '}' + recSep;
    line := line + RepeatString(indent, 3) + '}' + recSep;
    line := line + RepeatString(indent, 3) + ']} ' + recSep;
    line := line + indent + ']}';

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
    edExtractionPath.Text := '';
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
