unit caseGenerator_trace_disk_partition;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.JSON.Readers, System.JSON.Types, System.JSON, caseGenerator_util;

type
  TformTraceDiskPartition = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    btnClose: TButton;
    btnAddTrace: TButton;
    btnDeleteTrace: TButton;
    lbTrace: TListBox;
    panelFile: TPanel;
    Label6: TLabel;
    Label3: TLabel;
    panelHash: TPanel;
    Label9: TLabel;
    Label10: TLabel;
    cbHashMethod: TComboBox;
    Label11: TLabel;
    edHashValue: TEdit;
    Label12: TLabel;
    edHashSize: TEdit;
    cbType: TComboBox;
    Label13: TLabel;
    edOffset: TEdit;
    Label14: TLabel;
    edLength: TEdit;
    lbPartition: TListBox;
    btnAddPartition: TButton;
    btnRemovePartition: TButton;
    btnCancel: TButton;
    btnModifyTrace: TButton;
    edID: TEdit;
    procedure btnAddTraceClick(Sender: TObject);
    procedure btnDeleteTraceClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnAddPartitionClick(Sender: TObject);
    procedure btnRemovePartitionClick(Sender: TObject);
    procedure lbTraceChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyTraceClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
    function prepareTrace(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceDiskPartition: TformTraceDiskPartition;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceDiskPartition.btnDeleteTraceClick(Sender: TObject);
begin
  lbTrace.Items.Delete(lbTrace.ItemIndex);
end;

procedure TformTraceDiskPartition.btnModifyTraceClick(Sender: TObject);
begin
  if lbTrace.ItemIndex > - 1 then
    lbTrace.Items[lbTrace.ItemIndex] := prepareTrace('modify');
end;

procedure TformTraceDiskPartition.btnRemovePartitionClick(Sender: TObject);
begin
  if lbPartition.ItemIndex > - 1 then
    lbPartition.Items.Delete(lbPartition.ItemIndex);
end;

function TformTraceDiskPartition.JsonTokenToString(const t: TJsonToken): string;
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

procedure TformTraceDiskPartition.lbTraceChange(Sender: TObject);
var
  line, recSep, linePartition, partitionType: String;
  nPosLength, idx: Integer;

begin
  if lbTrace.ItemIndex > - 1 then
  begin
    recSep := #30 + #30;
    line := lbTrace.Items[lbTrace.ItemIndex];
    edHashValue.Text := ExtractField(line, '"hashValue":"');
    edHashSize.Text := ExtractField(line, '"sizeInBytes":"');
    partitionType := ExtractField(line, '"diskPartitionType":"');

    nPosLength := Pos('partitionLength',line);
    while  nPosLength > 0 do
    begin
      linePartition := '{"@type":"DiskPartition", ' + recSep;
      linePartition := linePartition + '"diskPartitionType":"' + ExtractField(line, '"diskPartitionType":"') + '", ' + recSep;
      linePartition := linePartition +  '"partitionID":"' + ExtractField(line, '"partitionID":"') + '",' + recSep;
      linePartition := linePartition +  '"partitionOffset":"' + ExtractField(line, '"partitionOffset":"') + '",' + recSep;
      linePartition := linePartition +  '"partitionLength":"' + ExtractField(line, '"partitionLength":"') + '"}' + recSep;
      lbPartition.Items.Add(linePartition);
      line := Copy(line, nPosLength  + 14, Length(line));
      nPosLength := Pos('partitionLength',line);
    end;
  end;

  if lbPartition.Items.Count > 0 then
  begin
    edID.Text := ExtractField(lbPartition.Items[0], '"partitionID":"');
    edOffset.Text := ExtractField(lbPartition.Items[0], '"partitionOffset":"');
    edLength.Text := ExtractField(lbPartition.Items[0], '"partitionLength":"');
  end;

  for idx:= 0 to cbType.Items.Count - 1 do
  begin
    if cbType.Items[idx]= partitionType then
    begin
      cbType.ItemIndex := idx;
      break;
    end;
  end;

end;

function TformTraceDiskPartition.prepareTrace(operation: String): String;
var
  line, recSep, indent: string;
  Uid: TGUID;
  idx: integer;
begin
   //cr := #13  +#10;
    recSep := #30 + #30;
    indent := '   ';

    line := '{' + recSep;

    idx := 0;
    if operation = 'add' then
    begin
      CreateGUID(Uid);
      line := line + indent + '"@id":"' + GuidToString(Uid) + '", ' + recSep;
      line := line + indent + '"@type":"Trace",' + recSep;
    end
    else
    begin
      idx := lbTrace.ItemIndex;
      line := line + indent + '"@id":"' + ExtractField(lbTrace.Items[idx], '"@id":"') + '", ' + recSep;
      line := line + indent + '"@type":"Trace",' + recSep;
    end;

    line := line +  indent + '"propertyBundle":[' + recSep;
    for idx:=0  to lbPartition.Items.Count - 1 do
      line := line + lbPartition.Items[idx] + ',' + recSep;

    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"type":"ContentData",' + recSep;
    line := line + RepeatString(indent, 2) + '"hash":[' + recSep;
    line := line + RepeatString(indent, 2) + '{"@type":"Hash",' + recSep;
    line := line + RepeatString(indent, 2) + '"hashMethod":"' + cbHashMethod.Items[cbHashMethod.ItemIndex] + '",' + recSep;
    line := line + RepeatString(indent, 2) + '"hashValue":"' + edHashValue.Text + '"' + recSep;
    line := line + indent + '}' + recSep;
    line := line + indent + '], ' + recSep;
    line := line + RepeatString(indent, 2) + '"SizeInBytes":"' + edHashSize.Text + '"' + recSep;
    line := line + indent + '}' + recSep + indent + ']' + recSep + '}';
    Result := line;
end;

procedure TformTraceDiskPartition.btnCancelClick(Sender: TObject);
begin
  formTraceDiskPartition.Close;
end;

procedure TformTraceDiskPartition.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line, recSep, crlf:string;
  idx: integer;
begin
  if lbTrace.Items.Count > 0 then
  begin
    crlf := #13 + #10;
    recSep := #30 + #30;
    idx:= 0;
    //dir := GetCurrentDir;
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceDISK_PARTITION.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_DISK_PARTITION":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbTrace.Items.Count - 2 do
      WriteLn(fileJSON, lbTrace.Items[idx] + ',');

    WriteLn(fileJSON, lbTrace.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');  // it's important write in separate lines
    WriteLn(fileJSON, #9#9 + '}');
    // the last ] and } must be written on two separate lines !!!
    //WriteLn(fileJSON, #9#9 + ']}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceDISK_PARTITION.json');

  formTraceDiskPartition.Close;
end;

procedure TformTraceDiskPartition.btnAddPartitionClick(Sender: TObject);
var
  recSep, line, indent : String;
begin
  recSep := #30 + #30;
  indent := '   ';

  if (Trim(edID.Text) = '') or (Trim(edOffset.Text) = '') or (Trim(edLength.Text) = '') then
    ShowMessage('ID and/or Offset and/or Length are missing')
  else
  begin
    line := indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":"DiskPartition", ' + recSep;
    line := line + RepeatString(indent, 2) + '"diskPartitionType":"' + cbType.Items[cbType.ItemIndex] + '", ' + recSep;
    line := line +  RepeatString(indent, 2) + '"partitionID":"' + edID.Text + '",' + recSep;
    line := line +  RepeatString(indent, 2) + '"partitionOffset":"' + edOffset.Text + '",' + recSep;
    line := line +  RepeatString(indent, 2) + '"partitionLength":"' + edLength.Text + '"' + recSep;
    line := line + indent + '}';
    lbPartition.Items.Add(line);
  end;

end;

procedure TformTraceDiskPartition.btnAddTraceClick(Sender: TObject);
begin
  if (lbPartition.Items.Count = 0) or (Trim(edHashValue.Text) = '')  then
    ShowMessage('Partition data and/or Hash value are missing!')
  else
  begin
    lbTrace.Items.Add(prepareTrace('add'));
    edID.Text := '';
    edOffset.Text := '';
    edLength.Text := '';
    lbPartition.Items.Clear;
    edHashValue.Text := '';
    edHashSize.Text := '';
    cbHashMethod.ItemIndex := 0;
  end;


end;

procedure TformTraceDiskPartition.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceDiskPartition.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceDiskPartition.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine, dir:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-traceDISK_PARTITION.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceDISK_PARTITION.json');
    Reset(fileJSON);
    lbTrace.Items.Clear;
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

  formTraceDiskPartition.ShowModal;

end;

end.
