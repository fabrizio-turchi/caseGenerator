unit caseGenerator_trace_disk_partition;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.JSON.Readers, System.JSON.Types, System.JSON;

type
  TformTraceDiskPartition = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    btnClose: TButton;
    btnAddTrace: TButton;
    btnDeleteTrace: TButton;
    lbTrace: TListBox;
    edID: TEdit;
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
    procedure btnAddTraceClick(Sender: TObject);
    procedure btnDeleteTraceClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnAddPartitionClick(Sender: TObject);
    procedure btnRemovePartitionClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function JsonTokenToString(const t: TJsonToken): string;
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

procedure TformTraceDiskPartition.btnRemovePartitionClick(Sender: TObject);
begin
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
    WriteLn(fileJSON, #9#9 + ']}');
    CloseFile(fileJSON);
  end;

  formTraceDiskPartition.Close;
end;

procedure TformTraceDiskPartition.btnAddPartitionClick(Sender: TObject);
var
  recSep, line : String;
begin
  recSep := #30 + #30;
  if (Trim(edID.Text) = '') or (Trim(edOffset.Text) = '') or (Trim(edLength.Text) = '') then
    ShowMessage('ID and/or Offset and/or Length are missing')
  else
  begin
    line := '{"@type":"DiskPartition", ' + recSep;
    line := line + '"diskPartitionType":"' + cbType.Items[cbType.ItemIndex] + '", ' + recSep;
    line := line +  '"partitionID":"' + edID.Text + '",' + recSep;
    line := line +  '"partitionOffset":"' + edOffset.Text + '",' + recSep;
    line := line +  '"partitionLength":"' + edLength.Text + '"}' + recSep;
  end;

end;

procedure TformTraceDiskPartition.btnAddTraceClick(Sender: TObject);
var
  line, recSep: string;
  Uid: TGUID;
  idx: integer;
begin
  if (lbPartition.Items.Count = 0) or (Trim(edHashValue.Text) = '')  then
    ShowMessage('Partition data and/or Hash value are missing!')
  else
  begin
    //cr := #13  +#10;
    recSep := #30 + #30;
    idx := 0;
    CreateGUID(Uid);
    line := '{"@id":"' + GuidToString(Uid) + '", "@type":"Trace",';
    line := line +  recSep + '"propertyBundle":[' + recSep;
    for idx:=0  to lbPartition.Items.Count - 1 do
      line := line + lbPartition.Items[idx];
    line := line + #9 + '{"type":"ContentData",' + recSep;
    line := line + '"hash":[' + recSep;
    line := line + '{"@type":"Hash",' + recSep;
    line := line + '"hashMethod":"' + cbHashMethod.Items[cbHashMethod.ItemIndex] + '",' + recSep;
    line := line + '"hashValue":"' + edHashValue.Text + '"}' + recSep;
    line := line + '], ' + recSep;
    line := line + '"SizeInBytes":"' + edHashSize.Text + '"' + recSep;
    line := line + '}]}';

    lbTrace.Items.Add(line);
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
  if FileExists(FpathCase + FuuidCase + '-traceFILE.json') then
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
