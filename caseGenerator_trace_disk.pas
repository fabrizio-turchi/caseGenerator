unit caseGenerator_trace_disk;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.JSON.Readers, System.JSON.Types, System.JSON, caseGenerator_util;

type
  TformTraceDisk = class(TForm)
    Label1: TLabel;
    edDeviceManufacturer: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    btnClose: TButton;
    btnAddTool: TButton;
    btnDeleteTool: TButton;
    Label5: TLabel;
    edDeviceSerial: TEdit;
    lbTrace: TListBox;
    edDeviceModel: TEdit;
    Panel1: TPanel;
    Label6: TLabel;
    btnModifyTrace: TButton;
    btnCancel: TButton;
    edDeviceCapacity: TEdit;
    Label4: TLabel;
    Button1: TButton;
    Button2: TButton;
    Panel3: TPanel;
    Label7: TLabel;
    edHashMethod: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    edHashValue: TEdit;
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
    function prepareItemTrace(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceDisk: TformTraceDisk;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceDisk.btnDeleteToolClick(Sender: TObject);
begin
  lbTrace.Items.Delete(lbTrace.ItemIndex);
end;

procedure TformTraceDisk.btnModifyTraceClick(Sender: TObject);
begin
  if lbTrace.ItemIndex > -1 then
    lbTrace.Items[lbTrace.ItemIndex] := prepareItemTrace('modify');
end;

procedure TformTraceDisk.FormShow(Sender: TObject);
begin
  //SendMessage(lbTool.Handle, LB_SETHORIZONTALEXTENT, 1000, 0);
end;

function TformTraceDisk.JsonTokenToString(const t: TJsonToken): string;
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

procedure TformTraceDisk.lbTraceChange(Sender: TObject);
var
line: String;
begin
  if lbTrace.ItemIndex > - 1 then
  begin
    line := lbTrace.Items[lbTrace.ItemIndex];
    edDeviceManufacturer.Text := ExtractField(line, '"uco-observable:manufacturer":"');
    edDeviceModel.Text := ExtractField(line, '"uco-observable:model":"');
    edDeviceSerial.Text := ExtractField(line, '"uco-observable:serialNumber":"');
    edDeviceCapacity.Text := ExtractField(line, '"uco-observable:diskSize":"');
    edHashMethod.Text :=     ExtractField(line, '"uco-observable:hashMethod":"');
    edHashValue.Text :=     ExtractField(line, '"uco-observable:hashValue":"');
  end;
end;

function TformTraceDisk.prepareItemTrace(operation: String): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: integer;
begin
  if (Trim(edDeviceManufacturer.Text) = '') or (Trim(edDeviceModel.Text) = '')  then
  begin
    ShowMessage('Manufacturer and/or Model are missing!');
    result := '';
  end
  else
  begin
    //cr := #13  +#10;
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
    line := line + indent + '"uco-core:facets":[' + recSep;
    line := line +  RepeatString(indent, 2) + '{' + recSep;
    line := line +  RepeatString(indent, 3)  + '"@type":"uco-observable:Device",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:manufacturer":"' + edDeviceManufacturer.Text + '",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:model":"' + edDeviceModel.Text + '",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:serialNumber":"' + edDeviceSerial.Text + '",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:devideType":"HDD"' + recSep;
    line := line +  RepeatString(indent, 2)  + '},' + recSep;
    line := line +  RepeatString(indent, 2) + '{' + recSep;
    line := line +  RepeatString(indent, 3)  + '"@type":"uco-observable:Disk",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:diskSize":"' + edDeviceCapacity.Text + '",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:diskType":"Fixed"' + recSep;
    line := line +  RepeatString(indent, 2)  + '},' + recSep;
    line := line +  RepeatString(indent, 2) + '{' + recSep;
    line := line +  RepeatString(indent, 3)  + '"@type":"uco-observable:ContentData",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:hash":[' + recSep;
    line := line +  RepeatString(indent, 3)  + '{' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:hashMethod":"' + edHashMethod.Text + '",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:hashValue":"' + edHashValue.Text + '"' + recSep;
    line := line +  RepeatString(indent, 2)  + '}' + recSep;
    line := line +  RepeatString(indent, 2)  + ']' + recSep;
    line := line +  indent  + '}' + recSep;


    line := line + indent + ']' + indent + recSep + '}' + recSep;
    Result := line;
  end;

end;

procedure TformTraceDisk.btnCancelClick(Sender: TObject);
begin
  formTraceDisk.Close;
end;

procedure TformTraceDisk.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line :string;
  idx: integer;
begin

  AssignFile(fileJSON, FpathCase + FuuidCase + '-traceDISK.json');
  if lbTrace.Items.Count > 0 then
  begin
    idx:= 0;
    //dir := GetCurrentDir;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_DISK":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbTrace.Items.Count - 2 do
      WriteLn(fileJSON,  #9#9 + lbTrace.Items[idx] + ',');

    WriteLn(fileJSON,  #9#9 + lbTrace.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceDISK.json');

  formTraceDisk.Close;
end;

procedure TformTraceDisk.btnAddToolClick(Sender: TObject);
begin
    if prepareItemTrace('add') = '' then
    else
    begin
      lbTrace.Items.Add(prepareItemTrace('add'));
      edDeviceManufacturer.Text := '';
      edDeviceModel.Text := '';
      edDeviceSerial.Text := '';
      edDeviceCapacity.Text := '';
    end;
end;

procedure TformTraceDisk.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceDisk.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceDisk.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir + '/';
  lbTrace.Items.Clear;
  // read file JSON uuidCase-traceDISK.json
  if FileExists(FpathCase + FuuidCase + '-traceDISK.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceDISK.json');
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

  formTraceDisk.ShowModal;

end;

end.
