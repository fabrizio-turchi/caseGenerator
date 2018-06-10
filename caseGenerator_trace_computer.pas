unit caseGenerator_trace_computer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.JSON.Readers, System.JSON.Types, System.JSON, caseGenerator_util;

type
  TformTraceComputer = class(TForm)
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
    Label4: TLabel;
    Panel2: TPanel;
    Label9: TLabel;
    edMacAddress: TEdit;
    Panel3: TPanel;
    edBIOSmanufacturer: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    edBIOSversion: TEdit;
    Label13: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Edit5: TEdit;
    Edit7: TEdit;
    Panel4: TPanel;
    Label16: TLabel;
    edOsName: TEdit;
    Label17: TLabel;
    edOsManufacturer: TEdit;
    Label18: TLabel;
    edOsVersion: TEdit;
    procedure btnAddToolClick(Sender: TObject);
    procedure btnDeleteToolClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbTraceChange(Sender: TObject);
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
  formTraceComputer: TformTraceComputer;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceComputer.btnDeleteToolClick(Sender: TObject);
begin
  lbTrace.Items.Delete(lbTrace.ItemIndex);
end;

procedure TformTraceComputer.FormShow(Sender: TObject);
begin
  //SendMessage(lbTool.Handle, LB_SETHORIZONTALEXTENT, 1000, 0);
end;

function TformTraceComputer.JsonTokenToString(const t: TJsonToken): string;
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

procedure TformTraceComputer.lbTraceChange(Sender: TObject);
var
line: String;
begin
  line := lbTrace.Items[lbTrace.ItemIndex];
  edDeviceManufacturer.Text := ExtractField(line, '"manufacturer":"');
end;

procedure TformTraceComputer.btnCloseClick(Sender: TObject);
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
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceCOMPUTER.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_TRACE":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbTrace.Items.Count - 2 do
      WriteLn(fileJSON, lbTrace.Items[idx] + ',');

    WriteLn(fileJSON, lbTrace.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']}');
    CloseFile(fileJSON);
  end;

  formTraceComputer.Close;
end;

procedure TformTraceComputer.btnAddToolClick(Sender: TObject);
var
  line, recSep: string;
  Uid: TGUID;
  idx: integer;
begin
  if (Trim(edDeviceManufacturer.Text) = '') or (Trim(edMacAddress.Text) = '')  then
    ShowMessage('Manufacturer and/or MAC Address are missing!')
  else
  begin
    //cr := #13  +#10;
    recSep := #30 + #30;
    CreateGUID(Uid);
    line := '{"@id":"' + GuidToString(Uid) + '", "@type":"Trace",';
    line := line +  recSep + '"propertyBundle":[' + recSep + '{' + recSep;
    line := line + #9 + '"@type":"Device",' + recSep;
    line := line + #9 + '"manufacturer":"' + edDeviceManufacturer.Text + '",' + recSep;
    line := line + #9 + '"model":"' + edDeviceModel.Text + '",' + recSep;
    line := line + #9 + '"serialNumber":"' + edDeviceSerial.Text + '"' + recSep;
    line := line + #9 + '},';
    line := line + #9 + '{"@type":"OperatingSystem",' + recSep;
    line := line + #9 + '"name":"' + edOsName.Text + '",' + recSep;
    line := line + #9 + '"version":"' + edOsVersion.Text + '",' + recSep;
    line := line + #9 + '"manufacturer":"' + edOsManufacturer.Text + '"' + recSep;
    line := line + #9 + '},' + recSep;
    line := line + #9 + '{"@type":"MACAddress",' + recSep;
    line := line + #9 + '"address":"' + edMacAddress.Text + '"' + recSep;
    line := line + #9 + '},' + recSep;
    line := line + #9 + '{"@type":"ComputerSpecification",' + recSep;
    line := line + #9 + '"biosManufacturer":"' + edBiosManufacturer.Text + '", ' + recSep;
    line := line + #9 + '"biosVersion":"' + edBiosVersion.Text + '" ' + recSep;
    line := line + #9 + '}' + recSep;
    line := line + #9 + ']}' + recSep;
    lbTrace.Items.Add(line);
    edDeviceManufacturer.Text := '';
    edDeviceModel.Text := '';
    edDeviceSerial.Text := '';
    edMACAddress.Text := '';
    edBIOSmanufacturer.Text := '';
    edBiosVersion.Text := '';
    edOsName.Text := '';
    edOsVersion.Text := '';
    edOsManufacturer.Text := '';
  end;


end;

procedure TformTraceComputer.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceComputer.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceComputer.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir + '/';
  lbTrace.Items.Clear;
  // read file JSON uuidCase-traceCOMPUTER.json
  if FileExists(FpathCase + FuuidCase + '-traceCOMPUTER.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceCOMPUTER.json');
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

  formTraceComputer.ShowModal;

end;

end.
