unit caseGenerator_trace_SIM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.JSON.Readers, System.JSON.Types, System.JSON, caseGenerator_util;

type
  TformTraceSIM = class(TForm)
    Label1: TLabel;
    edType: TEdit;
    Label2: TLabel;
    btnClose: TButton;
    btnAddTrace: TButton;
    btnDeleteTrace: TButton;
    Label4: TLabel;
    edCarrier: TEdit;
    edPIN: TEdit;
    Label5: TLabel;
    edCapacity: TEdit;
    Label8: TLabel;
    lbTrace: TListBox;
    cbForm: TComboBox;
    Label3: TLabel;
    edPUK: TEdit;
    Label7: TLabel;
    Label6: TLabel;
    edICCID: TEdit;
    Label9: TLabel;
    edPhoneNumber: TEdit;
    btnModifyTrace: TButton;
    btnCancel: TButton;
    procedure btnAddTraceClick(Sender: TObject);
    procedure btnDeleteTraceClick(Sender: TObject);
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
    function ExtractField(line, subLine: String): String;
    function PrepareItemTrace(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceSIM: TformTraceSIM;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceSIM.btnDeleteTraceClick(Sender: TObject);
begin
  lbTrace.Items.Delete(lbTrace.ItemIndex);
end;

procedure TformTraceSIM.btnModifyTraceClick(Sender: TObject);
begin
  if lbTrace.ItemIndex > -1 then
    lbTrace.Items[lbTrace.ItemIndex] := PrepareItemTrace('modify');

end;

function TformTraceSIM.ExtractField(line, subLine: String): String;
var
  fieldValue: String;
  fieldStart, fieldEnd: Integer;
begin
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
  fieldEnd   := Pos('"', fieldValue);
  Result := Copy(fieldValue, 1, fieldEnd - 1);
end;

procedure TformTraceSIM.FormShow(Sender: TObject);
begin
  //SendMessage(lbTool.Handle, LB_SETHORIZONTALEXTENT, 1000, 0);
  edType.Text := '';
  cbForm.ItemIndex := -1;
  edCapacity.Text := '';
  edCarrier.Text := '';
  edICCID.Text :=  '';
  edPhoneNumber.Text :=   '';
  edPIN.Text :=  '';
  edPUK.Text :=  '';
end;

function TformTraceSIM.JsonTokenToString(const t: TJsonToken): string;
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

procedure TformTraceSIM.lbTraceChange(Sender: TObject);
var
  line, cbValue: String;
  idx: Integer;
begin
  if lbTrace.ItemIndex > - 1 then
  begin
    line := lbTrace.Items[lbTrace.ItemIndex];

    edType.Text := ExtractField(line, '"SIMType":"');
    cbValue := ExtractField(line, '"SIMForm":"');
    for idx:=0 to cbForm.Count - 1 do
    begin
      if AnsiContainsStr(cbForm.Items[idx], cbValue) then
      begin
        cbForm.ItemIndex := idx;
        break;
      end;
    end;
    edCapacity.Text := ExtractField(line, '"StorageCapacity":"');
    edCarrier.Text := ExtractField(line, '"Carrier":"');
    edICCID.Text :=  ExtractField(line, '"ICCID":"');
    edPhoneNumber.Text :=   ExtractField(line, '"PhoneNumber":"');
    edPIN.Text :=  ExtractField(line, '"PIN":"');
    edPUK.Text :=  ExtractField(line, '"PUK":"');
  end;
end;

function TformTraceSIM.PrepareItemTrace(operation: String): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: integer;
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
    guidNoBraces :=  ExtractField(lbTrace.Items[lbTrace.ItemIndex], '"@id":"');

  line := line + indent + '"@id":"' + guidNoBraces + '", ' + recSep;
  line := line + indent + '"@type":"Trace",' + recSep;
  line := line + indent + '"propertyBundle":[' + recSep;
  line := line + indent + '{' + recSep;
  line := line + RepeatString(indent, 2) + '"@type":"' + edType.Text + '", ' + recSep;
  line := line + RepeatString(indent, 2) + '"Form":"' + cbForm.Items[cbForm.ItemIndex] + '", ' + recSep;
  line := line + RepeatString(indent, 2) + '"StorageCapacity":"' + edCapacity.Text + '", ' + recSep;
  line := line + RepeatString(indent, 2) + '"Carrier":"' + edCarrier.Text + '", ';
  line := line + RepeatString(indent, 2) + '"ICCID":"' + edICCID.Text + '", ';
  line := line + RepeatString(indent, 2) + '"PhoneNumber":"' + edPhoneNumber.Text + '" ';
  if Trim(edPIN.Text) <> '' then
     line := line + ', ' + recSep + RepeatString(indent, 2) + '"PIN":"' + edPIN.Text + '" ' ;

  if Trim(edPUK.Text) <> '' then
     line := line + ', ' + recSep + RepeatString(indent, 2) + '"PUK":"' + edPUK.Text + '" ' ;

  line := line + recSep + indent + '}]' + recSep  + '}';
  Result := line;

end;

procedure TformTraceSIM.btnCancelClick(Sender: TObject);
begin
  formTraceSIM.Close;
end;

procedure TformTraceSIM.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line, recSep, crlf:string;
  idx: integer;
begin
  if lbTrace.Items.Count > 0 then
  begin
    crlf := #13 + #10;
    recSep := #30 + #30;
    idx := 0;
    //dir := GetCurrentDir;
  // create file JSON uuidCase-traceSIM.json
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceSIM.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_SIM":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbTrace.Items.Count - 2 do
      WriteLn(fileJSON, lbTrace.Items[idx] + ',');

    WriteLn(fileJSON, lbTrace.Items[idx]);

    WriteLn(fileJSON, #9#9 + ']');  // it's important write in separate lines
    WriteLn(fileJSON, #9#9 + '}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceSIM.json');

  formTraceSIM.Close;
end;

procedure TformTraceSIM.btnAddTraceClick(Sender: TObject);
begin
  if (Trim(edType.Text) = '') or (cbForm.ItemIndex = -1)  then
    ShowMessage('Type and/or Form SIM are missing!')
  else
  begin
    lbTrace.Items.Add(PrepareItemTrace('add'));
    edType.Text := '';
    cbForm.ItemIndex := -1;
    edCapacity.Text := '';
    edCarrier.Text := '';
    edPIN.Text := '';
    edPUK.Text := '';
    edICCID.Text := '';
    edPhoneNumber.Text := '';
  end;
end;

procedure TformTraceSIM.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceSIM.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceSIM.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  lbTrace.Items.Clear;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-traceSIM.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceSIM.json');
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

  formTraceSIM.ShowModal;

end;

end.
