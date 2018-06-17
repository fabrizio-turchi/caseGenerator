unit caseGenerator_trace_mobile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.JSON.Readers, System.JSON.Types, System.JSON;

type
  TformTraceMobile = class(TForm)
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
    edMobileStorage: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    edMobileIMEI: TEdit;
    Panel3: TPanel;
    edIphoneID: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    edIphoneOwner: TEdit;
    Label13: TLabel;
    cbMobileDay: TComboBox;
    cbMobileYear: TComboBox;
    cbMobileMonth: TComboBox;
    timeMobile: TTimeEdit;
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
    Label19: TLabel;
    Label20: TLabel;
    edAccountMSISDN: TEdit;
    Panel5: TPanel;
    btnModifyTrace: TButton;
    btnCancel: TButton;
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
    function ExtractField(line, subLine: String): String;
    function prepareItemTrace: String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceMobile: TformTraceMobile;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceMobile.btnDeleteToolClick(Sender: TObject);
begin
  lbTrace.Items.Delete(lbTrace.ItemIndex);
end;

procedure TformTraceMobile.btnModifyTraceClick(Sender: TObject);
begin
  if lbTrace.ItemIndex > -1  then
    lbTrace.Items[lbTrace.ItemIndex] := prepareItemTrace();
end;

function TformTraceMobile.ExtractField(line, subLine: String): String;
var
  fieldValue: String;
  fieldStart, fieldEnd: Integer;
begin
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
  fieldEnd   := Pos('"', fieldValue);
  Result := Copy(fieldValue, 1, fieldEnd - 1);
end;

procedure TformTraceMobile.FormShow(Sender: TObject);
var
  idxStart, idxEnd, idx: integer;
begin
  idxStart := 2000;
  idxEnd := 2018;
  cbMobileYear.Items.Clear;
  for idx:= idxStart to idxEnd do
    cbMobileYear.Items.Add(IntToStr(idx));
  eddeviceManufacturer.Text := '';
  edDeviceModel.Text :=  '';
  edDeviceSerial.Text :=  '';
  edMobileIMEI.Text := '';
  edMobileStorage.Text := '';
  edAccountMSISDN.Text := '';
end;

function TformTraceMobile.JsonTokenToString(const t: TJsonToken): string;
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

procedure TformTraceMobile.lbTraceChange(Sender: TObject);
var
  line, mobileDateTime, sDate, sDay, sMonth, sYear: String;
  idx: Integer;
begin
  if lbTrace.ItemIndex > -1 then
  begin
    line := lbTrace.Items[lbTrace.ItemIndex];

    eddeviceManufacturer.Text := ExtractField(line, '"manufacturer":"');
    edDeviceModel.Text :=  ExtractField(line, '"model":"');
    edDeviceSerial.Text :=  ExtractField(line, '"serialNumber":"');
    edMobileIMEI.Text := ExtractField(line, '"IMEI":"');
    edMobileStorage.Text := ExtractField(line, '"storageCapacity":"');
    edAccountMSISDN.Text := ExtractField(line, '"MSISDN":"');
    mobileDateTime := ExtractField(line, '"clockSetting":"');
    sDate := Copy(mobileDateTime, 1, 10);
    sDay := Copy(sDate, 9, 2);
    for idx:=0 to cbMobileDay.Items.Count - 1 do
    begin
      if cbMobileDay.Items[idx] = sDay then
      begin
        cbMobileDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 6, 2);
    for idx:=0 to cbMobileMonth.Items.Count - 1 do
    begin
      if cbMobileMonth.Items[idx] = sMonth then
      begin
        cbMobileMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbMobileYear.Items.Count - 1 do
    begin
      if cbMobileYear.Items[idx] = sYear then
      begin
        cbMobileYear.ItemIndex := idx;
        break;
      end;
    end;

    timeMobile.Text := Copy(mobileDateTime, 12, 8);
  end;
end;

function TformTraceMobile.prepareItemTrace: String;
var
  line, recSep: string;
  Uid: TGUID;
  idx: integer;
begin
  recSep := #30 + #30;
    CreateGUID(Uid);
    line := '{"@id":"' + GuidToString(Uid) + '", "@type":"Trace",';
    line := line +  recSep + '"propertyBundle":[' + recSep + '{' + recSep;
    line := line + #9 + '"@type":"Device",' + recSep;
    line := line + #9 + '"manufacturer":"' + edDeviceManufacturer.Text + '",' + recSep;
    line := line + #9 + '"model":"' + edDeviceModel.Text + '",' + recSep;
    line := line + #9 + '"serialNumber":"' + edDeviceSerial.Text + '"' + recSep;
    line := line + #9 + '},';
    line := line + #9 + '{"@type":"MobileDevice",' + recSep;
    line := line + #9 + '"IMEI":"' + edMobileIMEI.Text + '",' + recSep;
    line := line + #9 + '"storageCapacity":"' + edMobileStorage.Text + '",' + recSep;
    line := line + #9 + '"clockSetting":"' + cbMobileYear.Items[cbMobileYear.ItemIndex] + '-';
    line := line + cbMobileMonth.Items[cbMobileMonth.ItemIndex] + '-';
    line := line + cbMobileDay.Items[cbMobileDay.ItemIndex];
    line := line + 'T' + TimeToStr(timeMobile.Time) + 'Z"}';

    if Trim(edIphoneID.Text) <> '' then
    begin
      line := line + #9 + ', ' + recSep + '{"@type":"iPhoneDevice", "uniqueID":"' + edIphoneID.Text + '",' + recSep;
      line := line + #9 + '"ownerName":"' + edIphoneOwner.Text + '"}' + recSep;
    end;

    if Trim(edOsName.Text) <> '' then
    begin
      line := line + #9 + ', ' + recSep + '{"@type":"OperatingSystem", "name": "' + edOsName.Text + '",' + recSep;
      line := line + #9 + '"manufacturer":"' + edOsManufacturer.Text + '"}' + recSep;
      line := line + #9 + '"version":"' + edOsVersion.Text + '"}' + recSep;
    end;

    if Trim(edAccountMSISDN.Text) <> '' then
    begin
      line := line + #9 + ', ' + recSep + '{"@type":"MobileAccount", "MSISDN":"';
      line := line  + edAccountMSISDN.Text + '"}' + recSep;
    end;

    line := line + recSep + #9 + ']' + recSep + '}';
end;

procedure TformTraceMobile.btnCancelClick(Sender: TObject);
begin
   formTraceMobile.Close;
end;

procedure TformTraceMobile.btnCloseClick(Sender: TObject);
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
  // create file JSON uuidCase-traceMOBILE.json
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceMOBILE.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_TRACE":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbTrace.Items.Count - 2 do
      WriteLn(fileJSON, lbTrace.Items[idx] + ',');

    if lbTrace.Items.Count > 0 then
      WriteLn(fileJSON, lbTrace.Items[idx]);

    WriteLn(fileJSON, #9#9 + ']');  // it's important write in separate lines
    WriteLn(fileJSON, #9#9 + '}');
    CloseFile(fileJSON);
  end;

  formTraceMobile.Close;
end;

procedure TformTraceMobile.btnAddToolClick(Sender: TObject);
begin
  if (Trim(edDeviceManufacturer.Text) = '') or (Trim(edMobileIMEI.Text) = '')  then
    ShowMessage('Manufacturer and/or IMEI are missing!')
  else
  lbTrace.Items.Add(prepareItemTrace());
  edDeviceManufacturer.Text := '';
  edDeviceModel.Text := '';
  edDeviceSerial.Text := '';
  edMobileIMEI.Text := '';
  edMobileStorage.Text := '';
  cbMobileDay.ItemIndex := -1;
  cbMobileMonth.ItemIndex := -1;
  cbMobileYear.ItemIndex := -1;
end;

procedure TformTraceMobile.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceMobile.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceMobile.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  lbTrace.Items.Clear;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase + '-traceMOBILE.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceMOBILE.json');
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

  formTraceMobile.ShowModal;

end;

end.
