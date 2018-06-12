unit caseGenerator_location;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, caseGenerator_util;

type
  TformLocation = class(TForm)
    Label1: TLabel;
    lbLocation: TListBox;
    edLocality: TEdit;
    edRegion: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnClose: TButton;
    btnAddLocation: TButton;
    btnDeleteLocation: TButton;
    edPostalCode: TEdit;
    edStreet: TEdit;
    lblLatitude: TLabel;
    edLatitude: TEdit;
    Label6: TLabel;
    edLongitude: TEdit;
    btnModifyLocation: TButton;
    btnCancel: TButton;
    procedure btnPiuClick(Sender: TObject);
    procedure btnAddLocationClick(Sender: TObject);
    procedure btnDeleteLocationClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lbLocationChange(Sender: TObject);
    procedure btnModifyLocationClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    //function ExtractField(line, subLine: String): String;
    function prepareObjectCaseLine(): String;
    procedure clearFormFields();
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formLocation: TformLocation;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformLocation.btnDeleteLocationClick(Sender: TObject);
begin
  lbLocation.Items.Delete(lbLocation.ItemIndex);
  clearFormFields();
end;

procedure TformLocation.btnModifyLocationClick(Sender: TObject);
begin
  lbLocation.Items[lbLocation.ItemIndex] := prepareObjectCaseLine();
  clearFormFields();
end;

procedure TformLocation.btnPiuClick(Sender: TObject);
var
  line: string;
  Uid: TGUID;
begin
  if (Trim(edRegion.Text) = '') or (Trim(edLocality.Text) = '')  then
    ShowMessage('Region or Locality name are missing!')
  else
  begin
    line := '{"locality":"' + edLocality.Text + '", "region":"' + edRegion.Text + '", "uuid":"' + GuidToString(Uid) + '", "postalCode":"';
    line := line +  edPostalCode.Text + '","street":"' + edStreet.Text + '"}';
    lbLocation.Items.Add(line);
  end;
end;

procedure TformLocation.clearFormFields;
begin
  edRegion.Text := '';
  edLocality.Text := '';
  edPostalCode.Text := '';
  edStreet.Text := '';
  edLatitude.Text := '';
  edLongitude.Text := '';
end;

procedure TformLocation.lbLocationChange(Sender: TObject);
var
  line: String;
begin
  line := lbLocation.Items[lbLocation.ItemIndex];
  edRegion.Text := ExtractField(line, '"region":"');
  edLocality.Text := ExtractField(line, '"locality":"');
  edPostalCode.Text := ExtractField(line, '"postalCode":"');
  edStreet.Text := ExtractField(line, '"street":"');
  edLatitude.Text := ExtractField(line, '"latitude":"');
  edLongitude.Text := ExtractField(line,'"longitude":"');
end;

function TformLocation.prepareObjectCaseLine: String;
var
  line, recSep: string;
  Uid: TGUID;
begin
  recSep := #30 + #30; // record separator, not printable
  CreateGUID(Uid);
  line := '{"@id":"' + GuidToString(Uid) + '", ' + recSep;
  line := line + '"@type":"Location", ' + recSep;
  line := line + '"propertyBundle":[{' + recSep;
  line := line + '"@type":"SimpleAddress", ' + recSep;
  line := line + '"locality":"' + edLocality.Text + '", ' + recSep;
  line := line + '"region":"' + edRegion.Text + '", ' + recSep;
  line := line + '"postalCode":"' + edPostalCode.Text + '", ' + recSep;
  line := line + '"street":"' + edStreet.Text + '"' + recSep;
  line := line + '}';
  if (Trim(edLatitude.Text) ='') or (Trim(edLongitude.Text) = '') then
  else
  begin
    line := line + ', ' + recSep;
    line := line + '{"@type":"LatLongCoordinates", ' + recSep;
    line := line + '"latitude":"' + edLatitude.Text + '", ' + recSep;
    line := line + '"longitude":"' + edLongitude.Text + '" ' + recSep;
    line := line + '}' + recSep;
    line := line + ']}';
  end;
  Result := line;
end;

{
function TformLocation.ExtractField(line, subLine: String): String;
var
  fieldValue: String;
  fieldStart, fieldEnd: Integer;
begin
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
  fieldEnd   := Pos('"', fieldValue);
  Result := Copy(fieldValue, 1, fieldEnd - 1);
end;
}
procedure TformLocation.btnCancelClick(Sender: TObject);
begin
  formLocation.Close;
end;

procedure TformLocation.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line, dir:string;
  idx: integer;
begin
  if lbLocation.Items.Count > 0 then
  begin
    //dir := GetCurrentDir;
    idx := 0;
    AssignFile(fileJSON, FpathCase + FuuidCase  + '-location.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_LOCATION":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbLocation.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbLocation.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbLocation.Items[idx]);
    line := '        ]\}';
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end;

  formLocation.Close;
end;

procedure TformLocation.btnAddLocationClick(Sender: TObject);
begin
  if (Trim(edlocality.Text) = '') or (Trim(edRegion.Text) = '')  then
    ShowMessage('Locality or Region name are missing!')
  else
  begin
    lbLocation.Items.Add(prepareObjectCaseLine());
    clearFormFields();
  end;
end;

procedure TformLocation.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformLocation.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformLocation.ShowWithParamater(pathCase:String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine, dir:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-identity.json
  if FileExists(FpathCase + FuuidCase  + '-location.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase  + '-location.json');
    Reset(fileJSON);
    lbLocation.Items.Clear;
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

        lbLocation.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formLocation.ShowModal;
end;

end.
