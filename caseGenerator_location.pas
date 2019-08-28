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
    function prepareObjectCaseLine(operation: String): String;
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
  if lbLocation.ItemIndex > - 1 then
  begin
    lbLocation.Items[lbLocation.ItemIndex] := prepareObjectCaseLine('modify');
    clearFormFields();
  end;
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
  if  lbLocation.ItemIndex > - 1 then
  begin
    line := lbLocation.Items[lbLocation.ItemIndex];
    edRegion.Text := ExtractField(line, '"region":"');
    edLocality.Text := ExtractField(line, '"locality":"');
    edPostalCode.Text := ExtractField(line, '"postalCode":"');
    edStreet.Text := ExtractField(line, '"street":"');
    edLatitude.Text := ExtractField(line, '"latitude":"');
    edLongitude.Text := ExtractField(line,'"longitude":"');
  end;
end;

function TformLocation.prepareObjectCaseLine(operation: String): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30; // record separator, not printable
  indent := '   ';

  line := '{' + recSep;

  if operation = 'add' then
  begin
    CreateGUID(Uid);
    guidNoBraces := Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
  end
  else
     guidNoBraces :=  ExtractField(lbLocation.Items[lbLocation.ItemIndex], '"@id":"');

  line := line + indent + '"@id":"' + guidNoBraces + '", ' + recSep;
  line := line + indent + '"@type":"Location", ' + recSep;
  line := line + indent +  '"propertyBundle":[' + recSep;
  line := line + indent + '{' + recSep;
  line := line + RepeatString(indent, 2) + '"@type":"' + guidNoBraces + '-SimpleAddress", ' + recSep;
  line := line + RepeatString(indent, 2) + '"locality":"' + edLocality.Text + '", ' + recSep;
  line := line + RepeatString(indent, 2) + '"region":"' + edRegion.Text + '", ' + recSep;
  line := line + RepeatString(indent, 2) + '"postalCode":"' + edPostalCode.Text + '", ' + recSep;
  line := line + RepeatString(indent, 2) + '"street":"' + edStreet.Text + '"' + recSep;
  line := line + indent + '}';
  if (Trim(edLatitude.Text) ='') or (Trim(edLongitude.Text) = '') then
    line := line + recSep
  else
  begin
    line := line + ', ' + recSep;
    line := line + indent + '{' + recSep + RepeatString(indent, 2) + '"@type":"' + guidNoBraces + '-LatLongCoordinates", ' + recSep;
    line := line + RepeatString(indent, 2) + '"latitude":"' + edLatitude.Text + '", ' + recSep;
    line := line + RepeatString(indent, 2) + '"longitude":"' + edLongitude.Text + '" ' + recSep;
    line := line + indent + '}' + recSep;
    line := line + indent  + ']' + recSep + '}';
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
    AssignFile(fileJSON, FpathCase + FuuidCase  + '-location.json', CP_UTF8);

    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_LOCATION":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbLocation.Items.Count - 2 do
      WriteLn(fileJSON, UTF8Encode(#9#9 + lbLocation.Items[idx] + ','));

    WriteLn(fileJSON, UTF8Encode(#9#9 + lbLocation.Items[idx]));
    line := '        ]\}';
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-location.json');

  formLocation.Close;
end;

procedure TformLocation.btnAddLocationClick(Sender: TObject);
begin
  if (Trim(edlocality.Text) = '') or (Trim(edRegion.Text) = '')  then
    ShowMessage('Locality or Region name are missing!')
  else
  begin
    lbLocation.Items.Add(prepareObjectCaseLine('add'));
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
