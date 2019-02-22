unit caseGenerator_identity;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, caseGenerator_util;

type
  TformIdentity = class(TForm)
    Label1: TLabel;
    lbIdentity: TListBox;
    edName: TEdit;
    edFamilyName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    timeBirthTime: TTimeEdit;
    btnClose: TButton;
    btnAddIdentity: TButton;
    btnDeleteIdentity: TButton;
    sbIdentity: TStatusBar;
    cbDay: TComboBox;
    cbMonth: TComboBox;
    cbYear: TComboBox;
    btnModifyIdentity: TButton;
    btnCancel: TButton;
    procedure btnMenoClick(Sender: TObject);
    procedure btnPiuClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnAddIdentityClick(Sender: TObject);
    procedure btnDeleteIdentityClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lbIdentityChange(Sender: TObject);
    procedure btnModifyIdentityClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    function prepareObjectCaseLine(operation: String): String;
    //function ExtractField(line, subLine: String): String;
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase:String; uuidCase: String);
    { Public declarations }
  end;

var
  formIdentity: TformIdentity;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformIdentity.btnDeleteIdentityClick(Sender: TObject);
begin
  lbIdentity.Items.Delete(lbIdentity.ItemIndex);
end;

procedure TformIdentity.btnMenoClick(Sender: TObject);
begin
  lbIdentity.Items.Delete(lbIdentity.ItemIndex);
end;

procedure TformIdentity.btnPiuClick(Sender: TObject);
var
  line: string;
begin
  if (Trim(edName.Text) = '') or (Trim(edFamilyName.Text) = '')  then
    ShowMessage('Name or Family name are missing!')
  else
  begin
    line := '{"name":"' + edName.Text + '", "familyName":"' + edFamilyName.Text + '", "BirthDate":"';
    line := line +  cbDay.Items[cbDay.ItemIndex] + cbMonth.Items[cbMonth.ItemIndex] + cbYear.Items[cbYear.ItemIndex] + '", "BirthTime":"' + TimeToStr(timeBirthTime.Time) + '"}';
    lbIdentity.Items.Add(line);
  end;
end;

{function TformIdentity.ExtractField(line, subLine: String): String;
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

procedure TformIdentity.btnModifyIdentityClick(Sender: TObject);
begin
  if lbIdentity.ItemIndex > - 1 then
    lbIdentity.Items[lbIdentity.ItemIndex] := prepareObjectCaseLine('modify');
end;

procedure TformIdentity.btnCancelClick(Sender: TObject);
begin
  formIdentity.Close;
end;

procedure TformIdentity.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line, dir:string;
  idx: integer;
begin
  if lbIdentity.Items.Count > 0 then
  begin
    //dir := GetCurrentDir;
    idx := 0;
    AssignFile(fileJSON, FPathCase + FuuidCase + '-identity.json');
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := '"OBJECTS_IDENTITY":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbIdentity.Items.Count - 2 do
      WriteLn(fileJSON, lbIdentity.Items[idx] + ',');

    WriteLn(fileJSON, lbIdentity.Items[idx]);
    WriteLn(fileJSON, ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-identity.json');

  formIdentity.Close;
end;

procedure TformIdentity.btnAddIdentityClick(Sender: TObject);
begin
  if (Trim(edName.Text) = '') or (Trim(edFamilyName.Text) = '')  then
    ShowMessage('Name or Family name are missing!')
  else
  begin
    lbIdentity.Items.Add(prepareObjectCaseLine('add')); // prepareObjectCaseLine returns the Object CASE
    edName.Text := '';
    edFamilyName.Text := '';
    cbDay.ItemIndex:= -1;
    cbMonth.ItemIndex := -1;
    cbYear.ItemIndex := -1;
    timeBirthTime.Text := '';
  end;
end;

procedure TformIdentity.FormShow(Sender: TObject);
var
  idxStart, idxEnd, idx: integer;
begin
  idxStart := 1930;
  idxEnd := 2030;
  cbYear.Items.Clear;
  for idx:= idxStart to idxEnd do
    cbYear.Items.Add(IntToStr(idx));
  edName.Text := '';
  edFamilyName.Text := '';
end;

procedure TformIdentity.lbIdentityChange(Sender: TObject);
var
  line, birthDate, sDate, sDay, sMonth, sYear: string;
  idx: Integer;
begin

  if lbIdentity.ItemIndex > - 1 then
  begin
    line := lbIdentity.Items[lbIdentity.ItemIndex];
    edName.Text := ExtractField(line, '"givenName":"');
    edFamilyName.Text := ExtractField(line, '"familyName":"');
    birthDate := ExtractField(line, '"birthDate":"');
    sDate := Copy(birthDate, 1, 10);
    sDay := Copy(sDate, 9, 2);
    for idx:=0 to cbDay.Items.Count - 1 do
    begin
      if cbDay.Items[idx] = sDay then
      begin
        cbDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 6, 2);
    for idx:=0 to cbMonth.Items.Count - 1 do
    begin
      if cbMonth.Items[idx] = sMonth then
      begin
        cbMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbYear.Items.Count - 1 do
    begin
      if cbYear.Items[idx] = sYear then
      begin
        cbYear.ItemIndex := idx;
        break;
      end;
    end;

    timeBirthTime.Text := Copy(birthDate, 12, 8);
  end;
end;

function TformIdentity.prepareObjectCaseLine(operation: String): String;
var
 line, recSep, indent: string;
  Uid: TGUID;
begin
  recSep := #30 + #30;
  indent := '   ';

  line := '{' + recSep;
  if operation = 'add' then
  begin
    CreateGUID(Uid);
    line := line + indent + '"@id":"' + GuidToString(Uid) + '",' + recSep;
  end
  else
    line := line + indent + '"@id":"' + ExtractField(lbIdentity.Items[lbIdentity.ItemIndex], '"@id":"') + '",' + recSep;


  line := line + indent + '"@type":"Identity",' + recSep;
  line := line + indent + '"propertyBundle":[' + recSep;
  line := line + indent + '{' + recSep;
  line := line + RepeatString(indent, 2) + '"@type":"SimpleName",' + recSep;
  line := line + RepeatString(indent, 2) + '"givenName":"' + edName.Text + '",' + recSep;
  line := line + RepeatString(indent, 2) + '"familyName":"' + edFamilyName.Text + '"' + recSep;
  line := line + indent + '},' + recSep;
  line := line + indent + '{' + recSep;
  line := line +  RepeatString(indent, 2) + '"@type":"BirthInformation",' + recSep;
  line := line +  RepeatString(indent, 2) + '"birthDate":"';
  line := line +  cbYear.Items[cbYear.ItemIndex] + '-' + cbMonth.Items[cbMonth.ItemIndex] + '-' + cbDay.Items[cbDay.ItemIndex];
  line := line + 'T' + TimeToStr(timeBirthTime.Time) + 'Z"' + recSep;
  line := line +  indent + '}' + recSep + indent + ']' + recSep + '}';
  Result := line;
end;

procedure TformIdentity.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformIdentity.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformIdentity.ShowWithParamater(pathCase:String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine, dir:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  lbIdentity.Items.Clear;
  // read file JSON uuidCase-identity.json
  if FileExists(FPathCase + FUuidCase + '-identity.json') then
  begin
    AssignFile(fileJSON, FPathCase + FUuidCase + '-identity.json');
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

        lbIdentity.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formIdentity.ShowModal;
end;

procedure TformIdentity.SpeedButton1Click(Sender: TObject);
begin
  lbIdentity.Items.Delete(lbIdentity.ItemIndex);
end;

end.
